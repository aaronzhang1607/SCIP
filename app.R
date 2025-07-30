# 声明全局变量，避免R CMD检查时出现警告
# 这些变量主要在ggplot2的aes()函数中使用
utils::globalVariables(c(
  "RealTime",
  "xmin",
  "xmax",
  "Temperature",
  "start_time",
  "end_time",
  "xrange",
  "yrange"
))

# 加载必要的R包
library(shiny)     # 用于创建交互式Web应用
library(ggplot2)   # 用于数据可视化
library(plotly)    # 用于创建交互式图表
library(tools)     # 提供文件路径操作等工具函数

# 设置Shiny应用在默认浏览器中打开
options(shiny.launch.browser = TRUE)

# 创建灰色背景区域的辅助函数，用于在图表中显示暗周期（关灯时段）
# 参数:
#   date_seq: 日期序列
#   light_begin: 开灯时间(格式: "HH:MM:SS")
#   dark_begin: 关灯时间(格式: "HH:MM:SS")
#   start_time: 图表的开始时间
#   end_time: 图表的结束时间
# 返回值: 包含xmin和xmax的数据框，用于绘制灰色背景（暗周期）
make_shade_df <- function(
  date_seq,
  light_begin,
  dark_begin,
  start_time,
  end_time
) {
  if (light_begin == dark_begin) return(data.frame())
  
  # 如果关灯时间在开灯时间之后（同一天内）
  if (dark_begin > light_begin) {
    # 创建暗周期区域（从关灯时间到次日开灯时间）
    df <- rbind(
      data.frame(
        xmin = as.POSIXct(paste(date_seq, dark_begin)),
        xmax = as.POSIXct(paste(date_seq + 1, light_begin))
      )
    )
  } else {
    # 如果关灯时间在开灯时间之前（跨天）
    df <- rbind(
      data.frame(
        xmin = as.POSIXct(paste(date_seq, dark_begin)),
        xmax = as.POSIXct(paste(date_seq, light_begin))
      )
    )
  }
  df <- df[df$xmin < end_time & df$xmax > start_time, ]
  df$xmin[df$xmin < start_time] <- start_time
  df$xmax[df$xmax > end_time] <- end_time
  df
}

# 定义用户界面
ui <- fluidPage(
  # 应用标题
  titlePanel("温度动态折线图"),
  
  # 使用fluidRow和column创建三列布局，调整列宽比例
  fluidRow(
    # 第一列：数据操作（变窄）
    column(
      width = 2,  # 从3减少到2
      wellPanel(
        h4("数据操作"),
        # 文件上传控件
        fileInput("csvfile", "选择CSV文件", accept = ".csv"),
        textInput("save_filename", "保存新文件名", value = "", 
                 placeholder = "例如: newdata.csv"),
        actionButton("save_btn", "保存格式化文件", class = "btn-primary"),
        br(), br(),
        textInput("save_plot_dir", "PNG保存路径", value = "", 
                 placeholder = "例如: C:/plots"),
        textInput("save_plot_filename", "图片文件名", value = "", 
                 placeholder = "例如: plot.png"),
        actionButton("save_plot_btn", "保存当前图片", class = "btn-primary"),
        textOutput("save_msg"),
        textOutput("save_plot_msg"),
        
        # 添加分隔线
        hr(),
        
        # 读取已处理CSV文件区域
        h4("读取已处理数据"),
        fileInput("processed_csv", "选择已处理的CSV文件", 
                 accept = ".csv",
                 buttonLabel = "浏览..."),
        actionButton("load_processed_btn", "加载数据", 
                    class = "btn-primary"),
        checkboxInput("show_processed", "显示处理数据", value = FALSE),
        br(), br(),
        h5("数据预览"),
        div(style = 'overflow-x: auto; font-size: 80%;',
            tags$style(HTML(
              "#processed_data_preview table {
                width: 100%;
                white-space: nowrap;
                font-size: 10px;
              }
              #processed_data_preview th, #processed_data_preview td {
                padding: 2px 4px;
                white-space: nowrap;
                overflow: hidden;
                text-overflow: ellipsis;
                max-width: 100px;
              }"
            )),
            tableOutput("processed_data_preview")
        )
      )
    ),
    
    # 第二列：参数设置（保持窄）
    column(
      width = 2,  # 保持2单位宽度
      wellPanel(
        h4("光照设置"),
        textInput("light_begin", "开灯", value = "08:00:00", 
                 placeholder = "HH:MM:SS", width = "100%"),
        textInput("dark_begin", "关灯", value = "20:00:00", 
                 placeholder = "HH:MM:SS", width = "100%"),
        hr(),
        h4("时间范围"),
        uiOutput("start_time_ui"),
        uiOutput("end_time_ui"),
        hr(),
        h4("图表设置"),
        textInput("plot_title", "标题", value = "Temperature Over Time", 
                 width = "100%"),
        uiOutput("xrange_ui"),
        uiOutput("yrange_ui")
      )
    ),
    
    # 第三列：图表显示（进一步变宽）
    column(
      width = 8,  # 从7增加到8
      wellPanel(
        style = "padding: 15px;",
        plotlyOutput("lineplot", height = "750px")  # 使用plotlyOutput替代plotOutput
      )
    )
  )
)

# 服务器端逻辑
server <- function(input, output, session) {
  # 当CSV文件被上传时执行
  observeEvent(input$csvfile, {
    req(input$csvfile)
    default_name <- sub(
      "\\.csv$",
      "_Transformed.csv",
      input$csvfile$name,
      ignore.case = TRUE
    )
    updateTextInput(session, "save_filename", value = default_name)
    default_plot_name <- sub(
      "\\.csv$",
      "_plot.png",
      input$csvfile$name,
      ignore.case = TRUE
    )
    updateTextInput(session, "save_plot_filename", value = default_plot_name)
    # 自动填充起止时间
    df <- read.csv(input$csvfile$datapath)
    if (!"RealTime" %in% names(df) && all(c("Date", "Time") %in% names(df))) {
      df$Date <- gsub(":", "/", df$Date)
      df$RealTime <- paste(df$Date, df$Time)
    }
    df$RealTime <- sub("(\\d{2}:\\d{2}:\\d{2})\\..*", "\\1", df$RealTime)
    df$RealTime <- as.POSIXct(df$RealTime, format = "%Y/%m/%d %H:%M")
    min_time <- min(df$RealTime, na.rm = TRUE)
    max_time <- max(df$RealTime, na.rm = TRUE)
    updateTextInput(
      session,
      "start_time",
      value = format(min_time, "%Y-%m-%d %H:%M")
    )
    updateTextInput(
      session,
      "end_time",
      value = format(max_time, "%Y-%m-%d %H:%M")
    )
    # 自动填充图片标题
    default_title <- "Temperature Over Time"
    updateTextInput(session, "plot_title", value = default_title)
  })

  # 读取并预处理原始数据的reactive表达式
  # 返回包含RealTime列的数据框
  data_raw <- reactive({
    req(input$csvfile)
    df <- read.csv(input$csvfile$datapath)
    if (!"RealTime" %in% names(df) && all(c("Date", "Time") %in% names(df))) {
      df$Date <- gsub(":", "/", df$Date)
      df$RealTime <- paste(df$Date, df$Time)
    }
    df$RealTime <- sub("(\\d{2}:\\d{2}:\\d{2})\\..*", "\\1", df$RealTime)
    df$RealTime <- as.POSIXct(df$RealTime, format = "%Y/%m/%d %H:%M")
    df
  })

  # 动态生成开始时间输入框
  # 根据数据自动设置默认值为数据中的最小时间
  output$start_time_ui <- renderUI({
    df <- data_raw()
    min_time <- min(df$RealTime, na.rm = TRUE)
    textInput(
      "start_time",
      "起始时间(YYYY-MM-DD HH:MM)",
      value = format(min_time, "%Y-%m-%d %H:%M")
    )
  })
  output$end_time_ui <- renderUI({
    df <- data_raw()
    max_time <- max(df$RealTime, na.rm = TRUE)
    textInput(
      "end_time",
      "终止时间(YYYY-MM-DD HH:MM)",
      value = format(max_time, "%Y-%m-%d %H:%M")
    )
  })

  # 根据用户选择的时间范围过滤数据
  # 自动处理开始时间大于结束时间的情况
  filtered_data <- reactive({
    df <- data_raw()
    req(input$start_time, input$end_time)
    st <- as.POSIXct(input$start_time, format = "%Y-%m-%d %H:%M")
    et <- as.POSIXct(input$end_time, format = "%Y-%m-%d %H:%M")
    # 自动纠正顺序
    if (!is.na(st) && !is.na(et) && st > et) {
      tmp <- st
      st <- et
      et <- tmp
    }
    df <- df[df$RealTime >= st & df$RealTime <= et, ]
    df
  })

  # 加载并显示已处理的CSV数据
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$load_processed_btn, {
    req(input$processed_csv)
    tryCatch({
      # 读取CSV文件
      df <- read.csv(input$processed_csv$datapath, 
                    stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8")
      
      # 打印数据结构用于调试
      print("处理后的数据结构:")
      print(str(df))
      print("前几行数据:")
      print(head(df))
      print(paste0("RealTime 类型: ", class(df$RealTime)))
      print(paste0("Temperature 类型: ", class(df$Temperature)))
      print(paste0("Injection 类型: ", class(df$Injection)))
      print(paste0("Injection 值: ", paste(unique(df$Injection), collapse = ", ")))
      # 确保包含必要的列
      if (!"Injection" %in% names(df)) {
        showNotification("错误: 文件必须包含'Injection'列", 
                        type = "error")
        return()
      }
      processed_data(df)
      showNotification("数据加载成功", type = "message")
    }, error = function(e) {
      showNotification(paste("加载文件时出错:", e$message), 
                      type = "error")
    })
  })
  
  # 显示处理后的数据预览
  output$processed_data_preview <- renderTable({
    df <- processed_data()
    req(df)
    head(df, 3)  # 只显示前3行作为预览
  }, rownames = FALSE, width = "100%")
  
  # 获取当前要显示的数据（原始或处理后的）
  current_data <- reactive({
    if (isTRUE(input$show_processed) && !is.null(processed_data())) {
      df <- processed_data()
      # 确保包含必要的列
      if (!all(c("RealTime", "Temperature") %in% names(df))) {
        showNotification("错误: 处理后的数据必须包含'RealTime'和'Temperature'列", 
                        type = "error")
        return(NULL)
      }
      # 确保RealTime是POSIXct类型
      if (!inherits(df$RealTime, "POSIXct")) {
        # 尝试不同的时间格式
        df$RealTime <- tryCatch({
          as.POSIXct(df$RealTime, format = "%Y/%m/%d %H:%M")
        }, error = function(e) {
          as.POSIXct(df$RealTime)
        })
      }
      # 确保Temperature是数值类型
      if (!is.numeric(df$Temperature)) {
        df$Temperature <- as.numeric(as.character(df$Temperature))
      }
      # 确保Injection是逻辑类型
      if ("Injection" %in% names(df)) {
        if (is.character(df$Injection)) {
          df$Injection <- toupper(trimws(df$Injection)) == "TRUE"
        } else if (is.factor(df$Injection)) {
          df$Injection <- as.logical(as.character(df$Injection))
        }
      }
      df
    } else {
      filtered_data()
    }
  })
  
  # 获取Injection为TRUE的时间点
  injection_times <- reactive({
    if (!isTRUE(input$show_processed)) return(NULL)
    df <- processed_data()
    req(df, "Injection" %in% names(df))
    df$RealTime[df$Injection == TRUE & !is.na(df$Injection)]
  })

  # 保存处理后的数据到CSV文件
  observeEvent(input$save_btn, {
    req(input$csvfile)
    df <- filtered_data()
    tb_cols <- intersect(c("RealTime", "Temperature"), names(df))
    if (nrow(df) == 0 || length(tb_cols) == 0) {
      output$save_msg <- renderText({
        "无可保存数据，请检查时间区间或数据列名。"
      })
      return()
    }
    
    # 添加Injection列并设置为FALSE
    if ("Injection" %in% names(df)) {
      df$Injection <- FALSE
    } else {
      df$Injection <- FALSE
      tb_cols <- c(tb_cols, "Injection")  # 确保保存时包含Injection列
    }
    new_filename <- input$save_filename
    if (!nzchar(new_filename)) {
      new_filename <- sub(
        "\\.csv$",
        "_Transformed.csv",
        input$csvfile$name,
        ignore.case = TRUE
      )
    }
    if (!grepl("\\.csv$", new_filename, ignore.case = TRUE)) {
      new_filename <- paste0(new_filename, ".csv")
    }
    tryCatch(
      {
        # 确保只保存需要的列，并保持列顺序
        write.csv(
          df[, tb_cols, drop = FALSE],
          file = new_filename,
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )
        output$save_msg <- renderText({
          paste0("保存成功：", new_filename)
        })
      },
      error = function(e) {
        output$save_msg <- renderText({
          paste0("保存失败：", e$message)
        })
      }
    )
  })

  # 保存当前图表为PNG图片
  observeEvent(input$save_plot_btn, {
    req(input$csvfile)
    plot_filename <- input$save_plot_filename
    plot_dir <- input$save_plot_dir
    if (!nzchar(plot_filename)) {
      output$save_plot_msg <- renderText({
        "请填写图片文件名！"
      })
      return()
    }
    if (!nzchar(plot_dir)) {
      output$save_plot_msg <- renderText({
        "请填写图片保存路径！"
      })
      return()
    }
    
    # 创建目录（如果不存在）
    if (!dir.exists(plot_dir)) {
      dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    full_plot_path <- file.path(plot_dir, plot_filename)
    
    tryCatch(
      {
        # 使用plotly的导出功能
        plotly_IMAGE(
          x = output$lineplot(),
          width = 1200,
          height = 800,
          format = "png",
          out_file = full_plot_path
        )
        
        output$save_plot_msg <- renderText({
          paste0("图片已保存：", full_plot_path)
        })
      },
      error = function(e) {
        output$save_plot_msg <- renderText({
          paste0("图片保存失败：", e$message)
        })
      }
    )
  })

  output$xrange_ui <- renderUI({
    df <- filtered_data()
    rng <- range(df$RealTime, na.rm = TRUE)
    sliderInput(
      "xrange",
      "选择X轴范围(时间)",
      min = rng[1],
      max = rng[2],
      value = rng,
      timeFormat = "%Y-%m-%d %H:%M"
    )
  })
  output$yrange_ui <- renderUI({
    df <- filtered_data()
    rng <- range(df$Temperature, na.rm = TRUE)
    min_y <- min(25, floor(rng[1]))
    max_y <- max(40, ceiling(rng[2]))
    sliderInput(
      "yrange",
      "选择Y轴范围(温度)",
      min = min_y,
      max = max_y,
      value = c(min_y, max_y)
    )
  })

  # 渲染交互式温度折线图
  output$lineplot <- renderPlotly({
    # 设置日期时间格式为英文，避免中文系统下的本地化问题
    Sys.setlocale("LC_TIME", "C")
    
    # 获取当前数据
    df <- current_data()
    req(df, nrow(df) > 0, input$xrange, input$yrange)
    
    # 确保RealTime是POSIXct类型
    if (!inherits(df$RealTime, "POSIXct")) {
      df$RealTime <- as.POSIXct(df$RealTime)
    }
    
    # 获取光照设置
    light_begin <- ifelse(
      is.null(input$light_begin) || !nzchar(input$light_begin),
      "08:00:00",
      input$light_begin
    )
    dark_begin <- ifelse(
      is.null(input$dark_begin) || !nzchar(input$dark_begin),
      "20:00:00",
      input$dark_begin
    )
    
    # 设置时间范围
    start_time <- min(df$RealTime, na.rm = TRUE)
    end_time <- max(df$RealTime, na.rm = TRUE)
    
    # 生成暗周期阴影区域
    date_seq <- seq(as.Date(start_time), as.Date(end_time), by = "day")
    shade_df <- make_shade_df(
      date_seq,
      light_begin,
      dark_begin,
      start_time,
      end_time
    )
    
    # 创建基础图表
    p <- plot_ly()
    
    # 准备shapes列表用于添加暗周期背景和注射标记
    shapes <- list()
    
    # 添加暗周期背景
    if (nrow(shade_df) > 0) {
      for (i in seq_len(nrow(shade_df))) {
        shapes[[length(shapes) + 1]] <- list(
          type = "rect",
          x0 = shade_df$xmin[i],
          x1 = shade_df$xmax[i],
          y0 = input$yrange[1],
          y1 = input$yrange[2],
          fillcolor = "rgba(128, 128, 128, 0.2)",
          line = list(width = 0),
          layer = "below"
        )
      }
    }
    
    # 如果是处理后的数据且包含Injection列，添加注射标记
    if (isTRUE(input$show_processed) && "Injection" %in% names(df)) {
      injection_times <- df$RealTime[df$Injection == TRUE & !is.na(df$Injection)]
      if (length(injection_times) > 0) {
        for (time in injection_times) {
          shapes[[length(shapes) + 1]] <- list(
            type = "line",
            x0 = time,
            x1 = time,
            y0 = input$yrange[1],
            y1 = input$yrange[2],
            line = list(
              color = "red",
              width = 1,
              dash = "dash"
            )
          )
        }
      }
    }
    
    # 添加温度曲线
    p <- p %>% add_trace(
      data = df,
      x = ~RealTime,
      y = ~Temperature,
      type = "scatter",
      mode = "lines",
      name = "Temperature",
      line = list(color = "#377eb8", width = 1),
      hoverinfo = "text",
      text = ~paste(
        "时间: ", format(RealTime, "%Y-%m-%d %H:%M"),
        "<br>温度: ", round(Temperature, 2), "°C"
      )
    )
    
    # 设置图表布局
    p <- p %>% layout(
      title = list(
        text = input$plot_title,
        font = list(family = "Arial", size = 20, weight = "bold")
      ),
      xaxis = list(
        title = "RealTime",
        type = "date",
        tickformat = "%b %d %H:%M",
        tickangle = -60,
        range = as.POSIXct(input$xrange, origin = "1970-01-01"),
        showgrid = FALSE,
        showline = TRUE,
        linewidth = 1,
        mirror = TRUE
      ),
      yaxis = list(
        title = "Temperature(°C)",
        range = input$yrange,
        dtick = 3,
        showgrid = FALSE,
        showline = TRUE,
        linewidth = 1,
        mirror = TRUE
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center",
        yanchor = "top"
      ),
      margin = list(l = 60, r = 20, t = 60, b = 100),
      hovermode = "closest",
      showlegend = TRUE,
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      # 使用shapes参数添加暗周期背景和注射标记
      shapes = shapes
    )
    
    # 添加交互功能
    p <- p %>% config(
      displayModeBar = TRUE,
      scrollZoom = TRUE,
      modeBarButtonsToAdd = list(
        "select2d",
        "lasso2d",
        "zoomIn",
        "zoomOut",
        "autoScale",
        "resetScale",
        "toImage"
      ),
      toImageButtonOptions = list(
        format = "png",
        filename = "temperature_plot",
        width = 1200,
        height = 800
      )
    )
    
    p
  })
}

# 运行Shiny应用
shinyApp(ui, server)
