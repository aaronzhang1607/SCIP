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
        plotOutput("lineplot", height = "750px")  # 增加图表高度
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
      df <- read.csv(input$processed_csv$datapath, 
                    stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8")
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
    df <- filtered_data()
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
    full_plot_path <- file.path(plot_dir, plot_filename)
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
    start_time <- min(df$RealTime, na.rm = TRUE)
    end_time <- max(df$RealTime, na.rm = TRUE)
    date_seq <- seq(as.Date(start_time), as.Date(end_time), by = "day")
    shade_df <- make_shade_df(
      date_seq,
      light_begin,
      dark_begin,
      start_time,
      end_time
    )
    breaks_vec <- if (nrow(shade_df) > 0)
      sort(unique(c(shade_df$xmin, shade_df$xmax))) else NULL
    tryCatch(
      {
        Sys.setlocale("LC_TIME", "C")
        png(full_plot_path, width = 300, height = 300, units = "mm", res = 300)
        p <- ggplot(df, aes(x = RealTime)) +
          (if (nrow(shade_df) > 0)
            geom_rect(
              data = shade_df,
              inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "grey50",
              alpha = 0.5
            ) else NULL) +
          geom_line(
            aes(y = Temperature, color = "Temperature"),
            linewidth = 0.5
          ) +
          scale_y_continuous(
            name = "Temperature(°C)",
            expand = c(0, 0),
            limits = range(df$Temperature, na.rm = TRUE),
            breaks = seq(
              min(df$Temperature, na.rm = TRUE),
              max(df$Temperature, na.rm = TRUE),
              by = 3
            )
          ) +
          scale_x_datetime(
            breaks = breaks_vec,
            date_labels = "%b %d %H:%M",
            limits = range(df$RealTime, na.rm = TRUE)
          ) +
          scale_color_manual(values = c("Temperature" = "#377eb8"), name = "") +
          theme_bw(base_family = "Arial") +
          theme(
            plot.title = element_text(
              family = "Arial",
              face = "bold",
              size = 16,
              hjust = 0.5
            ),
            plot.subtitle = element_text(
              family = "Arial",
              face = "plain",
              size = 14,
              hjust = 0.5
            ),
            axis.title = element_text(
              family = "Arial",
              face = "plain",
              size = 13
            ),
            axis.text = element_text(
              family = "Arial",
              face = "plain",
              size = 12
            ),
            axis.text.x = element_text(angle = 60, hjust = 1),
            axis.line = element_line(linewidth = 0.5, colour = "black"),
            legend.title = element_text(
              family = "Arial",
              face = "plain",
              size = 12
            ),
            legend.text = element_text(
              family = "Arial",
              face = "plain",
              size = 12
            ),
            legend.position = "bottom",
            panel.grid = element_blank()
          ) +
          labs(x = "RealTime", title = input$plot_title)
        print(p)
        dev.off()
        output$save_plot_msg <- renderText({
          paste0("图片已保存：", full_plot_path)
        })
      },
      error = function(e) {
        dev.off()
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

  # 渲染温度折线图
  output$lineplot <- renderPlot({
    # 设置日期时间格式为英文，避免中文系统下的本地化问题
    Sys.setlocale("LC_TIME", "C")
    Sys.setlocale("LC_TIME", "C")
    df <- filtered_data()
    req(input$xrange, input$yrange)
    if (nrow(df) == 0) return(NULL)
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
    start_time <- min(df$RealTime, na.rm = TRUE)
    end_time <- max(df$RealTime, na.rm = TRUE)
    date_seq <- seq(as.Date(start_time), as.Date(end_time), by = "day")
    shade_df <- make_shade_df(
      date_seq,
      light_begin,
      dark_begin,
      start_time,
      end_time
    )
    breaks_vec <- if (nrow(shade_df) > 0)
      sort(unique(c(shade_df$xmin, shade_df$xmax))) else NULL
    ggplot(df, aes(x = RealTime)) +
      # 添加灰色背景表示暗周期
      (if (nrow(shade_df) > 0)
        geom_rect(
          data = shade_df,
          inherit.aes = FALSE,
          aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          fill = "grey80",
          alpha = 0.5
        ) else NULL) +
      geom_line(aes(y = Temperature, color = "Temperature"), linewidth = 0.5) +
      scale_y_continuous(
        name = "Temperature(°C)",
        expand = c(0, 0),
        limits = input$yrange,
        breaks = seq(input$yrange[1], input$yrange[2], by = 3)
      ) +
      scale_x_datetime(
        breaks = breaks_vec,
        date_labels = "%b %d %H:%M",
        limits = as.POSIXct(input$xrange, origin = "1970-01-01")
      ) +
      scale_color_manual(values = c("Temperature" = "#377eb8"), name = "") +
      theme_bw(base_family = "Arial") +
      theme(
        plot.title = element_text(
          family = "Arial",
          face = "bold",
          size = 16,
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          family = "Arial",
          face = "plain",
          size = 14,
          hjust = 0.5
        ),
        axis.title = element_text(family = "Arial", face = "plain", size = 13),
        axis.text = element_text(family = "Arial", face = "plain", size = 12),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        legend.title = element_text(
          family = "Arial",
          face = "plain",
          size = 12
        ),
        legend.text = element_text(family = "Arial", face = "plain", size = 12),
        legend.position = "bottom",
        panel.grid = element_blank()
      ) +
      labs(x = "RealTime", title = input$plot_title)
  })
}

shinyApp(ui, server)
