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

library(shiny)
library(ggplot2)
library(tools)

# 灰色区间辅助函数，支持跨午夜
make_shade_df <- function(
  date_seq,
  light_begin,
  dark_begin,
  start_time,
  end_time
) {
  if (light_begin == dark_begin) return(data.frame())
  if (light_begin < dark_begin) {
    df <- data.frame(
      xmin = as.POSIXct(paste(date_seq, light_begin)),
      xmax = as.POSIXct(paste(date_seq, dark_begin))
    )
  } else {
    df <- rbind(
      data.frame(
        xmin = as.POSIXct(paste(date_seq, light_begin)),
        xmax = as.POSIXct(paste(date_seq, "23:59:59"))
      ),
      data.frame(
        xmin = as.POSIXct(paste(date_seq + 1, "00:00:00")),
        xmax = as.POSIXct(paste(date_seq + 1, dark_begin))
      )
    )
  }
  df <- df[df$xmin < end_time & df$xmax > start_time, ]
  df$xmin[df$xmin < start_time] <- start_time
  df$xmax[df$xmax > end_time] <- end_time
  df
}

ui <- fluidPage(
  titlePanel("温度动态折线图"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csvfile", "选择CSV文件", accept = ".csv"),
      textInput("save_filename", "保存新文件名（如：newdata.csv）", value = ""),
      actionButton("save_btn", "保存格式化文件"),
      textInput("save_plot_dir", "PNG图片保存路径", value = ""),
      textInput(
        "save_plot_filename",
        "保存图片文件名（如：plot.png）",
        value = ""
      ),
      actionButton("save_plot_btn", "保存当前图片为PNG"),
      textInput("plot_title", "图片标题", value = "Temperature Over Time"),
      textInput("light_begin", "开灯时间 (格式HH:MM:SS)", value = "08:00:00"),
      textInput("dark_begin", "关灯时间 (格式HH:MM:SS)", value = "20:00:00"),
      uiOutput("start_time_ui"),
      uiOutput("end_time_ui"),
      uiOutput("xrange_ui"),
      uiOutput("yrange_ui"),
      textOutput("save_msg"),
      textOutput("save_plot_msg")
    ),
    mainPanel(
      plotOutput("lineplot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$csvfile, {
    req(input$csvfile)
    default_name <- sub(
      "\\.csv$",
      "_New.csv",
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

  # 手动起止时间输入框
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
    new_filename <- input$save_filename
    if (!nzchar(new_filename)) {
      new_filename <- sub(
        "\\.csv$",
        "_New.csv",
        input$csvfile$name,
        ignore.case = TRUE
      )
    }
    if (!grepl("\\.csv$", new_filename, ignore.case = TRUE)) {
      new_filename <- paste0(new_filename, ".csv")
    }
    tryCatch(
      {
        write.csv(
          df[, tb_cols, drop = FALSE],
          file = new_filename,
          row.names = FALSE
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

  output$lineplot <- renderPlot({
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
      (if (nrow(shade_df) > 0)
        geom_rect(
          data = shade_df,
          inherit.aes = FALSE,
          aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          fill = "grey50",
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
