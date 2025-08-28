# 服务器逻辑装配
library(shiny)
library(plotly)

# 使用空合并运算符（在任何使用前定义）
`%||%` <- function(a, b) if (!is.null(a)) a else b

# 定义全局函数，确保可以被其他模块访问
interpolate_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  out <- df
  # 确保 RealTime 为数值时间用于插值
  xt <- as.numeric(as.POSIXct(out$RealTime))
  # Temperature
  if ("Temperature" %in% names(out)) {
    y <- out$Temperature
    keep <- !is.na(y) & !is.na(xt)
    if (sum(keep) >= 2) {
      y_int <- approx(x = xt[keep], y = y[keep], xout = xt, method = "linear", rule = 1, ties = "ordered")$y
      out$Temperature <- y_int
    }
  }
  # Locomotion
  if ("Locomotion" %in% names(out)) {
    y <- out$Locomotion
    keep <- !is.na(y) & !is.na(xt)
    if (sum(keep) >= 2) {
      y_int <- approx(x = xt[keep], y = y[keep], xout = xt, method = "linear", rule = 1, ties = "ordered")$y
      out$Locomotion <- y_int
    }
  }
  out
}

scip_server <- function(input, output, session) {
  # 旧/新数据模块
  old_df <- dataModuleServer("old_data", expect_new = FALSE)
  new_df <- dataModuleServer("new_data", expect_new = TRUE)

  # 动态范围UI（旧）
  output$xrange_ui_old <- renderUI({
    df <- old_df()
    req(df)
    rng <- range(df$RealTime, na.rm = TRUE)
    sliderInput("xrange_old", "选择X轴范围(时间)", min = rng[1], max = rng[2], value = rng, timeFormat = "%Y-%m-%d %H:%M")
  })
  output$yrange_ui_old <- renderUI({
    df <- old_df()
    req(df)
    rng <- range(df$Temperature, na.rm = TRUE)
    min_y <- if (is.finite(rng[1])) floor(min(25, rng[1])) else 25
    max_y <- if (is.finite(rng[2])) ceiling(max(40, rng[2])) else 40
    sliderInput("yrange_old", "选择Y轴范围(温度)", min = min_y, max = max_y, value = c(min_y, max_y))
  })

  # 动态范围UI（新）
  output$xrange_ui_new <- renderUI({
    df <- new_df()
    req(df)
    rng <- range(df$RealTime, na.rm = TRUE)
    sliderInput("xrange_new", "选择X轴范围(时间)", min = rng[1], max = rng[2], value = rng, timeFormat = "%Y-%m-%d %H:%M")
  })
  output$yrange_ui_new <- renderUI({
    df <- new_df()
    req(df)
    rng <- range(df$Temperature, na.rm = TRUE)
    min_y <- if (is.finite(rng[1])) floor(min(25, rng[1])) else 25
    max_y <- if (is.finite(rng[2])) ceiling(max(40, rng[2])) else 40
    sliderInput("yrange_new", "选择Y轴范围(温度)", min = min_y, max = max_y, value = c(min_y, max_y))
  })
  output$yrange_ui_new_loco <- renderUI({
    df <- new_df()
    req(df)
    if (!"Locomotion" %in% names(df)) return(NULL)
    rng <- range(df$Locomotion, na.rm = TRUE)
    if (!all(is.finite(rng))) rng <- c(0, 1)
    min_y <- max(0, floor(rng[1]))
    max_y <- ceiling(rng[2])
    sliderInput("yrange_new_loco", "选择Y轴范围(Locomotion)", min = min_y, max = max_y, value = c(min_y, max_y))
  })

  # 绘图（旧）
  output$plot_old <- renderPlotly({
    Sys.setlocale("LC_TIME", "C")
    df <- old_df(); req(df, input$xrange_old, input$yrange_old)
    build_temp_plot(
      df = df,
      title = "单体温数据温度曲线",
      light_begin = input$light_begin_old %||% "08:00:00",
      dark_begin  = input$dark_begin_old %||% "20:00:00",
      yrange = input$yrange_old,
      xrange = input$xrange_old,
      shade_df_fun = make_shade_df
    )
  })

  # 绘图（双功能数据）
  output$plot_new <- renderPlotly({
    Sys.setlocale("LC_TIME", "C")
    df <- new_df(); req(df, input$xrange_new, input$yrange_new)
    build_temp_plot(
      df = df,
      title = "双功能数据温度曲线",
      light_begin = input$light_begin_new %||% "08:00:00",
      dark_begin  = input$dark_begin_new %||% "20:00:00",
      yrange = input$yrange_new,
      xrange = input$xrange_new,
      shade_df_fun = make_shade_df
    )
  })

  # 绘图（双功能数据）Locomotion
  output$plot_new_loco <- renderPlotly({
    Sys.setlocale("LC_TIME", "C")
    df <- new_df(); req(df, input$xrange_new)
    if (!"Locomotion" %in% names(df)) return(NULL)
    build_loco_plot(
      df = df,
      title = "双功能数据运动曲线",
      light_begin = input$light_begin_new %||% "08:00:00",
      dark_begin  = input$dark_begin_new %||% "20:00:00",
      yrange = if (!is.null(input$yrange_new_loco)) input$yrange_new_loco else NULL,
      xrange = input$xrange_new,
      shade_df_fun = make_shade_df
    )
  })

  # 清洗报告已移除

  # 下载：保存格式化（按时间范围截取，保留NA，不插值）
  output$download_old <- downloadHandler(
    filename = function() paste0("formatted_old_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      df <- old_df(); req(df)
      # 按当前选择的时间范围截取
      if (!is.null(input$xrange_old)) {
        df <- df[df$RealTime >= input$xrange_old[1] & df$RealTime <= input$xrange_old[2], , drop = FALSE]
      }
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$download_new <- downloadHandler(
    filename = function() paste0("formatted_new_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      df <- new_df(); req(df)
      if (!is.null(input$xrange_new)) {
        df <- df[df$RealTime >= input$xrange_new[1] & df$RealTime <= input$xrange_new[2], , drop = FALSE]
      }
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}


