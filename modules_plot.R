# 绘图模块：统一构建Plotly温度折线图
library(plotly)

# 构建温度图（带暗周期与注射标记）
# 参数：
# - df: 数据框，需包含RealTime, Temperature，可选Injection
# - title: 图标题
# - light_begin, dark_begin: 光照设置
# - yrange: y轴范围，长度为2的数值向量
# - xrange: x轴范围，长度为2的POSIXct向量
# - shade_df_fun: 生成暗周期数据框的函数
build_temp_plot <- function(df, title, light_begin, dark_begin, yrange, xrange, shade_df_fun) {
  # 保护性检查
  req <- function(x) if (is.null(x)) stop("缺少必要参数")
  req(df); req(yrange); req(xrange)
  if (!all(c("RealTime", "Temperature") %in% names(df))) stop("缺少RealTime或Temperature列")

  # 类型修正
  if (!inherits(df$RealTime, "POSIXct")) {
    df$RealTime <- as.POSIXct(df$RealTime)
  }
  # 先按xrange过滤，再排序，确保与横轴一致
  df <- df[!is.na(df$RealTime), , drop = FALSE]
  if (!is.null(xrange) && length(xrange) == 2) {
    df <- df[df$RealTime >= xrange[1] & df$RealTime <= xrange[2], , drop = FALSE]
  }
  df <- df[order(df$RealTime), , drop = FALSE]

  # 计算暗周期
  st <- min(df$RealTime, na.rm = TRUE)
  et <- max(df$RealTime, na.rm = TRUE)
  date_seq <- seq(as.Date(st), as.Date(et), by = "day")
  shade_df <- try(shade_df_fun(date_seq, light_begin, dark_begin, st, et), silent = TRUE)
  if (inherits(shade_df, "try-error")) shade_df <- data.frame()

  # 基于每日开/关灯时间构造x轴刻度（标签为具体时间）
  make_ticks <- function(date_seq, light_begin, dark_begin, st, et) {
    tz0 <- attr(df$RealTime, "tzone")
    tz  <- if (is.null(tz0) || tz0 == "") "" else tz0
    light_times <- as.POSIXct(paste(date_seq, light_begin), tz = tz)
    dark_times  <- as.POSIXct(paste(date_seq, dark_begin),  tz = tz)
    vals <- c(light_times, dark_times)
    o <- order(vals)
    vals <- vals[o]
    keep <- !is.na(vals) & vals >= st & vals <= et
    labs <- format(vals, "%m-%d %H:%M")
    list(vals = vals[keep], labs = labs[keep])
  }
  ticks <- make_ticks(date_seq, light_begin, dark_begin, st, et)

  # 构造图形
  p <- plot_ly()

  shapes <- list()
  if (nrow(shade_df) > 0 && length(yrange) == 2 && all(is.finite(yrange))) {
    for (i in seq_len(nrow(shade_df))) {
      shapes[[length(shapes) + 1]] <- list(
        type = "rect", x0 = shade_df$xmin[i], x1 = shade_df$xmax[i],
        y0 = yrange[1], y1 = yrange[2],
        fillcolor = "rgba(128,128,128,0.2)", line = list(width = 0), layer = "below"
      )
    }
  }

  # 注射标记
  if ("Injection" %in% names(df)) {
    inj_times <- df$RealTime[df$Injection == TRUE & !is.na(df$Injection)]
    if (length(inj_times) > 0) {
      for (tm in inj_times) {
        shapes[[length(shapes) + 1]] <- list(
          type = "line", x0 = tm, x1 = tm,
          y0 = yrange[1], y1 = yrange[2],
          line = list(color = "red", width = 1, dash = "dash")
        )
      }
    }
  }

  p <- p %>% add_trace(
    data = df,
    x = ~RealTime, y = ~Temperature,
    type = "scatter", mode = "lines",
    connectgaps = FALSE,
    name = "Temperature",
    line = list(color = "#377eb8", width = 1),
    hoverinfo = "text",
    text = ~paste(
      "时间: ", format(RealTime, "%Y-%m-%d %H:%M"),
      "<br>温度: ", round(Temperature, 2), "°C"
    )
  )

  # 若无可用刻度，回退为默认刻度设置
  has_ticks <- length(ticks$vals) > 0

  p <- p %>% layout(
    title = list(text = title, font = list(family = "Arial", size = 20, weight = "bold")),
    xaxis = list(
      title = "", type = "date",
      tickmode = if (has_ticks) "array" else NULL,
      tickvals = if (has_ticks) ticks$vals else NULL,
      ticktext = if (has_ticks) ticks$labs else NULL,
      tickangle = -60,
      range = xrange,
      showgrid = FALSE, showline = TRUE, linewidth = 1, mirror = TRUE
    ),
    yaxis = list(
      title = "Temperature(°C)", range = yrange, dtick = 3,
      showgrid = FALSE, showline = TRUE, linewidth = 1, mirror = TRUE
    ),
    legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
    margin = list(l = 60, r = 140, t = 60, b = 80),
    hovermode = "closest", showlegend = TRUE,
    plot_bgcolor = "white", paper_bgcolor = "white",
    shapes = shapes, dragmode = "pan"
  ) %>% config(
    displayModeBar = TRUE, scrollZoom = TRUE,
    toImageButtonOptions = list(format = "png", filename = "temperature_plot", width = 1200, height = 800)
  )

  p
}

# 构建Locomotion图（带暗周期与注射标记）
# yrange 可根据数据自动设定，要求提供xrange
build_loco_plot <- function(df, title, light_begin, dark_begin, yrange, xrange, shade_df_fun) {
  # 保护性检查
  req <- function(x) if (is.null(x)) stop("缺少必要参数")
  req(df); req(xrange)
  if (!all(c("RealTime", "Locomotion") %in% names(df))) stop("缺少RealTime或Locomotion列")

  # 类型修正
  if (!inherits(df$RealTime, "POSIXct")) {
    df$RealTime <- as.POSIXct(df$RealTime)
  }
  # 先按xrange过滤，再排序
  df <- df[!is.na(df$RealTime), , drop = FALSE]
  if (!is.null(xrange) && length(xrange) == 2) {
    df <- df[df$RealTime >= xrange[1] & df$RealTime <= xrange[2], , drop = FALSE]
  }
  df <- df[order(df$RealTime), , drop = FALSE]

  # 计算暗周期
  st <- min(df$RealTime, na.rm = TRUE)
  et <- max(df$RealTime, na.rm = TRUE)
  date_seq <- seq(as.Date(st), as.Date(et), by = "day")
  shade_df <- try(shade_df_fun(date_seq, light_begin, dark_begin, st, et), silent = TRUE)
  if (inherits(shade_df, "try-error")) shade_df <- data.frame()

  # y范围：若未提供则根据数据计算
  if (is.null(yrange)) {
    rng <- range(df$Locomotion, na.rm = TRUE)
    if (!all(is.finite(rng))) {
      yrange <- c(0, 1)
    } else {
      pad <- max(1, diff(rng) * 0.05)
      yrange <- c(max(0, floor(rng[1] - pad)), ceiling(rng[2] + pad))
    }
  }

  p <- plot_ly()

  shapes <- list()
  if (nrow(shade_df) > 0 && length(yrange) == 2 && all(is.finite(yrange))) {
    for (i in seq_len(nrow(shade_df))) {
      shapes[[length(shapes) + 1]] <- list(
        type = "rect", x0 = shade_df$xmin[i], x1 = shade_df$xmax[i],
        y0 = yrange[1], y1 = yrange[2],
        fillcolor = "rgba(128,128,128,0.2)", line = list(width = 0), layer = "below"
      )
    }
  }

  if ("Injection" %in% names(df)) {
    inj_times <- df$RealTime[df$Injection == TRUE & !is.na(df$Injection)]
    if (length(inj_times) > 0) {
      for (tm in inj_times) {
        shapes[[length(shapes) + 1]] <- list(
          type = "line", x0 = tm, x1 = tm,
          y0 = yrange[1], y1 = yrange[2],
          line = list(color = "red", width = 1, dash = "dash")
        )
      }
    }
  }

  p <- p %>% add_trace(
    data = df,
    x = ~RealTime, y = ~Locomotion,
    type = "scatter", mode = "lines",
    connectgaps = FALSE,
    name = "Locomotion",
    line = list(color = "#4daf4a", width = 1),
    hoverinfo = "text",
    text = ~paste(
      "时间: ", format(RealTime, "%Y-%m-%d %H:%M"),
      "<br>运动: ", round(Locomotion, 2)
    )
  )

  # 基于每日开/关灯时间构造x轴刻度（标签为具体时间）
  make_ticks <- function(date_seq, light_begin, dark_begin, st, et) {
    tz0 <- attr(df$RealTime, "tzone")
    tz  <- if (is.null(tz0) || tz0 == "") "" else tz0
    light_times <- as.POSIXct(paste(date_seq, light_begin), tz = tz)
    dark_times  <- as.POSIXct(paste(date_seq, dark_begin),  tz = tz)
    vals <- c(light_times, dark_times)
    o <- order(vals)
    vals <- vals[o]
    keep <- !is.na(vals) & vals >= st & vals <= et
    labs <- format(vals, "%m-%d %H:%M")
    list(vals = vals[keep], labs = labs[keep])
  }
  ticks <- make_ticks(date_seq, light_begin, dark_begin, st, et)

  # 若无可用刻度，回退为默认刻度设置
  has_ticks <- length(ticks$vals) > 0

  p <- p %>% layout(
    title = list(text = title, font = list(family = "Arial", size = 20, weight = "bold")),
    xaxis = list(
      title = "", type = "date",
      tickmode = if (has_ticks) "array" else NULL,
      tickvals = if (has_ticks) ticks$vals else NULL,
      ticktext = if (has_ticks) ticks$labs else NULL,
      tickangle = -60,
      range = xrange,
      showgrid = FALSE, showline = TRUE, linewidth = 1, mirror = TRUE
    ),
    yaxis = list(
      title = "Locomotion (Counts)", range = yrange,
      showgrid = FALSE, showline = TRUE, linewidth = 1, mirror = TRUE
    ),
    legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
    margin = list(l = 60, r = 140, t = 60, b = 80),
    hovermode = "closest", showlegend = TRUE,
    plot_bgcolor = "white", paper_bgcolor = "white",
    shapes = shapes, dragmode = "pan"
  ) %>% config(
    displayModeBar = TRUE, scrollZoom = TRUE,
    toImageButtonOptions = list(format = "png", filename = "locomotion_plot", width = 1200, height = 800)
  )

  p
}


