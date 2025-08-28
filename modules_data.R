# 数据导入模块（兼容旧/新两种格式）
# UI函数
library(shiny)

# 解析时间列的通用函数：
# - 若有RealTime，尝试多种格式解析
# - 若有Date+Time，组合后解析
.parse_realtime <- function(df) {
  if ("RealTime" %in% names(df)) {
    rt <- df$RealTime
  } else if (all(c("Date", "Time") %in% names(df))) {
    # 兼容不同日期分隔符
    d <- gsub("[.:]", "/", as.character(df$Date))
    t <- sub("(\\d{2}:\\d{2}:\\d{2})\\..*", "\\1", as.character(df$Time))
    rt <- paste(d, t)
  } else {
    return(NULL)
  }
  # 尝试常见格式
  try_formats <- c("%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M")
  parsed <- NULL
  for (fmt in try_formats) {
    parsed <- try(as.POSIXct(rt, format = fmt), silent = TRUE)
    if (!inherits(parsed, "try-error") && any(!is.na(parsed))) break
  }
  if (inherits(parsed, "try-error")) return(NULL)
  parsed
}

# 统一清洗函数：返回包含 RealTime, Temperature, Locomotion(可选), Injection(可选)
clean_dataframe <- function(df) {
  # 统一列名大小写
  names(df) <- trimws(names(df))
  # 寻找温度列名
  temp_col <- c("Temperature", "Temp", "temperature")
  col_t <- intersect(temp_col, names(df))
  if (length(col_t) == 0) stop("未找到温度列(Temperature/Temp)")
  Temperature <- suppressWarnings(as.numeric(df[[col_t[1]]]))
  # 兼容Locomotion列（若存在则转为数值）
  loco_col <- c("Locomotion", "loco", "Activity", "activity")
  col_l <- intersect(loco_col, names(df))
  Locomotion <- if (length(col_l) > 0) suppressWarnings(as.numeric(df[[col_l[1]]])) else NULL
  RealTime <- .parse_realtime(df)
  if (is.null(RealTime)) stop("未找到可解析的时间列(RealTime或Date+Time)")
  Injection <- if ("Injection" %in% names(df)) df$Injection else NULL
  if (!is.null(Injection)) {
    if (is.character(Injection)) Injection <- toupper(trimws(Injection)) == "TRUE"
    if (is.factor(Injection)) Injection <- as.logical(as.character(Injection))
  }
  out <- data.frame(RealTime = RealTime, Temperature = Temperature, stringsAsFactors = FALSE)
  if (!is.null(Locomotion)) out$Locomotion <- Locomotion
  if (!is.null(Injection)) out$Injection <- Injection
  # 仅移除RealTime为NA的行，保留Temperature为NA（缺失值不插补）
  out <- out[!is.na(out$RealTime), , drop = FALSE]
  # 不再构造分钟网格，保留原始时间分辨率
  # 按时间排序，便于后续绘图
  if (nrow(out) > 0) {
    out <- out[order(out$RealTime), , drop = FALSE]
  }
  out
}

# 生成数据清洗报告：
# - 缺失值统计
# - 异常值判定：Temperature不在[20,45]；Locomotion<0
generate_clean_report <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  n_total <- nrow(df)

  # 温度
  temp_na <- sum(is.na(df$Temperature))
  temp_out <- sum(!is.na(df$Temperature) & (df$Temperature < 20 | df$Temperature > 45))

  # 运动
  has_loco <- "Locomotion" %in% names(df)
  loco_na <- if (has_loco) sum(is.na(df$Locomotion)) else NA_integer_
  loco_out <- if (has_loco) sum(!is.na(df$Locomotion) & df$Locomotion < 0) else NA_integer_

  out <- data.frame(
    变量 = c("Temperature", "Locomotion"),
    总数 = c(n_total, n_total),
    缺失数 = c(temp_na, loco_na),
    缺失率 = c(round(100 * temp_na / n_total, 2), if (is.na(loco_na)) NA_real_ else round(100 * loco_na / n_total, 2)),
    异常数 = c(temp_out, loco_out),
    异常率 = c(round(100 * temp_out / n_total, 2), if (is.na(loco_out)) NA_real_ else round(100 * loco_out / n_total, 2)),
    stringsAsFactors = FALSE
  )
  out
}

# 线性插值：对齐到分钟序列后的 NA 采用线性插值（不外推）
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

# 模块UI
dataModuleUI <- function(id, label = "数据导入") {
  ns <- NS(id)
  tagList(
    h4(label),
    fileInput(ns("file"), "选择CSV文件", accept = ".csv"),
    checkboxInput(ns("header"), "包含表头", TRUE),
    checkboxInput(ns("show_preview"), "显示预览", FALSE),
    tableOutput(ns("preview"))
  )
}

# 模块Server：返回reactive数据框
# 参数expect_new=TRUE时代表新格式数据区域（仅用于标识，不强制格式）
dataModuleServer <- function(id, expect_new = FALSE) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveVal(NULL)
    observeEvent(input$file, {
      req(input$file)
      df <- try(read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE, fileEncoding = "UTF-8"), silent = TRUE)
      if (inherits(df, "try-error")) {
        showNotification("读取CSV失败，请检查编码或分隔符", type = "error")
        return()
      }
      cleaned <- try(clean_dataframe(df), silent = TRUE)
      if (inherits(cleaned, "try-error")) {
        showNotification(paste("清洗数据失败:", as.character(cleaned)), type = "error")
        return()
      }
      rv(cleaned)
      showNotification(ifelse(expect_new, "新数据加载成功", "数据加载成功"), type = "message")
    })

    output$preview <- renderTable({
      req(input$show_preview, rv())
      df <- head(rv(), 3)
      if ("RealTime" %in% names(df) && inherits(df$RealTime, "POSIXct")) {
        df$RealTime <- format(df$RealTime, "%Y-%m-%d %H:%M:%S")
      }
      df
    }, rownames = FALSE)

    return(rv)
  })
}


