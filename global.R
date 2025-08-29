# 全局初始化：确保在使用 ui.R/server.R 入口时预先加载模块与依赖

# 依赖包
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(plotly)
})

# 保证相对路径以当前应用目录为基准
.app_dir <- getwd()
.src <- function(rel) {
  f <- normalizePath(file.path(.app_dir, rel), winslash = "/", mustWork = TRUE)
  source(f, encoding = "UTF-8")
}

# 按顺序加载工具与模块（供 ui.R/server.R 使用）
.src("utils_shade.R")
.src("modules_data.R")
.src("modules_plot.R")
