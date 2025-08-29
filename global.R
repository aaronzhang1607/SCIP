# 全局初始化：确保在使用 ui.R/server.R 入口时预先加载模块与依赖

# 依赖包
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(plotly)
})

# 总是使用系统默认浏览器打开 Shiny 应用
options(shiny.launch.browser = TRUE, viewer = NULL)

# 直接按相对路径加载（runGitHub 会将工作目录设为应用根目录）
source("utils_shade.R", encoding = "UTF-8")
source("modules_data.R", encoding = "UTF-8")
source("modules_plot.R", encoding = "UTF-8")
