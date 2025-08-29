## 简化版入口：相对路径加载 + 默认外部浏览器

library(shiny)



# 加载文件（文件位于项目根目录，与 app.R 同级）
tryCatch({
  # 直接按相对路径加载（假定工作目录为应用根目录）
  source("utils_shade.R", encoding = "UTF-8")
  source("modules_data.R", encoding = "UTF-8")
  source("modules_plot.R", encoding = "UTF-8")
  source("ui.R", encoding = "UTF-8")
  source("server.R", encoding = "UTF-8")

  # 检查必要函数是否存在
  if (!exists("scip_ui")) stop("scip_ui not found")
  if (!exists("scip_server")) stop("scip_server not found")
  
  # 运行应用
  cat("Starting Shiny app...\n")

  # 使用系统默认浏览器（在 RStudio 中强制外部打开）
  options(shiny.launch.browser = TRUE, viewer = NULL)

  shinyApp(ui = scip_ui, server = scip_server)
  
}, error = function(e) {
  message("Error:")
  message(conditionMessage(e))
  traceback()
})
