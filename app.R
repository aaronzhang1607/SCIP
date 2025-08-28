## SCIP V3 入口：加载模块化UI/Server并运行应用

library(shiny)

# 设置工作目录到项目根目录
setwd("d:/OneDrive - sustc1607/WangLab/Projects/Mouse Torpor/Agtr1a/SCIP_V3")

# 加载所有必要的R文件
cat("Current working directory:", getwd(), "\n")
cat("Files in directory:", list.files("R"), "\n")

# 加载文件
tryCatch({
  source("R/utils_shade.R", encoding = 'UTF-8')
  source("R/modules_data.R", encoding = 'UTF-8')
  source("R/modules_plot.R", encoding = 'UTF-8')
  source("R/ui.R", encoding = 'UTF-8')
  source("R/server.R", encoding = 'UTF-8')
  
  # 检查必要函数是否存在
  if (!exists("scip_ui")) stop("scip_ui not found")
  if (!exists("scip_server")) stop("scip_server not found")
  
  # 运行应用
  cat("Starting Shiny app...\n")
  shinyApp(ui = scip_ui, server = scip_server)
  
}, error = function(e) {
  message("Error:")
  message(conditionMessage(e))
  traceback()
})
