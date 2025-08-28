## SCIP V3 入口：加载模块化UI/Server并运行应用（不使用 setwd，基于 app.R 自身路径）

library(shiny)

# 使用系统默认浏览器打开（在RStudio中也会外部打开）
options(shiny.launch.browser = TRUE)

# 计算当前脚本(app.R)的绝对路径（兼容 Rscript、RStudio、交互运行）
.this_script_path <- function() {
  # 1) Rscript 或 source 调用
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  fileArg <- "--file="
  path <- sub(fileArg, "", cmdArgs[grep(fileArg, cmdArgs)])
  if (length(path) > 0) return(normalizePath(path[1], winslash = "/", mustWork = FALSE))
  # 2) RStudio
  if (interactive()) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      p <- try(rstudioapi::getActiveDocumentContext()$path, silent = TRUE)
      if (!inherits(p, "try-error") && nzchar(p)) {
        return(normalizePath(p, winslash = "/", mustWork = FALSE))
      }
    }
  }
  # 3) 回退：当前工作目录下的 app.R
  return(normalizePath("app.R", winslash = "/", mustWork = FALSE))
}

.app_path <- .this_script_path()
.app_dir  <- dirname(.app_path)

# 统一的 source 助手：相对 app.R 所在目录
.src <- function(rel) {
  f <- normalizePath(file.path(.app_dir, rel), winslash = "/", mustWork = TRUE)
  source(f, encoding = "UTF-8")
}

# 调试信息（可选）
cat("[SCIP] app.R:", .app_path, "\n")
cat("[SCIP] dir  :", .app_dir,  "\n")

# 加载文件（文件位于项目根目录，与 app.R 同级）
tryCatch({
  .src("utils_shade.R")
  .src("modules_data.R")
  .src("modules_plot.R")
  .src("ui.R")
  .src("server.R")
  
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
