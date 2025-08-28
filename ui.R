# 现代化UI：shinydashboard + 简洁风格
library(shiny)
library(shinydashboard)
library(plotly)

scip_ui <- dashboardPage(
  dashboardHeader(title = "SCIP V3"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("单体温数据", tabName = "old", icon = icon("file-import")),
      menuItem("双功能数据", tabName = "new", icon = icon("cloud-upload-alt")),
      menuItem("关于", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper { background: #fafafa; }
      .box { border-top-color: #377eb8; }
      .skin-blue .main-header .logo { background-color: #377eb8; }
      .skin-blue .main-header .navbar { background-color: #377eb8; }
    '))),
    tabItems(
      # 单体温数据
      tabItem(tabName = "old",
        fluidRow(
          box(width = 3, title = "单体温数据与设置", solidHeader = TRUE, status = "primary",
              dataModuleUI("old_data", label = "上传单体温数据(CSV)"),
              hr(),
              textInput("light_begin_old", "开灯", value = "08:00:00"),
              textInput("dark_begin_old", "关灯", value = "20:00:00"),
              uiOutput("xrange_ui_old"),
              uiOutput("yrange_ui_old"),
              br(),
              downloadButton("download_old", "保存格式化文件")
          ),
          box(width = 9, title = "单体温数据绘图", solidHeader = TRUE, status = "primary",
              plotlyOutput("plot_old", height = "700px")
          )
        )
      ),
      # 双功能数据
      tabItem(tabName = "new",
        fluidRow(
          box(width = 3, title = "双功能数据与设置", solidHeader = TRUE, status = "primary",
              dataModuleUI("new_data", label = "上传双功能数据(CSV)"),
              hr(),
              textInput("light_begin_new", "开灯", value = "08:00:00"),
              textInput("dark_begin_new", "关灯", value = "20:00:00"),
              uiOutput("xrange_ui_new"),
              uiOutput("yrange_ui_new"),
              uiOutput("yrange_ui_new_loco"),
              br(),
              downloadButton("download_new", "保存格式化文件")
          ),
          box(width = 9, title = "双功能数据绘图", solidHeader = TRUE, status = "primary",
              plotlyOutput("plot_new", height = "340px"),
              br(),
              plotlyOutput("plot_new_loco", height = "340px"),
              br()
          )
        )
      ),
      # 关于页面
      tabItem(tabName = "about",
        fluidRow(
          box(width = 12, title = "关于 SCIP 应用", status = "primary", solidHeader = TRUE,
            h3("温度及活动动态折线图应用 (SCIP App)"),
            p("版本: 3.0.0"),
            hr(),
            h4("应用简介"),
            p("SCIP (Self-developed implant format Conversion and Interactive Plotting) 是一个用于可视化和分析植入式体温监测设备数据的交互式应用。支持单体温数据（旧格式）和双功能数据（体温+活动度，新格式）的导入、清洗、分析和导出。"),
            
            h4("主要功能"),
            tags$ul(
              tags$li("数据导入：支持单体温数据和双功能数据"),
              tags$li("数据清洗：自动检测并处理异常值和缺失数据"),
              tags$li("交互式可视化：温度曲线图和活动度曲线图"),
              tags$li("数据导出：支持导出处理后的数据和图表")
            ),
            
            h4("数据格式说明"),
            h5("双功能数据（新格式）"),
            tags$ul(
              tags$li("必需列：Name, Mac Address, Date, Time, Temperature"),
              tags$li("可选列：Locomotion（活动度）, Injection（注射标记）")
            ),
            
            h5("单体温数据（旧格式）"),
            tags$ul(
              tags$li("必需列：RealTime 或 Date+Time"),
              tags$li("可选列：Temperature（体温）")
            ),
            
            h4("使用帮助"),
            p("1. 在左侧菜单选择数据类型（单体温数据或双功能数据）"),
            p("2. 上传CSV文件并设置开灯/关灯时间"),
            p("3. 使用图表交互功能查看数据详情"),
            p("4. 点击'保存格式化文件'按钮导出处理后的数据"),
            
            hr(),
            p(" 2025 王同飞实验室内部使用"),
            p("如有问题，请联系技术支持")
          )
        )
      )
    )
  )
)

