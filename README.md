# 温度及活动动态折线图应用 (SCIP App)

## 项目简介
SCIP (Self-developed implant format Conversion and Interactive Plotting) 是一个用于可视化和分析植入式体温监测设备数据的交互式应用。支持单体温数据（旧格式）和双功能数据（体温+活动度，新格式）的导入、清洗、分析和导出。

## 主要功能

- **数据导入**：支持单体温数据（旧格式）和双功能数据（体温+活动度，新格式）
- **数据清洗**：自动检测并处理异常值和缺失数据
- **交互式可视化**：
  - 温度曲线图（含暗周期阴影）
  - 活动度（Locomotion）曲线图
  - 支持时间范围选择、缩放和平移
- **数据导出**：支持导出处理后的数据和图表

## 目录结构
- `R/` 模块化源码
  - `app.R` 应用入口
  - `ui.R` 界面定义（shinydashboard）
  - `server.R` 服务器逻辑（装配模块）
  - `modules_data.R` 数据导入与清洗（兼容新/旧格式）
  - `modules_plot.R` 绘图模块（温度与活动度）
  - `utils_shade.R` 暗周期阴影辅助函数
- `data/` 数据文件（可选）
- `output/` 导出结果（可选）

## 安装与运行

### 依赖安装
```r
install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "tidyr"))
```

### 运行应用
1. 在R中运行：
   ```r
   source("R/app.R", encoding = "UTF-8")
   ```
2. 或在RStudio中打开 `R/app.R` 并点击 "Run App"

## 支持的数据格式

### 双功能数据（新格式）
- 必需列：`Name`, `Mac Address`, `Date`, `Time`, `Temperature`
- 可选列：`Locomotion`（活动度）, `Injection`（注射标记）
- 时间格式：
  - `Date`: `YYYY:MM:DD` 或 `YYYY.MM.DD` 或 `YYYY/MM/DD`
  - `Time`: `HH:MM:SS` 或 `HH:MM:SS.mmm`（毫秒可选）
- 数值范围：
  - `Temperature`: 体温（°C）
  - `Locomotion`: 活动度（Counts）
  - `Injection`: 注射标记（TRUE/FALSE 或 1/0）

### 单体温数据（旧格式）
- 必需列：`RealTime` 或 `Date`+`Time`
- 可选列：`Temperature`（体温）

## 使用说明

### 数据导入
1. 点击左侧菜单选择数据类型（单体温数据或双功能数据）
2. 点击"上传数据"按钮选择CSV文件
3. 设置开灯和关灯时间（用于暗周期显示）

### 数据可视化
- 温度曲线：显示体温随时间变化
- 活动度曲线：显示活动度随时间变化
- 交互功能：
  - 鼠标悬停查看数据点详情
  - 框选放大/双击还原
  - 时间轴缩放
  - 图例显示/隐藏

### 数据导出
- 点击"保存格式化文件"按钮导出处理后的数据
- 支持导出为CSV格式
- 导出内容包括：
  - 统一时间戳（RealTime）
  - 体温数据（Temperature）
  - 活动度数据（Locomotion，如存在）
  - 注射标记（Injection，如存在）

## 运行方式
1. 在 R 中运行入口：
   ```r
   source("R/app.R", encoding = "UTF-8")
   ```
   或在 RStudio/VSCode 中打开 `R/app.R` 并直接运行。
2. 在应用“新/旧数据导入”页上传 CSV。
3. 调整左侧光照参数与轴范围，查看图表与清洗报告。
4. 如需导出格式化数据，点击侧栏“保存格式化文件”。

## 近期修复与改进
- 修复旧数据绘图的时间轴范围传递问题（直接使用输入的 POSIXct，避免重复转换）。
- 图像按 `RealTime` 排序并启用 `connectgaps=TRUE`，避免仅显示散点或折线错乱。
- 新增格式化导出（对齐分钟 + 线性插值）。

## 常见问题（FAQ）
- 无法解析时间：请检查 `Date/Time` 是否为上述格式；或在 CSV 中提供 `RealTime` 列（标准日期时间）。
- Locomotion 图未显示：CSV 无该列时会自动隐藏对应控件与图。
- 温度/运动值全 NA：请确认分隔符为逗号与小数点格式为 `.`；必要时用 Excel/文本编辑器另存为 UTF-8。

## 开发规范
- 中文注释与简洁风格
- 模块化开发，便于维护与扩展
