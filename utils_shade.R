# 工具函数：根据日期区间生成暗周期阴影区域
# 输入：date_seq, light_begin, dark_begin, start_time, end_time
# 输出：包含xmin/xmax的数据框
make_shade_df <- function(date_seq, light_begin, dark_begin, start_time, end_time) {
  if (is.null(light_begin) || is.null(dark_begin) || light_begin == dark_begin) return(data.frame())
  if (dark_begin > light_begin) {
    df <- data.frame(
      xmin = as.POSIXct(paste(date_seq, dark_begin)),
      xmax = as.POSIXct(paste(date_seq + 1, light_begin))
    )
  } else {
    df <- data.frame(
      xmin = as.POSIXct(paste(date_seq, dark_begin)),
      xmax = as.POSIXct(paste(date_seq, light_begin))
    )
  }
  df <- df[df$xmin < end_time & df$xmax > start_time, , drop = FALSE]
  if (nrow(df) == 0) return(df)
  df$xmin[df$xmin < start_time] <- start_time
  df$xmax[df$xmax > end_time] <- end_time
  df
}
