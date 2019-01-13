source('R/utils/date.R')
source('R/formatdata.R')


if (!suppressMessages(suppressWarnings(require(ggplot2)))) {
  install.packages("ggplot2")
  library(ggplot2)
}

distributionDayGraphic <- function (df, title, xaxistitle) {
  hist(x = df$date, format = getDateFormat() , col = 'blue', breaks = "day", freq = T, main = title, xlab = xaxistitle)
}

distributionDayGraphic <- function (df) {
  ggplot(data = df, aes(x = df$date)) + geom_histogram(fill ="skyblue", alpha = 1) +
    ggtitle("Histogram") + xlab(label = "fecha") + ylab(label = "ataques") + theme_minimal()
}

histogramPortScan <- function() {
  df <- getDataFramePortScan()
  return (distributionDayGraphic(df))
}

histogramAllIps <- function() {
  df <- getDataFrameAllIps()
  return (distributionDayGraphic(df))
}

histogramUsers <- function() {
  df <- getDataFrameUsers()
  return (distributionDayGraphic(df))
}
