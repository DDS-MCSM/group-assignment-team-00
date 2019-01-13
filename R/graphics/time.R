source('R/utils/date.R')

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

distributionDayGraphic <- function (df, title, xaxistitle) {
  hist(x = df$date, format = getDateFormat() , col = 'blue', breaks = "day", freq = T, main = title, xlab = xaxistitle)
}

histogramDay <- function (df) {
  ggplot(data = df, aes(x = df$date)) + geom_histogram(fill ="skyblue", alpha = 1) +
    ggtitle("Histogram") + theme_minimal()
}
