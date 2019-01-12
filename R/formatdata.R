
source("R/rawdata.R")

#dependecies
if (!require(stringr)) {
  install.packages(stringr)
}


library(stringr)

#' Format dates
#'
#' @details Unzip files
#' @return
#' @export
#'
#' @examples
formatDate <- function(df) {
  datecolumn <- stringr::str_c(df$V1, df$V2, str_sub(df$V3, start = 1, end = 2), sep = ':')
  return (datecolumn)
}

#' Format Message
#'
#' @details Get the message info of log formatted
#' @return  message column formatted
#' @export
#'
#' @examples
formatMessage <- function(df) {
  messagecolumn <-stringr::str_c(df$V4,df$V5,df$v6)
  return (messagecolumn)
}

#' GetDateFrame
#'
#' @details Get DataFrame of LogTable
#' @return dataframe
#' @export
#'
#' @examples
getDataFrameLog <- function(logTable) {
  df <- data.frame(date = formatDate(logTable), message = formatMessage(logTable), stringsAsFactors = FALSE)
  return(df)
}


mergeAllDataFrames <- function(df1, df2, df3, df4) {
  submerge <- rbind(df1,df2)
  submerge2 <- rbind(df3,df4)
  return (rbind(submerge, submerge2))
}


extractNA <- function(df, columnname) {
  return (df[!is.na(df[columnname]),])
}

getRawData <- function () {

  logTable1 <- getLogTable1()
  df1 <- getDataFrameLog(logTable1)

  logTable2 <- getLogTable2()
  df2 <- getDataFrameLog(logTable2)

  logTable3 <- getLogTable3()
  df3 <- getDataFrameLog(logTable3)

  logTable4 <- getLogTable4()
  df4 <- getDataFrameLog(logTable4)

  df <- mergeAllDataFrames(df1, df2, df3, df4)

  return (df)

}


getDataFrameAllIps <- function(df) {
  dfIp <- df
  ip_pattern <- "[[:digit:]]{1,3}.[[:digit:]]{1,3}.[[:digit:]]{1,3}.[[:digit:]]{1,3}"
  df$date <- formatDateColumn(dfIp$date)
  dfIp$ip <- str_extract(dfIp$message, ip_pattern)
  dfIp <- extractNA(dfIp, 'ip')
  return(extractNA(dfIp, 'ip'))
}

getIp <- function (ip) {
  ip_pattern <- "[[:digit:]]{1,3}.[[:digit:]]{1,3}.[[:digit:]]{1,3}.[[:digit:]]{1,3}"
  return(str_extract(ip, ip_pattern))
}

getUsername <- function(string) {
  pattern <- '(?<=Invalid user\\s)\\w+'
  return(str_extract(string,pattern))
}


formatDateColumn <- function(string) {
  date <- str_replace(string = string, pattern = 'Nov', replace ='11')
  date <- str_replace(string = date, pattern = 'Dec', replace ='12')
  date <- str_c("2018", date, sep=" ")
  as.POSIXct(date, format="%Y %m %d %H:%M:%S")
}

getDataFrameUsers <- function(df) {
  dfUser <- df

  pattern <- '(?<=Invalid user\\s)\\w+'

  dfUser$user <- getUsername(dfUser$message)
  dfUser <- extractNA(dfUser, 'user')

  dfUser$ip <- getIp(dfUser$message)
  dfUser <- extractNA(dfUser, 'ip')

  dfUser$date <- formatDateColumn(dfUser$date)

  return (dfUser)
}


