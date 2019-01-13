
#dependecies
if (!require(stringr)) {
  install.packages(stringr)
}

if (!require(dplyr)) {
  install.packages("dplyr")
}

library(stringr)
library(dplyr)

source(paste(str_replace(getwd(), "map", ''),"/R/rawdata.R", sep=''))
source(paste(str_replace(getwd(), "map", ''),"/R/iptolocation/ip2location.R", sep=''))


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

  unzipLogFiles();

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


getDataFrameAllIps <- function(df = NULL, includeIpLocation = FALSE) {

  if (is.null(df)) {
    df <- getRawData()
  }


  dfIp <- df

  dfIp$date <- formatDateColumn(dfIp$date)
  dfIp$ip <- getIp(dfIp$message)
  dfIp <- extractNA(dfIp)

  if (includeIpLocation) {
    dfIp <- getIpToLocationDataframe(dfIp, "ip")
  }

  return(dfIp)
}

getIp <- function (ip) {
  ip_pattern <- "[[:digit:]]{1,3}[.][[:digit:]]{1,3}[.][[:digit:]]{1,3}[.][[:digit:]]{1,3}"
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
  return(as.POSIXct(date, format="%Y %m %d %H:%M:%S"))
}

getDataFrameUsers <- function(df = NULL, includeIpLocation = FALSE) {

  if (is.null(df)) {
    df <- getRawData()
  }

  dfUser <- df

  dfUser$user <- getUsername(dfUser$message)
  dfUser <- extractNA(dfUser, 'user')

  dfUser$ip <- getIp(dfUser$message)
  dfUser <- extractNA(dfUser, 'ip')

  dfUser$date <- formatDateColumn(dfUser$date)

  if (includeIpLocation) {
    dfUser <- getIpToLocationDataframe(dfUser, "ip")
  }

  return (dfUser)
}


isPortScanner <- function (message) {
  return (length(str_subset(string = message, pattern = 'Received disconnect')) > 0)
}

getDataFramePortScan<- function(df = NULL, includeIpLocation = FALSE) {

  if (is.null(df)) {
    df <- getRawData()
  }


  df$isPort <- lapply(df$message, FUN = isPortScanner)
  result <- df[df$isPort == TRUE,]
  df$isPort <- NULL
  result$isPort <- NULL
  result$ip <- getIp(result$message)
  result$date <- formatDateColumn(result$date)

  if (includeIpLocation) {
    result <- getIpToLocationDataframe(result, "ip")
  }

  return (result)

}


getDuplicatedDataframeColumn <- function(df, columname) {

  df <-

  data.frame()
}


getDuplicatedUsers <- function (df = NULL) {

  if (is.null(df)) {
      df <- getDataFrameUsers(includeIpLocation = TRUE)
  }

  countUsers <- count(df, user)

  return (countUsers)

}

getDuplicatedUsersWithCountry <- function(df = NULL) {

  if (is.null(df)) {
    df <- getDataFrameUsers(includeIpLocation = TRUE)
  }

  countUsers <- count(df, user, country)

  return (countUsers)
}

getDuplicatedIPs <- function(df = NULL) {

  if (is.null(df)) {
    df <- getDataFrameAllIps(includeIpLocation = TRUE)
  }

  ips <- count(df, ip, country)

  return (ips)
}


getTopUsers <- function (df = NULL, top = 5) {

  usersTop <- getDuplicatedUsers()

  usersTop<- arrange(usersTop, -n)
  usersTopN <- usersTop[1:top, ]

  return (usersTopN)

}


getTopUsersWithDate <- function (df = NULL, top = 5) {

  users <- getDataFrameUsers()
  usersTopN <- getTopUsers(top = top)

  filteredUsers <- inner_join(usersTopN, users, by = "user")

  return (filteredUsers)
}


getTopAttackedDate <- function(df = NULL , top = 5) {

  if (is.null(df)) {
    df <- getRawData()
  }

  df$date <- formatDateColumn(df$date)

  df$hour <- hour(df$date)
  df$day <- day(df$date)

  dfCount <- count(df,day)
  dfCount <- arrange(dfCount, -n)

  dfUniqueDays <-dfCount[1:top,]
  df$isTop <- df$day %in% dfUniqueDays$day
  df <- df[df$isTop == TRUE,]

  return(df)


}


getUniqueIpsDataframe <- function () {
  df <- getFilteredIps()

  return (df)

}
