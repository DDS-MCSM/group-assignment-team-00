
#dependecies
if (!require(stringr)) {
  install.packages(stringr)
}

if (!require(dplyr)) {
  install.packages("dplyr")
}

if (!require(lubridate)) {
  install.packages("lubridate")
}

library(stringr)
library(dplyr)
library(lubridate)

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


#' Raw Data
#'
#' @details Get RawData from files formatted in a dataframe of two columns date and message
#' @return a dataframe with all logs
#' @export
#'
#' @examples

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

#' All Ips dataframe
#'
#' @details Get a dataframe of all IPs extracted, optionally can include location data
#' @return a dataframe with all logs with ip
#' @export
#'
#' @examples

getDataFrameAllIps <- function(includeIpLocation = FALSE) {

  df <- getSavedDataFrame('allips')

  if (is.null(df)) {
    df <- getRawData()
  } else {
    return (df)
  }


  dfIp <- df

  dfIp$date <- formatDateColumn(dfIp$date)
  dfIp$ip <- getIp(dfIp$message)
  dfIp <- extractNA(dfIp)

  if (includeIpLocation) {
    dfIp <- getIpToLocationDataframe(dfIp, "ip")
  }

  saveDataFrame(dfIp, 'allips')

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

#' All LoginAttemps dataframe
#'
#' @details Get a dataframe of all users and ips extracted, optionally can include location data
#' @return a dataframe with all logs with ip
#' @export
#'
#' @examples

getDataFrameUsers <- function(df = NULL, includeIpLocation = FALSE, uniqueIP = TRUE) {

  if (is.null(df)) {
    df <- getRawData()
  } else {
    return(df)
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

  if (uniqueIP) {
    dfUser <- distinct(dfUser,ip, .keep_all = TRUE)
  }

  return (dfUser)
}


isPortScanner <- function (message) {
  return (length(str_subset(string = message, pattern = 'Received disconnect')) > 0)
}

#' All PortScan dataframe
#'
#' @details Get a dataframe of all IPs extracted, optionally can include location data
#' @return a dataframe with all logs with ip
#' @export
#'
#' @examples

getDataFramePortScan<- function(df = NULL, includeIpLocation = FALSE, uniqueIP = FALSE) {

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

  if (uniqueIP) {
    result <- distinct(result,ip, .keep_all = TRUE)
  }

  return (result)

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


#' TopCountry dataframe
#'
#' @details Get a dataframe of n countries with more repetitions in logs
#' @return a dataframe
#' @export
#'
#' @examples

getTopCountry <- function(df = NULL, top = 5) {
  if (is.null(df)) {
    df = getDataFrameAllIps(includeIpLocation = FALSE)
  }

  dfCount <- count(df, country)
  dfCount <- arrange(dfCount, -n)

  dfTopCountry <-dfCount[1:top,]

  return(dfTopCountry)
}

#' TopCountry all logs dataframe
#'
#' @details Get a dataframe with all the logs of the n countries with more repetitions in logs
#' @return a dataframe
#' @export
#'
#' @examples

getTopAttacksByCountry <- function (df = NULL, top = 5) {

  if (is.null(df)) {
    df = getDataFrameAllIps(includeIpLocation = FALSE)
  }

  dfTopCountry <- getTopCountry(df, top)
  filteredCountry <- inner_join(dfTopCountry, df, by = "country")

  return(filteredCountry)

}

#' TopUsers dataframe
#'
#' @details Get a dataframe of n users with more repetitions in logs
#' @return a dataframe
#' @export
#'
#' @examples

getTopUsers <- function (df = NULL, top = 5) {

  usersTop <- getDuplicatedUsers()

  usersTop<- arrange(usersTop, -n)
  usersTopN <- usersTop[1:top, ]

  return (usersTopN)

}

#' TopUsers all logs dataframe
#'
#' @details Get a dataframe with all the logs of the n users with more repetitions in logs
#' @return a dataframe
#' @export
#'
#' @examples

getTopUsersWithDate <- function (df = NULL, top = 5) {

  users <- getDataFrameUsers()
  usersTopN <- getTopUsers(top = top)

  filteredUsers <- inner_join(usersTopN, users, by = "user")

  return (filteredUsers)
}

#' TopDate all logs dataframe
#'
#' @details Get a dataframe with all the logs of the n date with more repetitions in logs
#' @return a dataframe
#' @export
#'
#' @examples

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
