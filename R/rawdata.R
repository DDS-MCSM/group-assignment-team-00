if (!require(stringr)) {
  install.packages(stringr)
}


library(stringr)


#' Unzip files
#'
#' @details Unzip files
#' @return
#' @export
#'
#' @examples
unzipLogFiles <- function() {
  if (!dir.exists(str_c(str_replace(getwd(), "map", ''),"/logs"))) {
    unzip("logs.zip")
  }
}

#' Log files
#'
#' @details Get a logs from the first log
#' @return a table of logs without format
#' @export
#'
#' @examples

getLogTable1 <- function() {
  df = read.table(str_c(str_replace(getwd(), "map", ''),"/logs/secure-20181118"), sep = ":", fill = TRUE, header= FALSE)
  return (df)
}

#' Log files
#'
#' @details Get a logs from the second log
#' @return a table of logs without format
#' @export
#'
#' @examples
getLogTable2 <- function() {
  df = read.table(str_c(str_replace(getwd(), "map", ''),"/logs/secure-20181125"), sep = ":", fill = TRUE, header= FALSE)
  return (df)
}


#' Log files
#'
#' @details Get a logs from the third log
#' @return a table of logs without format
#' @export
#'
#' @examples
getLogTable3 <- function() {
  df = read.table(str_c(str_replace(getwd(), "map", ''),"/logs/secure-20181202"), sep = ":", fill = TRUE, header= FALSE)
  return (df)
}

#' Log files
#'
#' @details Get a logs from the fourth log
#' @return a table of logs without format
#' @export
#'
#' @examples
getLogTable4 <- function() {
  df = read.table(str_c(str_replace(getwd(), "map", ''),"/logs/secure-20181209"), sep = ":", fill = TRUE, header= FALSE)
  return (df)
}
