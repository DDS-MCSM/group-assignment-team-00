if (!require(stringr)) {
  install.packages("stringr");
}

library(stringr)

source(paste(str_replace(getwd(), "map", ''), '/R/utils/dataframes.R', sep=''))

getFilteredIps <- function(df = NULL, ipcolname = 'ip', forceRecalc = FALSE) {
  filteredIps <- getSavedDataFrame('iploglocation')

  if (is.null(filteredIps) | forceRecalc) {


    if (!require(devtools)) {
      install.packages("devtools")
      library(devtools)
    }

    if (!require(IPtoCountry)) {
      devtools::install_github("gitronald/IPtoCountry")
      library(IPtoCountry)
      data(IPs)
      #load the library data for ip2location
      devtools::install_github("gitronald/ip2locationData")
      library(ip2locationData)
      data(ip2location.lite.db11)
    }

    #this process is very slow for 1500 ips may take around 1h30min
    ips <- df[ipcolname]
    ips <- removeIPsDuplicaded(ips, 'ip')
    ip2location <- do.call(rbind, sapply(df[ipcolname], FUN=IP_location))
    filteredIps <- cbind(ips$ip, ip2location)
    saveDataFrame('filteredIps')


  }
  return (filteredIps)
}

getIpToLocationDataframe <- function(df, colname, filteredIps = NULL) {

  if (is.null(filteredIps)) {
    filteredIps = getFilteredIps(df, colname, FALSE)
  }

  total <- merge(x = df, y = filteredIps, by = 'ip')

  return (total)

}

removeIPsDuplicaded <- function(df, colname) {
  return(df[!duplicated(df[colname]),])
}

