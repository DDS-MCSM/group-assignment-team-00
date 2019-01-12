source('R/rawdata.R')
source('R/formatdata.R')
source('R/iptolocation/ip2location.R')
source('R/utils/dataframes.R')


unzipLogFiles()

df <- getRawData()
dfUser <- getDataFrameUsers(df)
dfip <- getDataFrameAllIps(df)

----

ip_pattern <- "[[:digit:]]{1,3}.[[:digit:]]{1,3}.[[:digit:]]{1,3}.[[:digit:]]{1,3}"
df$ip <- str_extract(df$message, ip_pattern)


unzipfile()
df <- getDataIp2Location()
saveDataFrame(df, "ip2location")
df2 <- getSavedDataFrame("ip2location")


rawData <- getRawData()
dfIps <- getDataFrameAllIps(rawData)
dfip <- dfIps;
ip2location <- getDataIp2Location()
dfip$ip <- as.factor(dfip$ip)
dfip$longip <- sapply(dfip['ip'], FUN = ip2long)
subset.data.frame(ip2location, ipBegin <= ip & ip <= ipEnd)
longIps <- dfip[!duplicated(dfip$longip),]$longip
dfip <- sapply(longIps, FUN=findLineNum, ip2location=ip2location)
dfip <- sapply(longIps, FUN=findLineNum, ip2location=ip2location)
ips <- 1
dfip2 <- sapply(dfip[1:20]$longip, FUN=getLocation, ip2location=ip2location)
return (dfip)
dfip['longip', 0]
dfip$longip
