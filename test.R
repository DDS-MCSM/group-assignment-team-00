source('R/rawdata.R')
source('R/formatdata.R')


unzipLogFiles()

df <- getData()
dfip <- getDataFrameAllIps(df)


ip_pattern <- "[[:digit:]]{1,3}.[[:digit:]]{1,3}.[[:digit:]]{1,3}.[[:digit:]]{1,3}"
df$ip <- str_extract(df$message, ip_pattern)
