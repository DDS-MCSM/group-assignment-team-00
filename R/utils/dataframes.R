if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}


getFilename <- function(filename) {
  return (str_c("data/",filename, '.RData'))
}

saveDataFrame <-function (df, filename) {
  filename <- getFilename(filename)
  save(df, file=filename)
}

getSavedDataFrame <- function (name) {
  filename <- getFilename(name)
  if (file.exists(filename)) {
    load(filename)
    #dataframes is loaded in the environment like df
    return (df)
  }
  return(NULL)
}
