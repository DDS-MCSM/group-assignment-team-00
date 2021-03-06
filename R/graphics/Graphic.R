source("R/formatdata.R")

if (!suppressMessages(suppressWarnings(require(grid)))) {
  install.packages("grid")
}

if (!suppressMessages(suppressWarnings(require(grid)))) {
  install.packages("gridExtra")
}

if (!suppressMessages(suppressWarnings(require(ggplot2)))) {
  install.packages("ggplot2")
}

if (!suppressMessages(suppressWarnings(require(plotly)))) {
  install.packages("plotly")
}

suppressMessages(suppressWarnings(library(grid)))
suppressMessages(suppressWarnings(library(plotly)))
suppressMessages(suppressWarnings(library(gridExtra)))

densidadUserTop <- function(){
  df <- getTopUsersWithDate()
  # TPara generar un plot de densidad.
  library(ggplot2)
  p  <- ggplot(df, aes(date, colour=user, fill=user))
  p  <- p + geom_density(alpha=0.55)
  p
}

getCountryTopGraphic <- function() {
  df <- getTopAttacksByCountry(top = 5)
  df$country <- as.character(df$country)
  # Make a basic scatter plot :
  p=plot_ly(df, x = ~date, y = ~country, type="scatter", text = paste("Fecha: ", df$date),
            mode = "markers", color = ~date, size = ~date)
  p
}

# Mostrar tabla top 5



tablaUserTop <- function(){
  df <- getTopUsers()
  grid.table(df)
}

tableTopCountry <- function() {
  df <- getTopCountry()
  grid.table(df)
}

# Charge the plotly library


getTopDateGraphic <- function(){
  df <- getTopAttackedDate(top = 20)
  # Make a basic scatter plot :
  p=plot_ly(df, x = ~day, y = ~hour, type="scatter", text = paste("Fecha: ", df$date),
          mode = "markers", color = ~day, size = ~day)
  p
}
