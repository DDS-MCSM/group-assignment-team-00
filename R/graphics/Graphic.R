source("R/formatdata.R")

densidadUserTop <- function(){
df <- getTopUsersWithDate()
# TPara generar un plot de densidad.
library(ggplot2)
p  <- ggplot(df, aes(date, colour=user, fill=user))
p  <- p + geom_density(alpha=0.55)
p
}

# Mostrar tabla top 5
library(grid)
library(gridExtra)

tablaUserTop <- function(){
df <- getTopUsers()
grid.table(df)
}

# Charge the plotly library
library(plotly)

getTopDateGraphic <- function(){
df <- getTopAttackedDate(top = 20)
# Make a basic scatter plot :
p=plot_ly(df, x = ~day, y = ~hour, type="scatter", text = paste("Fecha: ", df$date),
          mode = "markers", color = ~day, size = ~day)
p
}
