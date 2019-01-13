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
