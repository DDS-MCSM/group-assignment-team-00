if (!require(shiny)) {
  install.packages("shiny")
}

library(shiny)

if (!require(leaftep)) {
  install.packages("leaftep")
}

library(leaflet)

if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

vars <- c(
  "Only Ips" = 'ips',
  "PortScanner" = "port",
  "Login Attemp" = "login"
)

source(paste(str_replace(getwd(), "map", ''),'/R/formatdata.R', sep=''))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",

                    h2("IP explorer"),

                    selectInput("dataset", "Type Attack", vars)
                   # selectInput("size", "Size", vars, selected = "adultpop"),
                    #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                     # Only prompt for threshold when coloring or sizing by superzip
                    #                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                    #)

                 #   plotOutput("histCentile", height = 200),
                #    plotOutput("scatterCollegeIncome", height = 250)
      )


  )

)

server <- function(input, output, session) {

  points <- eventReactive(input$dataset, {
    if (input$dataset == "port") {
      ips <- getDataFramePortScan(includeIpLocation = TRUE, uniqueIP = TRUE)
    } else if (input$dataset == "login") {
      ips <- getDataFrameUsers(includeIpLocation = TRUE, uniqueIP = TRUE)
    } else {
      ips <- getUniqueIpsDataframe()
    }

    ipsPoints <- data.frame(lat = as.numeric(levels(ips$lat)[ips$lat]), long = as.numeric(levels(ips$long)[ips$long]))

  }, ignoreNULL = FALSE)

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addMarkers(data = points()) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })


}

shinyApp(ui, server)


