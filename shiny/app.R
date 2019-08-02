# Maps
library(tigris)
options(tigris_use_cache = TRUE)
library(shiny)
library(shinyWidgets)
library(acs)
library(stringr) # to pad fips codes
library(leaflet)
library(sp)
library(sf)
library(htmlwidgets)
library(maps)
library(stringr)
library(XML)
library(ggplot2)
library(rgeos)
library(devtools)
library(reshape2)
library(ggrepel)

###### Server

server <- function(input, output, session) {
  # Load registration data
  oh <- voting_districts("Ohio")
  nobleco <- readRDS(file = "noble.rds")
  oh <- oh[oh$COUNTYFP10=="121",]
  noblemerge <- geo_join(oh, nobleco, "VTDST10", "VTDST10")
  
  pal <- colorNumeric(
    palette = c("#D4F3FF", "#00A1DE"),
    domain = nobleco$x*100
  )
  
  popup <- paste0("Census Precinct ID: ", noblemerge$VTDST10, "<br>", "Percent of Electors with Popular Names: ", 100*round(noblemerge$x,3), "%")
  
  map3 <- leaflet(noblemerge) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = noblemerge, 
                fillColor = ~pal(100*x), 
                color = "#FFFFFF", # you need to use hex colors
                fillOpacity = .7, 
                weight = 1, 
                highlightOptions = highlightOptions(color = "#FFFFFF", 
                                                    weight = 3,
                                                    opacity = 1,
                                                    fillOpacity = 1,
                                                    bringToFront = T),
                smoothFactor = 0.2,  
                popup = popup) %>%
    addLegend(title = "Noble Co, OH:<br>% Electors w/<br>Popular Names",
      pal = pal, values = ~(100*x), opacity = 1)
  
  output$map <- renderLeaflet(map3)
  }

###### UI

ui <- fluidPage(
      tags$style(type = "text/css", 
                  "#map {height: 100vh !important;
                  background-color: #FFFFFF;}"),
      tags$style(
                 HTML("#controls{padding-left:20px; 
                      padding-right:20px; 
                      padding-top:5px; 
                      padding-bottom:20px;
                      font-family: courier;}")
                      ),
      tags$style(
                  HTML("h4 {text-decoration: underline;}")
        ),
      leafletOutput("map", width="100%", height="100%"),
      tags$style(type = "text/css", ".container-fluid {padding:0;}"),
      tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}")
)

shinyApp(ui, server)




