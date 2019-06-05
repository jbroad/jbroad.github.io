# Maps
library(tigris)
options(tigris_use_cache = TRUE)
library(shiny)
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

###### Server

server <- function(input, output, session) {
  # Load registration data
  legreg2018 <- read.csv("legreg2018_2.csv") # District level data, 2018
  
  # Color info
  # E7E7E7 - dark gray under selected tab
  # F8F8F8 - light gray under unselected tab
  colors <- c("#919191", "#DE0000", "#00A1DE")
  pal <- colorNumeric(
    palette = c("#DE0000","#EFE4FF", "#00A1DE"),
    domain = leg_merged$twopar
  )
  
  # Map
  map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                          minZoom = 6, maxZoom = 10)) %>%
#    addPolygons(data = wrld,
 #               fillColor = "#E7E7E7",
  #              color = "#FFFFFF",
   #             fillOpacity = 1,
    #            weight = 1.5,
     #           opacity = 1,
      #          smoothFactor = .6) %>%  
#    addPolygons(data = states,
 #               fillColor = "#E7E7E7",
  #              color = "#FFFFFF",
   #             fillOpacity = 1,
    #            weight = 1.5,
     #           opacity = 1,
      #          smoothFactor = .6) %>%
    addPolygons(data = leg_merged, 
                fillColor = ~pal(twopar), 
                color = "#FFFFFF", 
                fillOpacity = .6, 
                opacity = 1,
                weight = 1.5, 
                highlightOptions = highlightOptions(color = "#FFFFFF", 
                                                    weight = 3,
                                                    opacity = 1,
                                                    fillOpacity = 1,
                                                    bringToFront = T),
                smoothFactor = .6,
                #popup = popup
                layerId = ~leg_merged$district
    ) %>%
    setView(-110.6,34.25,zoom=7)
  output$map <- renderLeaflet(map)
  legreg2018 <- legreg2018
  
  observeEvent(input$map_shape_click, {
    district <- input$map_shape_click
  })
  output$sld <- renderText({
    if (is.null(input$map_shape_click)) {
      slds <- legreg2018[27,3]
      paste("District", slds, "Party Registration")
    }
    else {
      slds <- legreg2018[input$map_shape_click$id,3]
      paste("District", slds, "Party Registration")
    }
  })
  output$sen <- renderText({
    if (is.null(input$map_shape_click)) {
      senmem <- legreg2018[27,26]
      party <- legreg2018[27,27]
      paste0(senmem, " (", party, ")")
    }
    else {
      senmem <- legreg2018[input$map_shape_click$id,26]
      party <- legreg2018[input$map_shape_click$id,27]
      paste0(senmem, " (", party, ")")
    }
  })
  output$h1m <- renderText({
    if (is.null(input$map_shape_click)) {
      h1mem <- legreg2018[27,28]
      h1party <- legreg2018[27,29]
      paste0(h1mem, " (", h1party, ")")
    }
    else {
      h1mem <- legreg2018[input$map_shape_click$id,28]
      h1party <- legreg2018[input$map_shape_click$id,29]
      paste0(h1mem, " (", h1party, ")")
    }
  })
  output$h2m <- renderText({
    if (is.null(input$map_shape_click)) {
      h2mem <- legreg2018[27,30]
      h2party <- legreg2018[27,31]
      paste0(h2mem, " (", h2party, ")")
    }
    else {
      h2mem <- legreg2018[input$map_shape_click$id,30]
      h2party <- legreg2018[input$map_shape_click$id,31]
      paste0(h2mem, " (", h2party, ")")
    }
  })
  output$district <- renderPlot({
    if (is.null(input$map_shape_click)) {
      name <- as.data.frame(matrix(c("1","2","3",legreg2018[27, 23],legreg2018[27, 22],legreg2018[27, 21]),
                                   nrow=3,
                                   ncol=2,
                                   byrow=F),
                            stringsAsFactors = FALSE)
      name$V2 <- as.numeric(name$V2)
      ggplot(name, aes(x=V1, y=V2, fill=V1)) + 
        geom_col() + 
        geom_text(label = name$V2, nudge_y = -5, fontface = "bold", family = "mono", color="white") +
        coord_flip() + 
        ylim(0,60) +
        scale_fill_manual(values=colors) +
        theme_minimal() +
        theme(legend.position="none", 
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(family="mono"),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.margin=unit(c(t=0,r=-.25,b=0,l=-.45),"cm"))
    }
    else {
      name <- as.data.frame(matrix(c("1","2","3",legreg2018[input$map_shape_click$id, 23],legreg2018[input$map_shape_click$id, 22],legreg2018[input$map_shape_click$id, 21]),
                                   nrow=3,
                                   ncol=2,
                                   byrow=F),
                            stringsAsFactors = FALSE)
      name$V2 <- as.numeric(name$V2)
      ggplot(name, aes(x=V1, y=V2, fill=V1)) + 
        geom_col() + 
        geom_text(label = name$V2, nudge_y = -5, fontface = "bold", family = "mono", color="white") +
        coord_flip() + 
        ylim(0,60) +
        scale_fill_manual(values=colors) +
        theme_minimal() +
        theme(legend.position="none", 
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(family="mono"),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.margin=unit(c(t=0,r=-.25,b=0,l=-.45),"cm"))
    }
  })
}

###### UI

ui <- fluidPage(
      tags$style(type = "text/css", 
                  "#map {height: 100vh !important;}"),
      tags$style( 
                 HTML("#controls{padding-left:20px; 
                      padding-right:20px; 
                      padding-top:5px; 
                      padding-bottom:20px;
                      font-family: courier;}")
                      ),
      leafletOutput("map", width="100%", height="100%"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = T, draggable = T,
                  bottom = "auto", left = "auto", right = 5, top = 80, width = 300, height = "auto",
                  h4("District Details"),
                  textOutput("sld"),
                  plotOutput("district", height="80px"),
                  h4("Senate Representation"),
                  textOutput("sen"),
                  h4("House Representation"),
                  textOutput("h1m"),
                  textOutput("h2m")
      ),
      tags$style(type = "text/css", ".container-fluid {padding:0;}")
)

shinyApp(ui, server)




