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
  leg <- state_legislative_districts("Arizona", "lower")
  legreg2018 <- read.csv("legreg2018_2.csv") # District level data, 2018
  # Pad the GEOID to match the polygon data
  legreg2018$GEOID <- str_pad(as.character(legreg2018$GEOID), 5, side="left", pad="0")
  leg_merged<- geo_join(leg, legreg2018, "GEOID", "GEOID")
  sendf <- legreg2018[c(25,37,36,35)]
  sendf <- melt(sendf, id.vars="district")
  houdf <- legreg2018[c(25,46,45,47,44,43)]
  houdf <- melt(houdf, id.vars="district")
  houdf2 <- subset(legreg2018[c(25,49,48)])
  houdf2 <- melt(houdf2, id.vars="district")
  
  # Color info
  # E7E7E7 - dark gray under selected tab
  # F8F8F8 - light gray under unselected tab
  colors <- c("#919191", "#DE0000", "#00A1DE")
  colors2 <- c("#DE0000", "#919191", "#00A1DE")
  colors5 <- c("#DE0000", "#DE0000", "#919191", "#00A1DE", "#00A1DE")
  
  pal <- colorNumeric(
    palette = c("#D4F3FF", "#00A1DE"),
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
                fillOpacity = .8, 
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
      percent <- (legreg2018[27,32]/(legreg2018[27,32] + legreg2018[27,33] + legreg2018[27,34]))*100
      paste0(senmem, " (", party, ") won in 2018 with ", percent, "% of the vote.")
    }
    else {
      senmem <- legreg2018[input$map_shape_click$id,26]
      party <- legreg2018[input$map_shape_click$id,27]
      percent <- round((legreg2018[input$map_shape_click$id,32]/(legreg2018[input$map_shape_click$id,32] + legreg2018[input$map_shape_click$id,33] + legreg2018[input$map_shape_click$id,34]))*100, 2)
      paste0(senmem, " (", party, ") won in 2018 with ", percent, "% of the vote.")
    }
  })
  output$h1m <- renderText({
    if (is.null(input$map_shape_click)) {
      h1mem <- legreg2018[27,28]
      h1party <- legreg2018[27,29]
      h2mem <- legreg2018[27,30]
      h2party <- legreg2018[27,31]
      percenth1 <- round((legreg2018[27,48]), 2)
      percenth2 <- round((legreg2018[27,49]), 2)
      paste0(h1mem, " (", h1party, ") and ", h2mem, " (", h2party, ") won in 2018 with ", percenth1, "% and ", percenth2, "% of the vote, respectively")
    }
    else {
      h1mem <- legreg2018[input$map_shape_click$id,28]
      h1party <- legreg2018[input$map_shape_click$id,29]
      h2mem <- legreg2018[input$map_shape_click$id,30]
      h2party <- legreg2018[input$map_shape_click$id,31]
      percenth1 <- round((legreg2018[input$map_shape_click$id,48]), 2)
      percenth2 <- round((legreg2018[input$map_shape_click$id,49]), 2)
      paste0(h1mem, " (", h1party, ") and ", h2mem, " (", h2party, ") won in 2018 with ", percenth1, "% and ", percenth2, "% of the vote, respectively")
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
        geom_text(label = paste0(name$V2,"%"), nudge_y = -5, fontface = "bold", family = "mono", color="white") +
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
        geom_text(label = paste0(name$V2,"%"), nudge_y = -5, fontface = "bold", family = "mono", color="white") +
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
  output$senbar <- renderPlot({
    if (is.null(input$map_shape_click)) {
      tempdf <- subset(sendf, district==27)
      ggplot() + 
        geom_bar(aes(y = value, x = district, fill = variable), data = tempdf, color="white",
                 stat="identity") +
        scale_fill_manual(values=colors2) +
        geom_text(data = tempdf, aes(x = district, y = value[variable=="senprodem"],
                                     label = paste0(value[variable=="senprodem"],"%")), 
                  nudge_y = -10, 
                  color = "white",
                  fontface = "bold", 
                  family = "mono") +
        coord_flip() + 
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
    else if (input$map_shape_click$id==3 | input$map_shape_click$id==27 | input$map_shape_click$id==4 | input$map_shape_click$id==19 | input$map_shape_click$id==30) {
      tempdf <- subset(sendf, district==paste(input$map_shape_click$id))
      ggplot() + 
        geom_bar(aes(y = value, x = district, fill = variable), data = tempdf, color="white",
                 stat="identity") +
        scale_fill_manual(values=colors2) +
        geom_text(data = tempdf, aes(x = district, y = value[variable=="senprodem"],
                                     label = paste0(value[variable=="senprodem"],"%")), 
                  nudge_y = -10, 
                  color = "white",
                  fontface = "bold", 
                  family = "mono") +
        coord_flip() + 
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
      tempdf <- subset(sendf, district==paste(input$map_shape_click$id))
      ggplot() + 
        geom_bar(aes(y = value, x = district, fill = variable), data = tempdf, color="white",
                 stat="identity") +
        scale_fill_manual(values=colors2) +
        geom_text(data = tempdf, aes(x = district, y = value[variable=="senprodem"],
                                        label = paste0(value[variable=="senprodem"],"%")), 
                  nudge_y = -10, 
                  color = "white",
                  fontface = "bold", 
                  family = "mono") +
        geom_text(data = tempdf, aes(x = district, y = value[variable=="senprodem"],
                                        label = paste0(value[variable=="senprorep"],"%")),
                  nudge_y = 10, 
                  color = "white",
                  fontface = "bold", 
                  family = "mono") + 
        coord_flip() + 
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
  output$houbar <- renderPlot({
    if (is.null(input$map_shape_click)) {
      tempdf2 <- subset(houdf, district==27)
      ggplot() + 
        geom_bar(aes(y = value, x = district, fill = variable), data = tempdf2, color="white",
                 stat="identity") +
        scale_fill_manual(values=colors5) +
        coord_flip() + 
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
      tempdf2 <- subset(houdf, district==paste(input$map_shape_click$id))
      ggplot() + 
        geom_bar(aes(y = value, x = district, fill = variable), data = tempdf2, color="white",
                 stat="identity") +
        scale_fill_manual(values=colors5) +
        coord_flip() + 
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
      absolutePanel(id = "controls", class = "panel panel-default", fixed = T, draggable = T,
                  bottom = "auto", left = "auto", right = 5, top = 80, width = 300, height = "auto",
                  h4("District Details"),
                  textOutput("sld"),
                  plotOutput("district", height="80px"),
                  h4("Senate Representation"),
                  textOutput("sen"),
                  plotOutput("senbar", height="35px"),
                  h4("House Representation"),
                  textOutput("h1m"),
                  plotOutput("houbar", height="35px")
      ),
      tags$style(type = "text/css", ".container-fluid {padding:0;}")
)

shinyApp(ui, server)




