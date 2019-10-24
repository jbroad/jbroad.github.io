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

# Server -----

server <- function(input, output, session) {
  # Load map data
  leg <- state_legislative_districts("Arizona", "lower")
  # Load registration data
  legreg2018 <- read.csv("legreg2018_2.csv") # District level data, 2018
  legreg2 <- read.csv("az_sld_registration.csv")
  legreg2 <- subset(legreg2, year > 2012.5)
  legreg2$p_diff <- legreg2$prod - legreg2$pror
  legreg2$click <- 0
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
  bico <- c("#919191", "#3C5A8F")
  colors <- c("#919191", "#A3350D", "#3C5A8F") # Gray, red, blue
  colors2 <- c("#A3350D", "#919191", "#3C5A8F") #
  colors5 <- c("#A3350D", "#A3350D", "#919191", "#3C5A8F", "#3C5A8F")
  
  pal <- colorNumeric(
    palette = c("#A3350D", "#f1f1f1", "#3C5A8F"), # light red, red
    domain = leg_merged$twopar
  )
  
  # Map
  map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                          minZoom = 6, maxZoom = 10)) %>%
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
    setView(-111.5,34,zoom=7)
  output$map <- renderLeaflet(map)
  legreg2018 <- legreg2018
  observeEvent(input$map_shape_click, {
    district <- input$map_shape_click
  })
  output$sld <- renderText({
    if (is.null(input$map_shape_click)) {
      slds <- legreg2018[27,3]
      paste("Legislative District", slds)
    }
    else {
      slds <- legreg2018[input$map_shape_click$id,3]
      paste("Legislative District", slds)
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
        geom_text(label = paste0(name$V2,"%"), nudge_y = -5, fontface = "bold", color="white") +
        coord_flip() + 
        ylim(0,60) +
        scale_fill_manual(values=colors) +
        theme_minimal() +
        theme(legend.position="none", 
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
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
        geom_text(label = paste0(name$V2,"%"), nudge_y = -5, fontface = "bold", color="white") +
        coord_flip() + 
        ylim(0,60) +
        scale_fill_manual(values=colors) +
        theme_minimal() +
        theme(legend.position="none", 
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
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
                  fontface = "bold") +
        coord_flip() + 
        theme_minimal() +
        theme(legend.position="none", 
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
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
                  fontface = "bold") +
        coord_flip() + 
        theme_minimal() +
        theme(legend.position="none", 
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
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
                  fontface = "bold") +
        geom_text(data = tempdf, aes(x = district, y = value[variable=="senprodem"],
                                     label = paste0(value[variable=="senprorep"],"%")),
                  nudge_y = 10, 
                  color = "white",
                  fontface = "bold") + 
        coord_flip() + 
        theme_minimal() +
        theme(legend.position="none", 
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
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
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
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
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              plot.margin=unit(c(t=0,r=-.25,b=0,l=-.45),"cm"))
    }
  })
  output$reg <- renderPlot({
    if (is.null(input$map_shape_click)) {
      templeg <- subset(legreg2, district==27)
      ggplot(legreg2, aes(x=year, y=p_diff, 
                          color = as.factor(legreg2$district==27),
                          linetype = as.factor(legreg2$district==27),
                          size = as.factor(legreg2$district==27))) +
        geom_line(aes(group = district)) +
        scale_colour_manual(values = bico) + 
        scale_size_manual(values = c(.5,1.5)) +
        scale_linetype_manual(values = c("dotted","solid")) +
        theme_minimal() +
        theme(legend.position="none",
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    }
    else {
      legreg2$click[legreg2$distr==paste(input$map_shape_click$id)] <- 1
      ggplot(legreg2, aes(x=year, y=p_diff, 
                          color = as.factor(legreg2$click),
                          linetype = as.factor(legreg2$click),
                          size = as.factor(legreg2$click))) +
        geom_line(aes(group = district)) +
        scale_colour_manual(values = bico) + 
        scale_size_manual(values = c(.5,1.5)) +
        scale_linetype_manual(values = c("dotted","solid")) +
        theme_minimal() +
        theme(legend.position="none",
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    }
  })
}

# UI ----
ui <- fluidPage(
  fluidRow(
    column(width = 7,
           fluidRow(
             leafletOutput("map", width="100%", height="100%"),
             style = "height:100vh; background-color: white;")),
    column(width=5,
           fluidRow(
             h3(textOutput("sld")),
             h4("2018 Election Results: AZ Senate"),
             textOutput("sen"),
             plotOutput("senbar", height="35px"),
             style = "height:20vh; 
                      background-color: white;
                      padding: 20px;"),
           fluidRow(
             h4("2018 Election Results: AZ House"),
             textOutput("h1m"),
             plotOutput("houbar", height="35px"),
             style = "height:20vh; 
                      background-color: white;
                      padding: 20px;"),
           fluidRow(
             h4("Major Party Registration Differential"),
             "% Democrats - % Republicans",
             plotOutput("reg", height="350px"),
             style = "height:60vh; 
                      background-color: white; 
                      padding: 0 20px 0 20px;")))
)

shinyApp(ui, server)




