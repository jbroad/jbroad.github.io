library(tigris)
options(tigris_use_cache = TRUE)
library(rsconnect)
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

###### Setup

# Load polygon data
states <- states(cb = T) # Load data from tigris package
leg <- state_legislative_districts("Arizona", "lower") #Also from tigris
wrld <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE)) # from maps package

###### Server

server <- function(input, output, session) {
  # Load registration data
  legreg2018 <- read.csv("legreg2018.csv") # District level data, 2018
  
  # Pad the GEOID to match the polygon data
  legreg2018$GEOID <- str_pad(as.character(legreg2018$GEOID), 5, side="left", pad="0")
  
  # Merge the manipulated data with the polygon data
  leg_merged<- geo_join(leg, legreg2018, "GEOID", "GEOID")
  
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
    addPolygons(data = wrld,
                fillColor = "#E7E7E7",
                color = "#FFFFFF",
                fillOpacity = 1,
                weight = 1.5,
                opacity = 1,
                smoothFactor = .6) %>%  
    addPolygons(data = states,
                fillColor = "#E7E7E7",
                color = "#FFFFFF",
                fillOpacity = 1,
                weight = 1.5,
                opacity = 1,
                smoothFactor = .6) %>%
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
    if (is.null(input$map_shape_click)) return("")
    else {
      slds <- legreg2018[input$map_shape_click$id,12]
      paste("State Legislative District ", slds)
    }
  })
  output$district <- renderPlot({
    if (is.null(input$map_shape_click)) return("")
    else {
      name <- as.data.frame(matrix(c("1","2","3",legreg2018[input$map_shape_click$id, 10],legreg2018[input$map_shape_click$id, 9],legreg2018[input$map_shape_click$id, 8]),
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

ui <- navbarPage("Joseph Broad", 
                 id="nav",
                 tabPanel("Research",
                          #div(class="outer",
                              #tags$head(
                                # Include our custom CSS
                                #includeCSS("styles.css"))
                          #),
                          tags$style(type = "text/css", 
                                     "#map {height: calc(100vh - 52px) !important;}"),
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = T, draggable = T,
                                        bottom = "auto", left = "auto", right = 20, top = 60, width = 300, height = "auto",
                                        h4("District Details"),
                                        textOutput("sld"),
                                        plotOutput("district", height="80px")
                          ),
                          tags$style(type = "text/css", ".container-fluid {padding-right:0px; padding-left:0px; padding-bottom:0px;}"),
                          tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}")
                 ),
                 tabPanel("Home",
                          "Under Construction"),
                 tabPanel("Teaching"))

shinyApp(ui, server)




