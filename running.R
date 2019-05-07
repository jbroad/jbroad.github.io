library(tigris)
library(rsconnect)
library(shiny)
library(acs)
library(stringr) # to pad fips codes
library(leaflet)
library(sp)
library(htmlwidgets)
library(reshape2)
library(ggplot2)

# Load polygon data
states <- states(cb = T)
leg <- state_legislative_districts("Arizona", "lower")

# Load registration data
# Load registration data
legreg2018 <- read.csv("~/Documents/GitHub/jbroad.github.io/shiny/legreg2018.csv") # District level data, 2018

# Pad the GEOID to match the polygon data
legreg2018$GEOID <- str_pad(as.character(legreg2018$GEOID), 5, side="left", pad="0")

# Merge the manipulated data with the polygon data
leg_merged<- geo_join(leg, legreg2018, "GEOID", "GEOID")

# Melt data
colors <- c("#919191", "#DE0000", "#00A1DE")
t <- as.data.frame(matrix(c("1","2","3",round(legreg2018[1,10],2),round(legreg2018[1,9],2),round(legreg2018[1,8],2)),
            nrow=3,
            ncol=2,
            byrow=F),
            stringsAsFactors = FALSE)
t$V2 <- as.numeric(t$V2)
# Plot
ggplot(t, aes(x=V1, y=V2, fill=V1)) + 
  geom_col() + 
  geom_text(label = t$V2, nudge_y = -5, fontface = "bold", family = "mono", color="white") +
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
        plot.margin=unit(c(t=0,r=-.5,b=0,l=-.5),"cm"))



