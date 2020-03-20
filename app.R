# AZ Leg
library(shiny)
library(ggplot2)
library(rlist)
library(dplyr)
library(shinythemes)
library(reshape2)
library(shinydashboard)

f <- function(x) {
    function() {
        sample(1:x,1)
    }
}

m <- 10000

ui <- fluidPage(theme = shinytheme("lumen"),
                sidebarPanel(
                    fluidRow(
                        plotOutput("prob", height="300px")
                    )
                )
)

server <- function(input, output, session) {
    load("az_master_count.RData")
    d <- master[master$year > 2018,c(34,35,37,38,40,41,43,44,46,47,49,50)]
    share <- function(x, pro, neg) {
        p <- which(colnames(d)==pro)
        n <- which(colnames(d)==neg)
        d3 <- gather(x[order(-x[,p], -x[,n]),], group, num, compro:eduneg, factor_key = T)
        #d3 <- d3[order(as.numeric(d3$group), -d3$num),]
        d3$row <- rep(1:nrow(d), 6)
        quick <- rep(c(rep(1,nrow(d)), rep(-1,nrow(d))), 6)
        d3$num <- d3$num * quick
        d3$g2 <- ifelse(grepl("com", d3$group), "Companies",
                        ifelse(grepl("gov", d3$group), "Government Agencies",
                               ifelse(grepl("int", d3$group), "Interest Groups", 
                                      ifelse(grepl("nonp", d3$group), "Nonprofits",
                                             ifelse(grepl("profp|profn", d3$group), "Professional Agencies", "Education")))))
        #d3[d3$g2=="Companies",] <- d3[d3$g2=="Companies",][order(-(d3[d3$g2=="Companies",]$num)),]
        output$prob <- renderPlot({
          ggplot(d3[d3$group==pro | d3$group==neg,], aes(x=row, y=num, color=group)) + 
            geom_segment(aes(x=row, xend=row, y=0, yend=num), size=.1) +
            theme_minimal()
        })
    }
}
shinyApp(ui, server)


