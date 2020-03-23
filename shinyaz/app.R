# AZ Leg
library(shiny)
library(ggplot2)
library(rlist)
library(dplyr)
library(shinythemes)
library(reshape2)
library(shinydashboard)
library(tidyr)

ui <- fluidPage(
  titlePanel(
    "Requests to Speak: Arizona State Legislature (2014-2019)"
  ),
  fluidRow(
    textOutput("test"), style = "padding: 5px; padding-left: 15px;"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("varpro",
                  label = "Choose group category in favor of legislation",
                  choices = c("Companies & Trade Groups",
                              "Government Agencies & Utilities",
                              "Interest Groups, Unions & PACs",
                              "Nonprofits & Charities",
                              "Professional Associations & Licensing",
                              "Public & Charter Schools"),
                  selected = "Companies & Trade Groups"),
      selectInput("varneg",
                  label = "Choose group category opposing legislation",
                  choices = c("Companies & Trade Groups",
                              "Government Agencies & Utilities",
                              "Interest Groups, Unions & PACs",
                              "Nonprofits & Charities",
                              "Professional Associations & Licensing",
                              "Public & Charter Schools"),
                  selected = "Companies & Trade Groups")
    ),
    mainPanel(
      plotOutput("prob")
    )
  )
)

server <- function(input, output, session) {
  load("az_master_count.RData")
  master <- master[grepl("B", levels(as.factor(substr(master$billnum, 1, 2))))==T,]
  d <- master[master$year > 2014,c(21,34,35,37,38,40,41,43,44,46,47,49,50)]
  output$test <- renderText({ 
    paste("Bill Support among", input$varpro, "and Opposition from", input$varneg)
  })
  colrs <- c("compro" = "coral1", "comneg" = "firebrick4",
             "govpro" = "goldenrod1", "govneg" = "goldenrod4",
             "intpro" = "orange1", "intneg" = "orange4",
             "nonprofitpro" = "olivedrab1", "nonprofitneg" = "olivedrab4",
             "profpro" = "thistle1", "profneg" = "thistle4",
             "edupro" = "cadetblue1", "eduneg" = "cadetblue4")
  output$prob <- renderPlot({
    p <- switch(input$varpro,
                "Companies & Trade Groups" = "compro",
                "Government Agencies & Utilities" = "govpro",
                "Interest Groups, Unions & PACs" = "intpro",
                "Nonprofits & Charities" = "nonprofitpro",
                "Professional Associations & Licensing" = "profpro",
                "Public & Charter Schools" = "edupro")
    n <- switch(input$varneg,
                "Companies & Trade Groups" = "comneg",
                "Government Agencies & Utilities" = "govneg",
                "Interest Groups, Unions & PACs" = "intneg",
                "Nonprofits & Charities" = "nonprofitneg",
                "Professional Associations & Licensing" = "profneg",
                "Public & Charter Schools" = "eduneg")
    d3 <- gather(d[order(-d[,p], -d[,n]),], group, num, compro:eduneg, factor_key = T)
    d3$row <- rep(1:nrow(d), 6)
    quick <- rep(c(rep(1,nrow(d)), rep(-1,nrow(d))), 6)
    d3$num <- d3$num * quick
    ggplot(d3[d3$group==p | d3$group==n,], aes(x=row, y=num, color=group)) + 
      geom_segment(aes(x=row, xend=row, y=0, yend=num), size=0.4) +
      scale_color_manual(values=colrs,
                         breaks=c(p,n)) + 
      theme_minimal() +
      ylim(-25, 25) +
      ylab("Requests to Speak in Support (+) and Opposition (-)") +
      xlab("Bill ID (Sorted by Number of Orgs Supporting and Opposing)") +
      theme(
        legend.position = "none"
      )
  })
}
shinyApp(ui, server)


