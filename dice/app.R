# DnD Stats
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
                titlePanel("Dungeons and Data"),
  sidebarPanel(
        fluidRow(
          actionButton("d4", "D4", width="50px"),
          actionButton("d6", "D6", width="50px"),
          actionButton("d8", "D8", width="50px")
        ),
        br(),
        fluidRow(
          actionButton("d10", "D10", width="50px"),
          actionButton("d12", "D12", width="50px"),
          actionButton("d20", "D20", width="50px")
        ),
        br(),
        fluidRow(
          actionButton("reset","Clear", width="76px"),
          actionButton("roll", "ROLL!", width="77px")
        ),
        br(),
        wellPanel(
          fluidRow(
            textOutput("count"),
            textOutput("outcome"),
            textOutput("text1"),
            tags$head(tags$style("#text1{color: #FC9040;
                                        font-size: 40px;
                                  }"
                          )))),
        br(),
        fluidRow(
          sliderInput("ac", "Baddie's AC", min=1, max=30, value=10),
          sliderInput("atk", "ATK Modifier", min=0, max=15, value=3)
                )
        ),
  mainPanel(
    fluidRow(
        plotOutput("hi")
        ),
    fluidRow(
           plotOutput("prob")
         )
  )
)

server <- function(input, output, session) {
  val <- reactiveValues(d4 = 0, d6 = 0, d8 = 0, d10 = 0, d12 = 0, d20 = 0)

  observeEvent(input$d20, {
    val$d20 <- val$d20 + 1
  })
  observeEvent(input$d12, {
    val$d12 <- val$d12 + 1
  })
  observeEvent(input$d10, {
    val$d10 <- val$d10 + 1
  })
  observeEvent(input$d8, {
    val$d8 <- val$d8 + 1
  })
  observeEvent(input$d6, {
    val$d6 <- val$d6 + 1
  })
  observeEvent(input$d4, {
    val$d4 <- val$d4 + 1
  })
  output$count <- renderText({
    vald4 <- paste0(val$d4,"d4")
    vald6 <- paste0(val$d6,"d6")
    vald8 <- paste0(val$d8,"d8")
    vald10 <- paste0(val$d10,"d10")
    vald12 <- paste0(val$d12,"d12")
    vald20 <- paste0(val$d20,"d20")
    vals <- c(vald4,vald6,vald8,vald10,vald12,vald20)
    vals[vals=="0d4"|vals=="0d6"|vals=="0d8"|vals=="0d10"|vals=="0d12"|vals=="0d20"] <- NA
    vals <- na.omit(vals)
    paste0(vals,collapse = " + ")
  })
  observeEvent(input$roll, {
    rolls <- unlist(c(replicate(val$d4, f(4)()), 
                      replicate(val$d6, f(6)()),
                      replicate(val$d8, f(8)()),
                      replicate(val$d10, f(10)()),
                      replicate(val$d12, f(12)()),
                      replicate(val$d20, f(20)())
    )
    )
    output$outcome <- renderText({
      if (is.null(input$roll)) return("")
      else {
        paste0( "(", paste(rolls, collapse = " + "), ") = ")
      }
    })
    output$text1 <- renderText({ 
      if (is.null(input$roll)) return("")
      else {
        paste(sum(rolls)) 
      }
      })

    summer <- reactive(sum(rolls))
    output$hi <- renderPlot({
      if (val$d4==0) hd4 <- rep(0,m)
      else {
        hd4 <- rowSums(replicate(val$d4, sample(1:4,m,replace=T)))
      }
      if (val$d6==0) hd6 <- rep(0,m)
      else {
        hd6 <- rowSums(replicate(val$d6, sample(1:6,m,replace=T)))
      }
      if (val$d8==0) hd8 <- rep(0,m)
      else {
        hd8 <- rowSums(replicate(val$d8, sample(1:8,m,replace=T)))
      }
      if (val$d10==0) hd10 <- rep(0,m)
      else {
        hd10 <- rowSums(replicate(val$d10, sample(1:10,m,replace=T)))
      }
      if (val$d12==0) hd12 <- rep(0,m)
      else {
        hd12 <- rowSums(replicate(val$d12, sample(1:12,m,replace=T)))
      }
      if (val$d20==0) {
        hd20 <- rep(0,m)
      }
      else {
        hd20 <- rowSums(replicate(val$d20, sample(1:20,m,replace=T)))
      }
      e <- summer()
      
      #hist(hd4 + hd6 + hd8 + hd10 + hd12 + hd20, probability=T,xlab=NULL, main=NULL)
      #abline(v=chubbs, col=c("red"), lty=c(2), lwd=c(3))
      x <- hd4 + hd6 + hd8 + hd10 + hd12 + hd20
      t <- as.data.frame(table(x))
      t$x <- as.numeric(t$x)
      t$z <- e > t$x
      p <- round((mean(t$z > 0))*100, 0)
      t$z[e==t$x] <- 2
      ggplot(t, aes(x=t$x, y=t$Freq, fill=as.factor(t$z))) +
        geom_bar(position = "dodge", stat="identity") +
        theme_classic() +
        scale_fill_manual(name="Var2", values = c("#FC9040", "#180B55", "#2B879C")) + # higher, lower, mid
        xlab(element_blank()) +#2B879C
        ylab(element_blank()) +#180B55
        labs(title = paste0("Your roll of ", e, " was higher than roughly ", p, "% of all possible rolls")) +
        theme(legend.position = "none")
    })
  })
  observeEvent(input$reset,{
    output$outcome <- renderText({
      return("")
    })
    output$text1 <- renderText({
      return("")
    })
    output$hi <- renderPlot({
      NULL
    })
    val$d4 <- 0
    val$d6 <- 0
    val$d8 <- 0
    val$d10 <- 0
    val$d12 <- 0
    val$d20 <- 0
  })
  output$prob <- renderPlot({
    badac <- c(1:30)
    probhit <- c()
    probadv <- c()
    probdis <- c()
    test1 <- data.frame(table(expand.grid(1:20, 1:20, 1:input$atk)))
    test1$Var1 <- as.numeric(test1$Var1)
    test1$Var2 <- as.numeric(test1$Var2)
    test1$Var3 <- as.numeric(test1$Var3)
    test1$adv <- pmax(test1$Var1, test1$Var2)
    test1$dis <- pmin(test1$Var1, test1$Var2)
    for(i in 1:30) {
      probhit <- append(probhit, mean(test1$Var1 + test1$Var3 >= i))
      probadv <- append(probadv, mean(test1$adv + test1$Var3 >= i))
      probdis <- append(probdis, mean(test1$dis + test1$Var3 >= i))
    }
    plotters <- melt(cbind(probhit, probadv, probdis))
    ggplot(data=plotters,
           aes(x=Var1, y=value, colour=as.factor(Var2))) +
      geom_line(size=1) +
      scale_color_manual(name="Var2", values = c("#2B879C", "#FC9040", "#180B55")) +
      geom_vline(xintercept = input$ac, size=1, linetype="dotted") +
      theme_classic() + 
      labs(title = paste0("You Have the Following Probability of Hitting Baddie"), x="Baddie's Armor Class", y="Probability of Successful Hit") +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)

# It was highly rewarding to figure out how to find every possible combination of dice rolls, create a dataset of those rolls
# and then create a histogram from that data
# Unfortunately this process is highly demanding on processing power, and the sample function is amazingly fast by comparison
# Too bad the sample function is also so much uglier

# I created a variable replicating the set of possible outcomes - if the value is higher than 1 then the code for a list of those outcomes is created
# in string format, to be inserted into code below
#space <- unlist(c(replicate(val$d4, "1:4"),
#                  replicate(val$d6, "1:6"),
#                  replicate(val$d8, "1:8"),
#                  replicate(val$d10, "1:10"),
#                  replicate(val$d12, "1:12"),
#                  replicate(val$d20, "1:20")
#))
# Here I am adding to the string developed above - it adds the code before the substance, the commas separating the die rolls, and closing parentheses
#space <- paste0("rowSums(expand.grid(", paste(space, collapse=", "), "))")
# This code creates an object telling R to look at this string variable as if it is code and run it
#spacegrid <- eval(parse(text = space))


