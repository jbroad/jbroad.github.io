library(shiny)

f <- function(x) {
  function() {
    sample(1:x,1)
  }
}

x <- 0

ui <- fluidPage(
  actionButton("d20", "D20"),
  textOutput("text20"),
  actionButton("d12", "D12"),
  textOutput("text12"),
  actionButton("d10", "D10"),
  textOutput("text10"),
  actionButton("d8", "D8"),
  textOutput("text8"),
  actionButton("d6", "D6"),
  textOutput("text6"),
  actionButton("d4", "D4"),
  textOutput("text4"),
  textOutput("total")
)

server <- function(input, output) {

  output$text20 <- renderText({
    if (is.null(input$d20)) return("")
    else {
      text20 <- f(20)()
    }
  })
  output$text12 <- renderText({
    if (is.null(input$d12)) return("")
    else {
      text12 <- f(12)()
    }
  })
  output$text10 <- renderText({
    if (is.null(input$d10)) return("")
    else {
      text10 <- f(10)()
    }
  })
  output$text8 <- renderText({
    if (is.null(input$d8)) return("")
    else {
      text8 <- f(8)()
    }
  })
  output$text6 <- renderText({
    if (is.null(input$d6)) return("")
    else {
      text6 <- f(6)()
    }
  })
  output$text4 <- renderText({
    if (is.null(input$d4)) return("")
    else {
      text4 <- f(4)()
    }
  })
  output$total <- renderText({
    if(is.null(input$d4)) return(x)
    else{
      x <- x + f(4)()
    }
  })
}

shinyApp(ui, server)