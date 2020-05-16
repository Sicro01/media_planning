library(shiny)

ui <- fluidPage(
  "Total:",
  textOutput("total", inline = TRUE),
  actionButton("add1", "Add 1"),
  actionButton("add5", "Add 5")
)

server <- function(input, output, session) {
  values <- reactiveValues(total = 0)
  
  observeEvent(input$add1, {
    values$total <- values$total + 1
  })
  observeEvent(input$add5, {
    values$total <- values$total + 5
  })
  output$total <- renderText({
    values$total
  })
}

shinyApp(ui = ui, server = server)