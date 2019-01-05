library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "choose a number",
              value = 25,min = 1, max = 100),
  textInput(inputId = "title",
            label="write a title",
            value  ="Histogram"),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input,output) {
  output$stats <- renderPrint( {
    stats(rnorm(input$num))
    
  })
  
 }

shinyApp(ui=ui,server=server)