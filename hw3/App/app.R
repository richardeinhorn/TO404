library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices", windowTitle = 'Richard\'s RShiny'),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      uiOutput("productOutput"),
      uiOutput("subproductOutput"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(),
      h2(textOutput("no.results")),
      br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  
  output$productOutput <- renderUI({
    radioButtons("typeInput", "Product type",
                 choices = sort(unique(bcl$Type)),
                 selected = "BEER")
  })
  
  output$subproductOutput <- renderUI({
    if (is.null(input$typeInput)) {
      return(NULL)
    }
    
    radioButtons("subtypeInput", "Subproduct type",
                 choices = sort(unique(bcl[bcl$Type == input$typeInput,]$Subtype)))
  })
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput) || is.null(input$subtypeInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Subtype == input$subtypeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })

  output$no.results <- renderText({
    paste("Number of results:", ifelse(is.null(input$countryInput), 0, nrow(filtered())))
    })
}

shinyApp(ui = ui, server = server)