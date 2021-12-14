library(shiny)
library(tidyverse)
library(webchem)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Molarity converter"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput("substance","Name of Substance"),
      textInput("concM","Concentration in M"),
      textInput("concMu","Concentration in µg/L"),
      actionButton("calculate","Calculate"),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: table ----
      textOutput("error"),
      tableOutput("table")
      
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  rv <- reactiveValues()
  rv$data <- NULL
  text <- reactiveValues()
  text$data <- NULL
  
  
  data <- observeEvent(input$calculate, {
    
    req(
      isTruthy(input$substance),
      isTruthy(input$concM) || isTruthy(input$concMu)
    )
    if(isTruthy(input$concM) & isTruthy(input$concMu)){
      stop <- NULL
      text$data <- "You have supplied to concentration inputs, can not calculate"
      print("Two values supplied, logical error")
    }else{
      stop <- TRUE
      text$data <- NULL
    }
    req(stop)
    print("Event started")
    
    query <- cir_query(input$substance,"mw")
    query <- enframe(query)
    names(query) <- c("Substance","MW")
    query$MW <- as.numeric(query$MW)
    
    if(input$concM!=""){
      concM <- as.numeric(input$concM)
      result <- (concM*query$MW)*1000
      frame <- data.frame(M=concM,mugL=result,gL=result/1000)
      frame <- bind_cols(query,frame)
      
    }else{
      concMu <- as.numeric(input$concMu)
      result <- (concMu/1000)/query$MW
      frame <- data.frame(M=result,mugL=concMu,gL=concMu/1000)
      frame <- bind_cols(query,frame)
    }
    
  if(is.null(rv$data)){
    rv$data <- frame
  }else{
    rv$data <- bind_rows(rv$data,frame)
  }
  
  }
  )
  
  output$table <- renderTable(
    rv$data,digits=5
  )
  output$error <- renderText(
    text$data
  )
  
}

shinyApp(ui = ui, server = server)