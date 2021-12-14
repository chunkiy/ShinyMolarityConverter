
# libraries ---------------------------------------------------------------


library(shiny)
library(tidyverse)
library(webchem)


# ui ----------------------------------------------------------------------

ui <- fluidPage(
  
  #App Title
  titlePanel("Molarity converter"),
  
  # sidebar layout ----------------------------------------------------------

  sidebarLayout(
    
    # sidebarpanel ------------------------------------------------------------

    sidebarPanel(
      fluidRow(
        column(width=12,
               textInput("substance","Name of Substance"),
          
        )
      ),
      fluidRow(
        column(width=4,
               textInput("conc","Concentration")
        ),
        column(width=8,
               selectInput("unit","Select units of input",c("M","mM","mg/L","g/L"))
        )
      ),
      
      fluidRow(
        column(width=4,
          actionButton("calculate","Calculate")
        )
      )
    ),

    # mainPanel ---------------------------------------------------------------
    
    mainPanel(
      textOutput("error"),
      tableOutput("table")
    )
  )
)


# server ------------------------------------------------------------------


server <- function(input, output) {
  
  # variable/reactive values definition -------------------------------------
  
  units <- c("M","mM","mg/L","g/L")
  rv <- reactiveValues()
  rv$data <- NULL

  # data generation ---------------------------------------------------------

  
  data <- observeEvent(input$calculate, {
    
    
    # check for inputs --------------------------------------------------------
    req(
      isTruthy(input$substance),
      isTruthy(input$conc))
    
    print("Event started")

    # pubchem query -----------------------------------------------------------
    
    query <- cir_query(input$substance,"mw")
    query <- unlist(query)
    query <- data.frame(Substance=str_extract(names(query)[1],"[:alpha:]+"),MW=as.numeric(query[1]))
    
    # calculations, based on selectorInput ------------------------------------

    if(input$unit==units[1]){
      concM <- as.numeric(input$conc)
      result <- (concM*query$MW)*1000
      frame <- data.frame(M=concM,mM=concM*1000,mgL=result,gL=result/1000)
      frame <- bind_cols(query,frame)
      
    }else if(input$unit==units[2]){
      concmM <- as.numeric(input$conc)
      result <- ((concmM/1000)*query$MW)*1000
      frame <- data.frame(M=concmM/1000,mM=concmM,mgL=result,gL=result/1000)
      frame <- bind_cols(query,frame)
      
    }else if(input$unit==units[3]){
      concMg <- as.numeric(input$conc)
      result <- (concMg/1000)/query$MW
      frame <- data.frame(M=result,mM=result*1000,mgL=concMg,gL=concMg/1000)
      frame <- bind_cols(query,frame)
      
    }else{
      concgL <- as.numeric(input$conc)
      result <- (concgL)/query$MW
      frame <- data.frame(M=result,mM=result*1000,mgL=concgL*1000,gL=concgL)
      frame <- bind_cols(query,frame)
    }

    # addition of data to exisiting frame or new frame ------------------------

  if(is.null(rv$data)){
    rv$data <- frame
  }else{
    rv$data <- bind_rows(rv$data,frame)
  }
  
}
)

  # output rendering --------------------------------------------------------
  
  output$table <- renderTable(
    rv$data,digits=5
  )
  
}

shinyApp(ui = ui, server = server)