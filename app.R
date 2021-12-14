
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
               selectInput("unit","Select units of input",c("M","muM","mug/L","g/L"))
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
  
  units <- c("M","muM","mug/L","g/L")
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
    query <- enframe(query)
    names(query) <- c("Substance","MW")
    query$MW <- as.numeric(query$MW)
    
    # calculations, based on selectorInput ------------------------------------

    if(input$unit==units[1]){
      concM <- as.numeric(input$conc)
      result <- (concM*query$MW)*1000
      frame <- data.frame(M=concM,muM=concM*1000,mugL=result,gL=result/1000)
      frame <- bind_cols(query,frame)
      
    }else if(input$unit==units[2]){
      concmuM <- as.numeric(input$conc)
      result <- ((concmuM/1000)*query$MW)*1000
      frame <- data.frame(M=concmuM/1000,muM=concmuM,mugL=result,gL=result/1000)
      frame <- bind_cols(query,frame)
      
    }else if(input$unit==units[3]){
      concMu <- as.numeric(input$conc)
      result <- (concMu/1000)/query$MW
      frame <- data.frame(M=result,muM=result*1000,mugL=concMu,gL=concMu/1000)
      frame <- bind_cols(query,frame)
      
    }else{
      concgL <- as.numeric(input$conc)
      result <- (concgL)/query$MW
      frame <- data.frame(M=result,muM=result*1000,mugL=concgL*1000,gL=concgL)
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