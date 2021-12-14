
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
               selectInput("unit","Select units of input",c("M","mM","muM","mug/mL","g/L"))
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
  
  units <- c("M","mM","muM","mug/mL","g/L")
  
  rv <- reactiveValues()
  rv$data <- NULL
  

  # function set up ---------------------------------------------------------

  calc <- function(unit,MW,conc){
    
    if(unit==units[1]){
      concM <- as.numeric(conc)
      result <- (concM*MW)*1000
      x <- data.frame(M=concM,mM=concM*1000,muM=concM*1e6,mugmL=result,gL=result/1000)
      x <- bind_cols(query,frame)
      frame <<- x
      
    }else if(unit==units[2]){
      concmM <- as.numeric(conc)
      result <- ((concmM/1000)*MW)*1000
      x <- data.frame(M=concmM/1000,mM=concmM,muM=concmM*1000,mugmL=result,gL=result/1000)
      x <- bind_cols(query,frame)
      frame <<- x
      
    }else if(unit==units[3]){
      concmuM <- as.numeric(conc)
      result <- ((concmuM/1e6)*MW)*1000
      x <- data.frame(M=concmuM/1e6,mM=concmuM/1000,muM=concmuM,mugmL=result,gL=result/1000)
      x <- bind_cols(query,frame)
      frame <<- x
      
    }else if(unit==units[4]){
      concMg <- as.numeric(conc)
      result <- (concMg/1000)/MW
      x <- data.frame(M=result,mM=result*1000,muM=result*1e6,mugmL=concMg,gL=concMg/1000)
      x <- bind_cols(query,frame)
      frame <<- x
      
    }else{
      concgL <- as.numeric(conc)
      result <- (concgL)/MW
      x <- data.frame(M=result,mM=result*1000,muM=result*1e6,mugmL=concgL*1000,gL=concgL)
      x <- bind_cols(query,frame)
      frame <<- x
    }
  }

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
    print("query done")
    print(query)
    
    # calculations, based on selectorInput ------------------------------------

    unit <- input$unit
    print(unit)
    MW <- query$MW
    print(MW)
    conc <- input$conc
    print(conc)

    print("calc started")
    calc(unit,MW,conc)
    print("calc done")

    # addition of data to exisiting frame or new frame ------------------------
    colnames(frame) <- c("Substance","MW",units)
    
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