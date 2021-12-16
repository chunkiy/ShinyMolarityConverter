
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
      textOutput("wrong"),
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
  wrong <- reactiveValues()
  wrong$data <- NULL
  
  # data generation ---------------------------------------------------------

  data <- observeEvent(input$calculate, {
    
    # check for inputs --------------------------------------------------------
    
    req(
      isTruthy(input$substance),
      isTruthy(input$conc))
    
    #print("Event started")

    # load function -----------------------------------------------------------
    
    #print("loading functions")
    
    calc <- function(unit,MW,conc){
      
      if(unit==units[1]){
        concM <- as.numeric(conc)
        result <- (concM*MW)*1000
        x <- data.frame(M=concM,mM=concM*1000,muM=concM*1e6,mugmL=result,gL=result/1000)
        x <- bind_cols(query,x)
        placeholder <<- x
        
      }else if(unit==units[2]){
        concmM <- as.numeric(conc)
        result <- ((concmM/1000)*MW)*1000
        x <- data.frame(M=concmM/1000,mM=concmM,muM=concmM*1000,mugmL=result,gL=result/1000)
        x <- bind_cols(query,x)
        placeholder <<- x
        
      }else if(unit==units[3]){
        concmuM <- as.numeric(conc)
        result <- ((concmuM/1e6)*MW)*1000
        x <- data.frame(M=concmuM/1e6,mM=concmuM/1000,muM=concmuM,mugmL=result,gL=result/1000)
        x <- bind_cols(query,x)
        placeholder <<- x
        
      }else if(unit==units[4]){
        concMg <- as.numeric(conc)
        result <- (concMg/1000)/MW
        x <- data.frame(M=result,mM=result*1000,muM=result*1e6,mugmL=concMg,gL=concMg/1000)
        x <- bind_cols(query,x)
        placeholder <<- x
        
      }else{
        concgL <- as.numeric(conc)
        result <- (concgL)/MW
        x <- data.frame(M=result,mM=result*1000,muM=result*1e6,mugmL=concgL*1000,gL=concgL)
        x <- bind_cols(query,x)
        placeholder <<- x
      }
    }
    calcMulti <- function(name,unit,MW,conc){
      
      if(unit==units[1]){
        concM <- as.numeric(conc)
        result <- (concM*MW)*1000
        x <- data.frame(Substance=name,MW=MW,M=concM,mM=concM*1000,muM=concM*1e6,mugmL=result,gL=result/1000)
        placeholder <<- x
      }else if(unit==units[2]){
        concmM <- as.numeric(conc)
        result <- ((concmM/1000)*MW)*1000
        x <- data.frame(Substance=name,MW=MW,M=concmM/1000,mM=concmM,muM=concmM*1000,mugmL=result,gL=result/1000)
        placeholder <<- x
      }else if(unit==units[3]){
        concmuM <- as.numeric(conc)
        result <- ((concmuM/1e6)*MW)*1000
        x <- data.frame(Substance=name,MW=MW,M=concmuM/1e6,mM=concmuM/1000,muM=concmuM,mugmL=result,gL=result/1000)
        placeholder <<- x
      }else if(unit==units[4]){
        concMg <- as.numeric(conc)
        result <- (concMg/1000)/MW
        x <- data.frame(Substance=name,MW=MW,M=result,mM=result*1000,muM=result*1e6,mugmL=concMg,gL=concMg/1000)
        placeholder <<- x
      }else{
        concgL <- as.numeric(conc)
        result <- (concgL)/MW
        x <- data.frame(Substance=name,MW=MW,M=result,mM=result*1000,muM=result*1e6,mugmL=concgL*1000,gL=concgL)
        placeholder <<- x
      }
    }
    #print("functions loaded")
    
    # pubchem query -----------------------------------------------------------
    
    query <- cir_query(input$substance,"mw")
    query <- unlist(query)
    
    #define case unspecific variables (namely input)
    
    unit <- input$unit
    conc <- input$conc
    
    if (length(query)==1 & !is.na(query) ) { #the normal one as before
      print("nono")
      query <- data.frame(Substance=str_extract(names(query)[1],"[:alpha:]+"),MW=as.numeric(query[1])) #extract the 1 row correctly
      
      #define case unique variables
      MW <- query$MW
      
      #calc the thing as ususal
      calc(unit,MW,conc)
      go <- 1
      
    }else if(length(query)>1){
      names <- names(query)
      MW <- unname(query)
      leng <- length(names)
      query <- data.frame(Substance=names,MW=as.numeric(MW))
      framelist <- list()
      
      for(i in 1:leng){
        calcMulti(names[i],unit,MW[i],conc)
        framelist[[paste(names[i])]] <- placeholder
      }
      placeholder <- bind_rows(framelist)
      go <- 1
      
    }else if(is.na(query)){
      print("nothing found")
      wrong$data <- "No compound found"
      go <- 0
    }
    else{
      wrong$data <- "Something went wrong"
      go <- 0
    }
    
    # addition of data to exisiting frame or new frame ------------------------
  
  if(go==1){
    if(is.null(rv$data)){
      rv$data <- placeholder
    }else{
      rv$data <- bind_rows(rv$data,placeholder)
    }
  }else{
    print("No compound found")
  }
    }
)

  # output rendering --------------------------------------------------------
  
  output$table <- renderTable(
    rv$data,digits=5
  )
  output$wrong <- renderText(
    wrong$data
  )
}

shinyApp(ui = ui, server = server)