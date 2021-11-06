### Code for a shiny App that interacts with the googlemaps api to get periodic screenshots ###

library(tseries)
library(tidyverse)
library(plyr)
library(RSelenium)
library(pingr)
library(readxl)
library(openxlsx)
library(slackr)
library(shiny)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)
library(data.table)
library(webshot)
library(googleway)
library(htmlwidgets)

slackr::slackr_setup(username = "slackr",
                     incoming_webhook_url = "webhook_URL###",
                     token = "Slack_Token###")

# Read input file with coordinates and name of the places

# Open Selenium Driver

funcaoMapsScreenshots <- function(locais,date,sleepTime,fileName,lowerTime,upperTime,freq,freqSleep,freqLower,freqUpper,diasSemana){

  beginTime <- Sys.time()
  
  widgetName <- "temp.html"
  
  key_maps <- "mapsAPIKey###"
  
# All time loop
  
while(Sys.Date()<=date){
    
    i <- 1
    
    inicio <- as.POSIXlt(paste(lowerTime,sep=""),format("%H"),tz = "GMT")
    
    fim <- as.POSIXlt(paste(upperTime,sep=""),format("%H"),tz = "GMT")
    
    
    # Check your locale setting in Linux/UNIX. For english try: Sys.setenv("LANGUAGE"="En")
    
    if(weekdays(Sys.Date()) %in% diasSemana == TRUE){
    
    if(difftime(inicio,as.POSIXlt(format(Sys.time(),"%H:%M:%S"), format="%H:%M:%S",tz = "GMT"))<0 & 
       difftime(as.POSIXlt(format(Sys.time(),"%H:%M:%S"), format="%H:%M:%S",tz = "GMT"),fim)<0){

  # Loop of spots
      
  while(i<=nrow(locais)){
    
    latitude <- as.double(locais[i,1])
    longitude <- as.double(locais[i,2])
    zoom <- as.double(locais[i,3])
    nome <- locais[i,4]

    x <- googleway::google_map(key = key_maps,
                               location = c(latitude, longitude),zoom=zoom) %>% add_traffic()
    Sys.sleep(0.5)
    saveWidget(x, paste(fileName,widgetName,sep="/"))
    Sys.sleep(4)
    webshot(paste("file://",fileName,widgetName,sep="/"),paste(fileName,"/",format(Sys.time(), "%d_%m_%Y_%H_%M"),"_",nome,".png",sep=""),delay = 1.5)

    # Sys.sleep(4)
    
    i <- i+1

  } # Locais
  
  # Send message to slack to know if it running
  
  slackr_msg(paste("Ran screenshots: ",Sys.time(),sep=""), channel="maps-screenshots")
  
    }else{
      slackr_msg(paste("Outside choosen timeframe: ",Sys.time(),sep=""), channel="maps-screenshots")   
    }
    # Periodo Frequente
    
    if(freq=="Sim"){
      
      # slackr_msg(paste("Will ran more frequent time: ",Sys.time(),sep=""), channel="maps-screenshots")
      
      iniciofreq <- as.POSIXlt(paste(freqLower,sep=""),format("%H"),tz = "GMT")
      
      fimfreq <- as.POSIXlt(paste(freqUpper,sep=""),format("%H"),tz = "GMT")
      
      if(difftime(iniciofreq,as.POSIXlt(format(Sys.time(),"%H:%M:%S"), format="%H:%M:%S",tz = "GMT"))<0 & 
         difftime(as.POSIXlt(format(Sys.time(),"%H:%M:%S"), format="%H:%M:%S",tz = "GMT"),fimfreq)<0){
        
        endTime <- Sys.time()
        
        slackr_msg(paste(freqSleep/60," minutes until next screenshots: ",Sys.time(),sep=""), channel="maps-screenshots")
        
        Sys.sleep(freqSleep-as.integer(endTime-beginTime))
        
      }else{ # If out of frequent time
        endTime <- Sys.time()
        slackr_msg(paste(sleepTime/60," minutes until next screenshots: ",Sys.time(),sep=""), channel="maps-screenshots")
        Sys.sleep(sleepTime-as.integer(endTime-beginTime))}
      
    }else{ # If freq == "Nao"
      endTime <- Sys.time()
      slackr_msg(paste(sleepTime/60," minutes until next screenshots: ",Sys.time(),sep=""), channel="maps-screenshots")
      Sys.sleep(sleepTime-as.integer(endTime-beginTime))
      } # Frequent
    
    }else{
      endTime <- Sys.time()
      # If freq == "Nao" && Outside chosen weekdays
      slackr_msg(paste(sleepTime/60," minutes until next try. Day not chosen: ",Sys.time(),sep=""), channel="maps-screenshots")
      Sys.sleep(sleepTime-as.integer(endTime-beginTime)) 
    } # WeekDays
  
  
} # Date

} # Function

o <- c("38.7431402","38.7246453","38.7471283")
a <- c("-9.1566963","-9.1723024","-9.1951007")
b <- c("13.9","14.2","14.1")
c <- c("Teste1","Teste2","Teste3")
ficheiroTipo <- data.frame(o,a,b,c)
names(ficheiroTipo) <- c('Latitude','Longitude', 'Zoom', 'Name')



# UI ----
ui <- shinyUI(fluidPage(
  
  # tags$head(tags$script(src = "message-handler.js")),
  useShinyalert(),
  useShinyjs(),
  
  titlePanel("App Screenshot Maps"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(

      # Hipotese Codigos
      
      fileInput("file1","Upload the file in .xlsx format",
                multiple=FALSE,
                accept = c(".xlsx")),
      
      fluidRow(column(12, align="center",div(circleButton("help1", "?",size="sm"
      ), style = "margin-top:-2em"))),
      
      fluidRow(column(12, align="center",div(downloadButton('downloadTipo', "Download example file")
      ), style = "margin-top:1em")),
      
      # Horizontal line
      tags$hr(),
      
      # File name to put images
      textInput("fileName", h5('Where to store the images (you must use forward slashes "/")'), 
                value = "C:/Users/mfelix/Desktop/images"),
      span(textOutput("textFileName"),style="font-size: 12px; font-style: bold"),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select number of rows to display
      radioButtons("disp", "Display",
                   choices = c(Inicio = "head",
                               Total = "all"),
                   selected = "head"),
      
      # Horizontal line
      tags$hr(),
      
      # Time interval between images
      
      sliderInput("intervalo", h4("Time interval betyween images (minutes)"),
                  min = 30, max = 120, value = 30, step = 30),
      
      tags$hr(),
      
      # Time interval of activity
      
      sliderInput("tempoPesquisa", h4("Time interval for the program to run (hours)"),
                  min = 0, max = 24, value = c(0,24), step = 1),
      span(textOutput("textFileName2"),style="font-size: 12px; font-style: bold"),
      
      tags$hr(),
      
      # Dias da Semana a correr
      
      checkboxGroupInput("checkGroup", label = h3("Days of the week"), 
                         choices = list("monday" = "monday",
                                        "tuesday" = "tuesday",
                                        "wednesday" = "wednesday",
                                        "thursday" = "thursday",
                                        "friday" = "friday",
                                        "saturday" = "saturday",
                                        "sunday" = "sunday")),
      
      
      tags$hr(),

      # Input: Select type of base information
      radioGroupButtons(
        inputId = "frequencia",
        label = "More frequent period?",
        choices = c("Nao", "Sim")
      ),
      span(textOutput("textFileName3"),style="font-size: 12px; font-style: bold"),

      # Horizontal line
      tags$hr(),

      # Time interval between images

      sliderInput("frequenteIntervalo", h5("Frequent Period: time interval between images (minutes)"),
                  min = 10, max = 60, value = 30, step = 10),

      tags$hr(),
      
      # Time interval of activity
      
      sliderInput("frequenteTempoPesquisa", h5("Frequent Period: Time interval for the program to run (hours)"),
                  min = 0, max = 24, value = c(8,10), step = 1),

      tags$hr(),

      
      # Finishing date
      
      dateInput("date", 
                h4("Last day to run")),
      
      tags$hr(),
      


      
      
      span(textOutput("titleSide1"),style="font-size: 16px; font-style: bold"),
      
      actionButton("go", "Go"),
      
      tags$hr(),
      
      span(textOutput("author"),style="font-size: 14px; font-style: bold")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Data file
      
      span(textOutput("title1"),style="font-size: 20px;font-style: bold"),
      DT::dataTableOutput("contents", width = 300),
      
    ) # main Panel
  ) # side Bar Layout
) # fluid Page
) # UI

#SERVER----

server <- function(input, output, session) {
  
  # Hide Buttons
  
  observe({

      shinyjs::hide("frequenteIntervalo")
      shinyjs::hide("frequenteTempoPesquisa")

    if(input$frequencia == "Sim"){
      shinyjs::show("frequenteIntervalo")
      shinyjs::show("frequenteTempoPesquisa")
    }
  })


  # Text Inputs----
  
  output$titleSide1 <- renderText({ "Press to activate" })
  output$title1 <- renderText({ "Input Data" })
  output$author <- renderText({ "by: Manuel Felix" })
  output$textFileName <- renderText({ "Make sure the file exists and that the path is written correctly\nIMPORTANT: You must use forward slashes in the path!!!" })
  output$textFileName2 <- renderText({ "To take images during the whole day keep 0 to 24" })
  output$textFileName3 <- renderText({ "If, in a certain time period, you want more images than the pre-defined, press yes"})
  
  observeEvent(input$help1, {
    shinyalert("Formato do ficheiro", 
               paste("Type of file: Excel (.xlsx)",
                     "The file must only have 1 sheet",
                     "Format of the file: 4 columns (A, B, C, D):",
                     " - A: Latitude (google maps format)",
                     " - B: Longitude (google maps format)",
                     " - C: Zoom",
                     " - D: Name",
                     "If you have any doubts, download the example file",
                     "Max nº of lines: 2 lines per minute (ex. period de 30min, max = 60 lines)",
                     sep="\n"), 
               type = "info",
               animation = "slide-from-top")
  })
  
  output$downloadTipo <- downloadHandler(
    filename = function() {
      paste("FicheiroTipo",".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(ficheiroTipo, file)
    }
  )
  
  
  # Data Inputs----
  
  df <- reactive({withProgress(message = 'Updating',
                               read.xlsx(input$file1$datapath))
  })

  end <-reactive({withProgress(message = 'Updating',as.Date(input$date, format="%Y-%m-%d"))
  })
  
  
  # Intervalo entre imagens
  
  intervalo <- reactive({strtoi(input$intervalo)})

  sleepTime <- reactive({round(intervalo()*60)})
  
  
  # Intervalo entre imagens do periodo frequente
  
  intervaloFreq <- reactive({strtoi(input$frequenteIntervalo)})
  
  sleepTimeFreq <- reactive({round(intervaloFreq()*60)})
  
  
  # Control Texts----
  
  # output$control1 <- renderText({ input$checkGroup })
  # output$control2 <- renderText({ sleepTimeFreq() })
  
  
  # Tabela do Input
  
  output$contents <- DT::renderDataTable({
    
    req(input$file1)
    
    # Display
    if(input$disp == "head") {
      return(DT::datatable(head(df()), options = list(dom = 't')))
    }
    else {
      return(df())
    }
    
  })
  
  # Correr o coigo

  observeEvent(input$go,{
    
    shinyalert("Dados a ser recolhidos. Manter esta janela aberta antes que a recolha se encontrar concluida!",
               text = "Now kick back, relax, check the images being created on the folder and follow the updates on your slack channel",
               type = "info",
               animation = "slide-from-top")
    
    funcaoMapsScreenshots(df(),end(),sleepTime(),input$fileName,
                          input$tempoPesquisa[1],input$tempoPesquisa[2]
                          ,
                          input$frequencia,sleepTimeFreq(),
                          input$frequenteTempoPesquisa[1],input$frequenteTempoPesquisa[2],
                          input$checkGroup
                          )

  })
  
}

shinyApp(ui = ui, server = server)
