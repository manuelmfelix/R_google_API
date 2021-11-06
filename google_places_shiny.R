library(tseries)
library(tidyverse)
library(plyr)
library(shiny)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)
library(data.table)
library(googleway)
library(dplyr)
library(openxlsx)
library(geosphere)
library(readxl)

funcaoPlaces <- function(lugares,maxDist){

key_places <- "google_API_key" # Get the API key on google APIs

i <- 1

while(i<=nrow(lugares)){

latitude <- as.double(lugares[i,1])
longitude <- as.double(lugares[i,2])
local <- lugares[i,3]

df_places <- google_places(search_string = local, 
                           location = c(latitude, longitude),
                           # radius = 100,
                           rankby = "distance",
                           name = local,
                           key = key_places)

Sys.sleep(4)

df_next <- google_places(search_string = local, 
                         location = c(latitude, longitude),
                         # radius = 100,
                         rankby = "distance",
                         name = local,
                         key = key_places,
                         page_token = df_places$next_page_token)

Sys.sleep(4)

if(is.null(df_next$next_page_token)){
  
  teste0 <- as.data.frame(c(df_places$results$name,df_next$results$name))
  colnames(teste0)[1] <- "Name"
  teste1 <- rbind(df_places$results$geometry$location,df_next$results$geometry$location)
  teste2 <- as.data.frame(c(df_places$results$formatted_address,df_next$results$formatted_address))
  colnames(teste2)[1] <- "Address"
  
  
}else{

df_next2 <- google_places(search_string = local, 
                         location = c(latitude, longitude),
                         # radius = 5,
                         rankby = "distance",
                         name = local,
                         key = key_places,
                         page_token = df_next$next_page_token)

teste0 <- as.data.frame(c(df_places$results$name,df_next$results$name,df_next2$results$name))
colnames(teste0)[1] <- "Name"
teste1 <- rbind(df_places$results$geometry$location,df_next$results$geometry$location,df_next2$results$geometry$location)
teste2 <- as.data.frame(c(df_places$results$formatted_address,df_next$results$formatted_address,df_next2$results$formatted_address))
colnames(teste2)[1] <- "Address"

}

df <- cbind(teste0,teste1,teste2)
df <- df[,c(1,3,2,4)] 

distancia <- distm(c(longitude,latitude),df[2:3],distHaversine)

df <- cbind(df,t(distancia))
colnames(df)[5] <- "Dist"

df1 <- df %>% filter(Dist<=maxDist)

if(exists("dfFinal")){
  dfFinal <- rbind(dfFinal,df1)
}else{
  dfFinal <- df1
}

i <- i+1

} # Fim Loop

dfFinal <- dfFinal %>% distinct(Nome,Morada,.keep_all = TRUE)

dfFinal <- dfFinal[order(dfFinal$Dist), ]

return(dfFinal)

} # Função

# df_places$results$types

o <- c("38.7431402","38.7246453","38.7471283")
a <- c("-9.1566963","-9.1723024","-9.1951007")
b <- c("Bar","Establishment","Hospital")
ficheiroTipo <- data.frame(o,a,b)
names(ficheiroTipo) <- c('Latitude','Longitude', 'Tipo')




# UI ----------------------------------------------------------------------------------
ui <- shinyUI(fluidPage(
  
  # tags$head(tags$script(src = "message-handler.js")),
  useShinyalert(),
  useShinyjs(),
  
  titlePanel("App Places"),
  
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
      
      # Input: Select number of rows to display
      radioButtons("disp", "Display",
                   choices = c(Inicio = "head",
                               Total = "all"),
                   selected = "head"),
      
      tags$hr(),
      
      numericInput("distMax","Maximum distance to the search point",1000),
      
      circleButton("help3", "?",size="xs"
      ),
      
      tags$hr(),
      
      span(textOutput("titleSide1"),style="font-size: 16px; font-style: bold"),
      
      span(textOutput("control1"),style="font-size: 16px; font-style: bold"),
      
      actionButton("go", "Ativar"),
      
      tags$hr(),
      
      downloadButton('downloadData', "Download ficheiro final"),
      
      tags$hr(),
      
      span(textOutput("author"),style="font-size: 14px; font-style: bold")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Data file
      
      span(textOutput("title1"),style="font-size: 20px;font-style: bold"),
      DT::dataTableOutput("contents", width = 600),
      tags$hr(),
      span(textOutput("title2"),style="font-size: 20px;font-style: bold"),
      DT::dataTableOutput("contentsFinal", width = 600)
      
    ) # main Panel
  ) # side Bar Layout
) # fluid Page
) # UI

#SERVER ----------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Text Inputs----
  
  output$titleSide1 <- renderText({ "Press to activate" })
  output$title1 <- renderText({ "Input Data" })
  output$title2 <- renderText({ "Output Data" })
  output$author <- renderText({ "by: Manuel Felix" })
  
  observeEvent(input$help1, {
    shinyalert("File format", 
               paste("Type of file: Excel (.xlsx)",
                     "File with only one sheet",
                     "File format: 3 columns (A, B, C):",
                     " - A: Latitude of the point to search",
                     " - B: Longitude of the point to search",
                     " - C: Words to search for (i.e. Restaurant, Hospital, Establishment, Point of Interest)",
                     "For a better understanding, download the example file",
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
  
  
  observeEvent(input$help3, {
    shinyalert("Maximum distance", 
               paste("Maximum distance will eliminate every result which exceeds its value",
                     sep="\n"), 
               type = "info",
               animation = "slide-from-top")
  })
  
  
  # Data Inputs----
  
  df <- reactive({withProgress(message = 'Updating',
                               read_xlsx(input$file1$datapath))
  })

  # Mostrar tempo que demora
  
  timeTaken <- reactive({round(NROW(df())*9+3)})
  
  timeTakenMinutes <- reactive({round(timeTaken()/60)})

  # Tabela do Input
  
  output$contents <- DT::renderDataTable({
    
    req(input$file1)
    
    # Display
    if(input$disp == "head") {
      return(DT::datatable(head(df()), options = list(dom = 't')))
    }
    else {
      return(DT::datatable(df(), options = list(scrollX = TRUE)))
    }
    
  })
  
  # Criar Tabela do Output
  
  # output$control1 <- renderText({ input$distMax })
  
  
  filePlaces <- reactive({withProgress(message = 'Updating',
                                        funcaoPlaces(df(),input$distMax))
  })
  
  
  # Tabela do Output
  
  observeEvent(input$go,{
    output$contentsFinal<- DT::renderDataTable({
        if(input$disp == "head") {
          return(DT::datatable(head(filePlaces()), options = list(dom = 't')))
        }else{
          return(DT::datatable(filePlaces(), options = list(scrollX = TRUE)))
      }
    })
    
    if(timeTaken()<60){
      shinyalert("Please wait while we gather the data",
                 text = paste("Time: ",timeTaken(),"segundos",sep=" "),
                 type = "info",
                 animation = "slide-from-top")
    }else{
      shinyalert("Please wait while we gather the data",
                 text = paste("Time: ",timeTakenMinutes(),"minutos",sep=" "),
                 type = "info",
                 animation = "slide-from-top")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Places_",Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
          write.xlsx(filePlaces(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
