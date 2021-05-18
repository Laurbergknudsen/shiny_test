#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# header pane

library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(DT)
library(bslib)
library(thematic)
library(shinyWidgets)
library(googlesheets4)
library(shinyTime)

test = data.frame(chaufførid = as.integer(),
                  typedag = as.character(),
                  rute = as.character(),
                  temperatur = as.integer(),
                  batteri = as.double(),
                  tid = as.numeric(),
                  lagToStart = as.numeric(),
                  check.names = FALSE)

passat <- read_excel("passat.xlsx")

minPrice <- floor(min(passat$price)/1000)*1000
maxPrice <- ceiling(max(passat$price)/1000)*1000

gs4_auth(
  # cache = ".secrets",
  # email = "*"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("darkly"),
  mainPanel(
    DTOutput("table")
  ),

    # Application title
    titlePanel(
        title = div(),
               windowTitle = "EaDania"),
    headerPanel("Passat Web app"),
    div(a("EA-Dania", href = "https://www.eadania.dk", target="_blank")),
    br(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "left",
        sidebarPanel( "",
                      
                      selectInput("xaxis", label = "Vælg x-akse:", 
                                  choices = names(passat), selected = "km_per_liter", 
                                  TRUE, multiple = FALSE),
                      selectInput("yaxis", label = "Vælg y-akse:", 
                                  choices = names(passat), multiple = FALSE),
                      helpText(""),
                      selectInput("data_input", label = "Vælg datasæt:", choices = c("mtcars","faithful","iris")),
                      sliderInput(inputId = "slider_pris", 
                                  label = "Vælg pris", min = minPrice,
                                  max = maxPrice,
                                  value = c(minPrice,maxPrice), step = 1000,
                                  round = TRUE, ticks = TRUE),
                      
                      hr(),
                      helpText("Input data"),
                      selectizeInput(inputId = "chaufførid", label = "Chauffør Id",
                                     choices = c(seq(5000,5099)),
                                     options = list(
                                       placeholder = "Vælg dit ID",
                                       onInitialize = I('function() { this.setValue(""); }')
                                     )
                      ),
                      selectizeInput(inputId = "typedag", label = "Type dag",
                                     choices = c("Hverdag","Lørdag","Søndag"),
                                     options = list(
                                       placeholder = "Vælg dag",
                                       onInitialize = I('function() { this.setValue(""); }')
                                     )
                      ),
                      radioGroupButtons(inputId = "rute",label = "Vælg rute",
                                         choices = c("13 - Frydenlund", "13 - Holme Parkvej")),
                      sliderInput(inputId = "temperatur", label = "Temperatur",
                                  min = -30, max = 40, value = 0, step = 2),
                      numericInputIcon(inputId = "batteri", label = "Batteri",
                                   min = 0, max = 100, step = 0.5, value = NULL,
                                   help_text = "værdi fra 0-100", icon = icon("percent")
                      ),
                      timeInput("tid", label = "Køreplanens starttidspunkt",
                                seconds = F, value = Sys.time()),
                      actionButton("tilføj", "Tilføj observation")
                      
                      
                      
                      
                      
                      
            ),

        # Show a plot of the generated distribution
        mainPanel("",
                  tabsetPanel(id = "tabs",
                              tabPanel("Kørselsinformation",
                                       br(),
                                       actionButton("indlæs", "Indlæs data"),
                                       shiny::textOutput("forbrug"),
                                       br(),
                                       DTOutput("busTable")),
                              tabPanel("Bil Priser",
                                       dataTableOutput("price_table")),
                              tabPanel("Bil specs",
                                       selectizeInput(inputId = "add_to_table",
                                                      label = "Tilføj til tabel",
                                                      choices = passat$car,
                                                      multiple = FALSE),
                                       dataTableOutput("usertable")),
                              tabPanel("Plot",
                                       plotOutput("plot1",width = "100%")),
                              tabPanel("Tabel",
                                       tableOutput("tabel1")))
                              
                  )
    )
)

bustable_data <- test



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  busTableValues <- reactiveValues(df = test)
  
  
  observeEvent(input$tilføj, {
    
    show_alert(title = "Observation tilføjet",
               type = "success")
    
    data <- busTableValues$ny
    
    newRow <- data.frame(chaufførid = input$chaufførid,
                         typedag = input$typedag,
                         rute = input$rute,
                         temperatur = input$temperatur,
                         batteri = input$batteri,
                         tid = format(input$tid, "%H:%M"),
                         lagToStart = round((round(as.numeric(as.POSIXct(input$tid) - Sys.time()),0) *60) + 
                           ((as.numeric(as.POSIXct(input$tid) - Sys.time()))%%1),0),
                         check.names = FALSE)
    
    
    data <- rbind(data, newRow)
    
    
    busTableValues$ny <- data
    
    
    sheet_append(Sys.getenv("google_sheet1"), newRow, sheet = 1)
    
  })
  
  df <- eventReactive(input$indlæs, {
    
    read_sheet(paste0("https://docs.google.com/spreadsheets/",Sys.getenv("google_sheet1"),"/edit#gid=0"), range = "driver")
    
  })
  
  output$busTable <- renderDataTable({
    
    datatable(df(), style = "bootstrap")
    
  })
  
  
  
  
  
  

        output$plot1 <- renderPlot({
        ggplot(passat,aes_string(x = as.name(input$xaxis), y = as.name(input$yaxis))) +
            geom_point(color = "cyan") + 
            geom_point(data = df_biler(), color = "white", size = 5, shape = 1) +
            theme(panel.background = element_rect(color = "grey30", fill = "grey30"))
    })
    
    getdata <- reactive({
        get(input$data_input, "package:datasets")
    })
    
    output$tabel1 <-  renderTable({head(getdata(),5)})
    
   df_add_car <- reactive({
     
     req(input$add_to_table)
     
     df_add_car <-  passat %>% 
       filter(car == input$add_to_table)
     
   })
    
    output$usertable <- renderDataTable({
      datatable(df_add_car(), style = "bootstrap")
    })
    

    df_biler <- reactive({

      req(input$slider_pris)

      df_biler <-  passat %>%
        filter(price >= input$slider_pris[1], price <= input$slider_pris[2])

    })

    output$price_table <- renderDataTable({
     datatable(df_biler(), style = "bootstrap")
      
      
    })


    
}

# Run the application 
shinyApp(ui = ui, server = server)
