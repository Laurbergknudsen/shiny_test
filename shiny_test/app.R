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

passat <- read_excel("passat.xlsx")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("darkly"),

    # Application title
    titlePanel(title = div(img(height = 100, src = "logo.jpg")), windowTitle = "EaDania"),
    headerPanel("Passat Web app"),
    div(a("EA-Dania", href = "https://www.eadania.dk", target="_blank")),
    br(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "left",
        sidebarPanel( "",
                      selectInput("xaxis", label = "Vælg x-akse", 
                                  choices = names(passat), selected = "km_per_liter", 
                                  TRUE, multiple = FALSE),
                      selectInput("yaxis", label = "Vælg y-akse", 
                                  choices = names(passat), multiple = FALSE),
                      helpText("hjælpe tekst"),
                      selectInput("data_input", label = "Vælg datasæt", choices = c("mtcars","faithful","iris"))
            
           
            ),

        # Show a plot of the generated distribution
        mainPanel("",
                  tabsetPanel(id = "tabs",
                              tabPanel("Plot", plotOutput("plot1",width = "100%")),
                              tabPanel("Tabel", tableOutput("tabel1")))
                              
                  )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$plot1 <- renderPlot({
        ggplot(passat,aes_string(x = as.name(input$xaxis), y = as.name(input$yaxis))) +
            geom_point()
    })
    
    getdata <- reactive({
        get(input$data_input, "package:datasets")
    })
    
    output$tabel1 <-  renderTable({head(getdata())})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
