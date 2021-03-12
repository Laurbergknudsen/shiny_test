#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# header pane

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("title_"),
    headerPanel("Header"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( "sidebarPanel"
            
           
            ),

        # Show a plot of the generated distribution
        mainPanel("Main Panel" )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
