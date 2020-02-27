#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # # Application title
    # titlePanel("Map and Trend of Coronavirus"),
    # 
    # wellPanel(
    #     dateInput('date',
    #               label = 'Date input: yyyy-mm-dd',
    #               value = Sys.Date()
    #     ))
    
    column(4, wellPanel(
        
        dateInput('date',
                  label = 'Date to display',
                  value = Sys.Date() ),
        
        dateInput('date2',
                  label = paste('Date input 2: string for starting value,',
                                'dd/mm/yy format, locale ja, range limited,',
                                'week starts on day 1 (Monday)'),
                  value = as.character(Sys.Date()),
                  min = Sys.Date() - 5, max = Sys.Date() + 5,
                  format = "dd/mm/yy",
                  startview = 'year', language = 'zh-TW', weekstart = 1),
        
        selectInput('Case', label = "Case to display for the map", 
                    choices = c("Confirmed" = "Confirmed", 
                                "Death" = "Death", 
                                "Recovered" = "Recovered")),
        tableOutput("case")

        ) )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
