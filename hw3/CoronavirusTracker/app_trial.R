#
# This is a Shiny web application. Developed by Burson Tang, UID: 305068045
library(shiny)

# Source helpers ----
source("helpers.R")

# Define UI for application in navbarPage ----------
ui <- navbarPage(
    title = "Conronavirus Visualization App",
    tabPanel("China",
             
             ## Plot the distribution map
             sidebarLayout(
                 
                 sidebarPanel(
                     titlePanel("Coronavirus Case Distribution Map"),

                     dateInput("date", label = "Date to Display",
                               # use the day before today cause time dif
                               value = as.character(Sys.Date()-days(1)),
                               format = "yyyy-mm-dd"
                               ),

                     selectInput("Case", label = "Case to display",
                                 choices = c("Confirmed" = "confirmed",
                                             "Death" = "death",
                                             "Recovered" = "recovered")
                                 ),
                     # change the height of the sidebarpanel
                     style = "height: 450px"
                 ),
                 mainPanel(
                     wellPanel(plotOutput("ChinaMap", height = "440px"),
                               # change the padding of the wellPanel, also
                               # make the height consistent with sidebarpanel
                               style = "padding: 5px;height: 450px")
                 )
             ),
             
             # add separating space line here maybe?
             
             ## Plot the time series of Coronavirus
             wellPanel(
             titlePanel("Coronavirus Case Timeseries"),
             checkboxGroupInput("TS_Case", "choose cases to plot",
                                choices = c("Confirmed" = "confirmed",
                                            "Death" = "death",
                                            "Recovered" = "recovered"),
                                selected = c("Confirmed" = "confirmed",
                                             "Death" = "death",
                                             "Recovered" = "recovered"),
                                inline = TRUE
                                ),
             # make the space height as small as 5px, shrink space gray area
             style = "padding: 5px",
             ),
             
             # Use columns to plot two figures: cumulative, and daily increase
             # plotOutput("ChinaTS_inc", width = "75%")
             fluidRow(
             column(6, plotOutput("ChinaTS_cum", width = "75%")),
             column(6, plotOutput("ChinaTS_inc", width = "75%"))
             ),
             
             fluidRow(
                 column(12,h2("The Data table for above time series")),
                 column(4, selectInput("Date_T", "Date:",
                                       c("All", 
                                         unique(as.character(ncov_tbl$Date))
                                         )
                                       )
                        ),
                 column(4, selectInput("Province_T", "Province/Region:",
                                       c("All",
                                         unique(as.character(
                                             ncov_tbl$`Province/State`))
                                         )
                                       )
                        ),
                 column(4, selectInput("Case_T", "Case:",
                                       c("All", 
                                         unique(as.character(ncov_tbl$Case))
                                         )
                                       )
                        ),
                 wellPanel(DT::dataTableOutput("table_CH"))

                     )
             ),
    
    tabPanel("World")
)


    # tabPanel(title = "Trial_sidebarLayout", # --------------
    #          titlePanel("The distriubtion of Coronavirus"),
    #              sidebarPanel(
    #                  helpText("something"),
    #                  dateInput("date",label = "Date to Display",
    #                            value = "2020-02-18",
    #                            format = "yyyy-mm-dd"),
    # 
    #                  selectInput("Case", label = "Date to display",
    #                              choices = c("Confirmed" = "confirmed",
    #                                          "Death" = "death",
    #                                          "Recovered" = "recovered"))
    #              ),
    #              mainPanel(helpText("HELPPPPPP!!!!!!!!!!!"),plotOutput("ChinaMap"))
    # ),
    # 
    # tabPanel("Trial_sidebarLayout", # --------------
    #          titlePanel("The distriubtion of Coronavirus"),
    #          sidebarPanel(
    #              helpText("something"),
    #              dateInput("date",label = "Date to Display",
    #                        value = "2020-02-18",
    #                        format = "yyyy-mm-dd"),
    # 
    #              selectInput("Case", label = "Date to display",
    #                          choices = c("Confirmed" = "confirmed",
    #                                      "Death" = "death",
    #                                      "Recovered" = "recovered"))
    #          ),
    #          mainPanel(helpText("HELPPPPPP!!!!!!!!!!!"),plotOutput("ChinaMap"))
    # )
    # )
   
    # tabPanel(title = "Trial_1fluidRow", # --------------
    #          titlePanel("The distriubtion of Coronavirus"),
    #          fluidRow(column(2, wellPanel(
    #              dateInput("date",
    #                        label = "Date to Display",
    #                        value = "2020-02-18",
    #                        format = "yyyy-mm-dd"),
    # 
    #              selectInput("Case", label = "Date to display",
    #                          choices = c("Confirmed" = "confirmed",
    #                          "Death" = "death",
    #                          "Recovered" = "recovered"))
    #          )),
    #          column(width = 10,
    #                 wellPanel(plotOutput("ChinaMap", width = "100%")))),
    # 
    #          ),
    # 

# tabPanel(title = "China", # --------------
#          titlePanel("The distriubtion of Coronavirus"),
#          fluidRow(column(2, wellPanel(
# 
#                               dateInput('date',
#                                         label = 'Date to display',
#                                         value = "2020-02-18",
#                                         format = "yyyy-mm-dd"),
# 
# 
#                               selectInput('Case', label = "Case to display for the map",
#                                           choices = c("Confirmed" = "confirmed",
#                                                       "Death" = "death",
#                                                       "Recovered" = "recovered"))
#                               # ,tableOutput("Case")
# 
#                           )),
#                           # # For Debug
#                           # textOutput("result"),
#                           column(width = 10, wellPanel(plotOutput("ChinaMap", width = "100%")))
#                           ),
# 
#                  h2("Coronavirus Timeseries in China"),
#                  checkboxGroupInput("TS_Case", "choose cases to plot",
#                                     choices = c("Confirmed" = "confirmed",
#                                                    "Death" = "death",
#                                                    "Recovered" = "recovered"),
#                                     selected = c("Confirmed" = "confirmed",
#                                                            "Death" = "death",
#                                                            "Recovered" = "recovered")),
# 
#                  wellPanel(plotOutput("ChinaTS", width = "100%")),
# 
#                  h2("The Data table for above time series"),
#                  # wellPanel(tableOutput("ChinaTbl")),
# 
#                  fluidRow(
#                      column(4,
#                             selectInput("Date_T",
#                                         "Date:",
#                                         c("All",
#                                           unique(as.character(ncov_tbl$Date))))
#                      ),
#                      column(4,
#                             selectInput("Province_T",
#                                         "Province/Region:",
#                                         c("All",
#                                           unique(as.character(ncov_tbl$`Province/State`))))
#                      ),
#                      column(4,
#                             selectInput("Case_T",
#                                         "Case:",
#                                         c("All",
#                                           unique(as.character(ncov_tbl$Case))))
#                      )
#                  ),
#                  # Create a new row for the table.
#                  wellPanel(DT::dataTableOutput("table_CH"))
# 
#          ),
# 
# tabPanel(title = "World",
#     titlePanel("Map and Trend of Coronavirus"),
#     fluidRow(column(3, wellPanel(
# 
#         dateInput('date',
#                   label = 'Date to display',
#                   value = Sys.Date() ),
# 
# 
#         selectInput('Case', label = "Case to display for the map",
#                     choices = c("Confirmed" = "Confirmed",
#                                 "Death" = "Death",
#                                 "Recovered" = "Recovered")),
#         tableOutput("Case")
# 
#     )),
# 
#     column(width = 8, offset = 2, plotOutput("distPlot"))
#     )
# )
# )

# Define UI for application via fluidPage -------------------------
# ui <- fluidPage(
#     # Application title
#     titlePanel("Map and Trend of Coronavirus"),
#     fluidRow(column(2, wellPanel(
#                  
#                  dateInput('date',
#                            label = 'Date to display',
#                            value = "2020-02-18",
#                            format = "yyyy-mm-dd"),
#                  
#                  
#                  selectInput('Case', label = "Case to display for the map",
#                              choices = c("Confirmed" = "confirmed",
#                                          "Death" = "death",
#                                          "Recovered" = "recovered"))
#                  # ,tableOutput("Case")
#                  
#              )),
#              # # For Debug
#              # textOutput("result"),
#              
#              # column(8,mainPanel(plotOutput("ChinaMap")))
#              # column(width = 8, offset = 2, mainPanel(plotOutput("ChinaMap")))
#              # column(width = 10, mainPanel(plotOutput("ChinaMap")))
#              column(width = 10, wellPanel(plotOutput("ChinaMap", width = "100%")))
#              ),
#     
#     h2("Coronavirus Timeseries in China"),
#     checkboxGroupInput("TS_Case", "choose cases to plot",
#                        choices = c("Confirmed" = "confirmed",
#                                       "Death" = "death",
#                                       "Recovered" = "recovered"),
#                        selected = c("Confirmed" = "confirmed",
#                                               "Death" = "death",
#                                               "Recovered" = "recovered")),
#                        # selected = c("confirmed",
#                        #              "death",
#                        #              "recovered"),
# 
#     wellPanel(plotOutput("ChinaTS", width = "100%")),
#     
#     h2("The Data table for above time series"),
#     # wellPanel(tableOutput("ChinaTbl")),
#     
#     fluidRow(
#         column(4,
#                selectInput("Date_T",
#                            "Date:",
#                            c("All",
#                              unique(as.character(ncov_tbl$Date))))
#         ),
#         column(4,
#                selectInput("Province_T",
#                            "Province/Region:",
#                            c("All",
#                              unique(as.character(ncov_tbl$`Province/State`))))
#         ),
#         column(4,
#                selectInput("Case_T",
#                            "Case:",
#                            c("All",
#                              unique(as.character(ncov_tbl$Case))))
#         )
#     ),
#     # Create a new row for the table.
#     wellPanel(DT::dataTableOutput("table_CH"))
#     
#     )
    

# Define server logic for plots and tables ------------
server <- function(input, output) {
    
    plotdate <- "2020-02-18"
    case <- "confirmed"

    # # Distribution map in china
    output$ChinaMap <- renderPlot({
        ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau", "Hong Kong", "Taiwan")) %>%
            # filter(Date == plotdate, Case == case) %>%
            filter(Date == input$date, 
                   Case == input$Case) %>%
            group_by(`Province/State`) %>%
            top_n(1, Date) %>% # take the latest count on that date
            right_join(chn_prov, by = c("Province/State" = "NAME_ENG")) %>%
            ggplot() +
            geom_sf(mapping = aes(fill = Count, geometry = geometry)) +
            # scale_fill_gradient(low = "white",
            #                     high = "red",
            #                     trans = "log10",
            #                     limits = c(1, 50000),
            #                     breaks = c(1, 10, 100, 1000, 10000),
            #                     name = "") +
            scale_fill_gradientn(colors = wes_palette("Zissou1", 100, type = "continuous"),
                                 trans = "log10") + # can we find a better palette?
            # #scale_fill_brewer(palette = "Dark2") +
            theme_bw() +
            theme(text = element_text(size=20))+
            labs(title = str_c(input$Case, " cases"), subtitle = input$date)
    })
    
    ## time series of different cases in China
    # cumulative count
    output$ChinaTS_cum <- renderPlot({
        ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau",
                                           "Hong Kong", "Taiwan")) %>%
            # group_by(input$date, input$Case) %>%
            group_by(Date, Case) %>%
            summarise(total_count = sum(Count)) %>%
            filter(Case %in% c(input$TS_Case)) %>%
            # filter(Case %in% c(Confirmed, Death, Recovered)) %>%
            # print()
            ggplot() +
            geom_line(mapping = aes(x = Date, y = total_count, color = Case),
                      size = 2) +
            
            # Old method
            # scale_color_manual(values = c("red", "black", "green")) +
            
            # Assign color to the specific variable instead
            scale_color_manual(values = c("confirmed" = "red", 
                                          "death" = "black", 
                                          "recovered" = "green")) +
            # get rid of scientific notation
            scale_y_continuous(labels = comma)+  
            labs(y = "Count") +
            theme_bw()+
            theme(text = element_text(size=20))+
            labs(title = "Time Series of Cumulative Count")
    })
    
    # daily increased count for different cases
    output$ChinaTS_inc <- renderPlot({
        # Calcuate the daily increment
        b <- ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau", 
                                           "Hong Kong", "Taiwan")) %>%
            # group_by(input$date, input$Case) %>%
            group_by(Date, Case) %>%
            summarise(total_count = sum(Count)) %>%
            mutate(increment = NA)
        # b <- c %>% mutate(trial = ifelse(Date > "2020-01-22" | Date < Sys.Date(),  ))
        b$increment[4:(nrow(b)-3)] = b$total_count[4:nrow(b)] - 
            b$total_count[1:(nrow(b)-3)]
        
        # Plot
        b %>%
            filter(Case %in% c(input$TS_Case)) %>%
            ggplot() +
            geom_line(mapping = aes(x = Date, y = increment, color = Case), 
                      size = 2) +

            # Old method
            # scale_color_manual(values = c("red", "black", "green")) +

            # Assign color to the specific variable instead
            scale_color_manual(values = c("confirmed" = "red",
                                          "death" = "black",
                                          "recovered" = "green")) +
            # get rid of scientific notation
            scale_y_continuous(labels = comma)+  #, format(total_count, scientific = F)) +
            labs(y = "Count") +
            theme_bw()+
            theme(text = element_text(size=20))+
            labs(title = "Time Series of Daily Increment")
    })
    
    # output$ChinaTbl <- renderTable({
    #     ncov_ch_tbl[, c(1,5,6,7)]
    # })
    
    output$table_CH <- DT::renderDataTable(DT::datatable({
        data <- ncov_ch_tbl[, c(1,5,6,7)]
        if (input$Date_T != "All") {
            data <- data[data$Date == input$Date_T,]
        }
        if (input$Province_T != "All") {
            data <- data[data$`Province/State` == input$Province_T,]
        }
        if (input$Case_T != "All") {
            data <- data[data$Case == input$Case_T,]
        }
        data
        }))
    
    # # For Debug
    # output$result <- renderText({
    #     paste("You chose", class(input$TS_Case))})

}

# Run the application 
shinyApp(ui = ui, server = server)

