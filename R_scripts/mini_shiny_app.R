library(shiny)

ui <- fluidPage(
numericInput("nStemRemaining", "N old stem remaining", value = 61141 ),
sliderInput("rate", "Rate (stem/day) as a team", min= 0, max = 1500, value = 800 ),

p("You'll need this number of work days:"),
textOutput("nWorkDay"), #, "Number of work days", min= 0, max = 120, value = 71 ),

p("You'll finish the census on this date (not counting rainy days):"),
textOutput("LastDate")

)

server <- function(input, output, session) {
  
 observe({
   

   today <- as.Date(Sys.time())
   
   nDaysNeeded <- ceiling(input$nStemRemaining/input$rate)
   
   dates <- today + as.difftime(1:2000, units = "days")
   dates_days <- weekdays(dates)
  
   dates_usable <- dates[!dates_days%in% c("Saturday", "Sunday") & !dates %in% as.Date(c("2023-06-19", "2023-06-20", "2023-06-21", # juneteenth + SERC field trip
                                                                                 "2023-07-03", "2023-07-04", "2023-07-05", # 4th of july and ??
                                                                                 "2023-09-04"))]
   


   # browser()
   output$nWorkDay <- renderText(nDaysNeeded)
   output$LastDate <- renderText(as.character(dates_usable)[nDaysNeeded])
     })
  
}

shinyApp(ui, server)
