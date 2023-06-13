library(shiny)

ui <- fluidPage(
numericInput("nStemRemaining", "N old stem remaining", value = 61141 ),
sliderInput("rate", "Rate (stem/day) as a team", min= 0, max = 1500, value = 800 ),

p("You'll need this number of work days:"),
textOutput("nWorkDay"), #, "Number of work days", min= 0, max = 120, value = 71 ),

p("You'll finish the census on htis date (this is counting the number of Mondays-Fridays, ignoring Holidays, field trip days and rainy days):"),
textOutput("LastDate")

)

server <- function(input, output, session) {
  
 observe({
   

   today <- as.Date(Sys.time())
   
   nDaysNeeded <- ceiling(input$nStemRemaining/input$rate)
   
   dates <- today + as.difftime(1:2000, units = "days")
   dates_days <- weekdays(dates)
  

   # browser()
   output$nWorkDay <- renderText(nDaysNeeded)
   output$LastDate <- renderText(as.character(dates)[! dates_days%in% c("Saturday", "Sunday")][nDaysNeeded])
     })
  
}

shinyApp(ui, server)
