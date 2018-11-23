#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mapproj)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
       
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        map(database= "world", regions  = "India", exact=T, col="grey80", fill=TRUE, projection="gilbert", orientation= c(90,0,90)),
        
        lat <- c(23.30, 28.38),
        
        lon <- c(80, 77.12), # Lon and Lat for two cities Bhopal and Delhi
        
        coord <- mapproject(lon, lat, proj="gilbert", orientation=c(90, 0, 90)),
        
        points(coord, pch=20, cex=1.2, col="red"),
      
         plotOutput("distPlot")
        
      )
   )
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

