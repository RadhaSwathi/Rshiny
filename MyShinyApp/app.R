#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(sliderInput(inputId = "nums",
                            label = "Choose number of iteration",
                            min = 1,
                            max = 50,
                            value = 30),
                plotOutput("rollers")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$rollers<-renderPlot(
    {
      roll4<-function(die=1:6){
        mydierolling4<- sample(die,size=1,replace = T,prob = c(1/8,1/8,1/8,1/8,1/8,3/8))
      }
      roll4()
      baisedrolls<-replicate(as.numeric(input$nums),roll4())
      print(roll4())
      
      library(ggplot2)
      print(baisedrolls)
      qplot(baisedrolls,binwidth=1)
    }
  )  
} 

   
# Run the application 
shinyApp(ui = ui, server = server)



