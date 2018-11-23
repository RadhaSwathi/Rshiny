#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(rsconnect)
library(ggmap)
library(ggplot2)
library(shinyjs)
library(readxl)
library(googleway)
library(V8)
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
if (interactive()) {
  ui <- fluidPage(theme = "bootstrap.css",
    # App title ----
    #titlePanel("L&T Constructions"),
    headerPanel(
      img(src = "/logo2.jpg", height = 50, width = 200)
      ),
      ### the rest of your code
    
    # Sidebar layout with a input and output definitions ----
    useShinyjs(),
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      # Application title
      headerPanel(
        img(src = "/logo2.jpg", height = 150, width = 200,align = "left")
        
      ),
      # Sidebar with a slider input for number of bins 
        sidebarPanel(
          
          selectInput(inputId = "Method",
                      label = 'Construction type:',
                      choices = c("ESR", "WTP","CWR","CWPH","Intake")),
          
          selectInput(inputId = "State",
                      label = 'State:',
                      choices = c("Madhya Pradesh", "Maharashtra","Odisha","Rajasthan")),
          # Input: Selector for choosing dataset ----
          selectInput(inputId = "Zone",
                      label ='Choose a zone:',
                      choices = c("Zone -2", "Zone -3","Zone -4")),
          
          textInput( inputId="Windspeed", label = "Wind speed", placeholder = "Wind speed in m/sec",value = 0),
          textInput( inputId="SBC", label ="SBC", placeholder ="SBC",value = 0),
          
          
          conditionalPanel(
            condition = "input.Method == 'ESR'",
            textInput( inputId="Height", label ="Height", placeholder = "Height",value = 0)),
          conditionalPanel(
            condition = "input.Method != 'CWPH'",
            sliderInput("Capacity",
                        "Capacity:",
                        min = 1,
                        max = 7000,
                        value = 50)
          ),
          conditionalPanel(
            condition = "input.Method == 'CWPH'",
            textInput( inputId="area", label ="Area", placeholder = "Area",value = 0)),
          conditionalPanel(
            condition = "input.Method == 'Intake'",
            textInput( inputId="Diameter", label ="Diameter", placeholder ="Diameter",value = 0),
            textInput( inputId="Depth", label ="Depth", placeholder ="Depth",value = 0)),
          actionButton("run", "Run Analysis", icon("paper-plane"), 
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  
 
    
      ),
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        # Output: Verbatim text for data summary ----
        verbatimTextOutput("summary"),
        google_mapOutput(outputId ="map1")
       

        
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  key<-'AIzaSyDgxiDHIHZLWKZvd3-WyphRbLrhsdDvJLs'
  # Generate a summary of the dataset ----
  output$summary <- renderText({
      print(paste("Construction Method = ",input$Method,
            "\nZone = ",input$Zone,
            "\nWind ="," ",{input$Windspeed},
              randomVals()))

  })
  
  
  output$map1 <- renderGoogle_map({
    print("rendering")
    india= google_geocode('india',key=key)
    india= geocode_coordinates(india)
    coord=cbind(india$lat,india$lng)
      if(input$Method=="ESR")
        df<-read_excel("/Users/rsklanu/Rshiny/Myproject/Data.xlsx", sheet = "ESR")
      if (input$Method=="WTP")
        df<-read_excel("/Users/rsklanu/Rshiny/Myproject/Data.xlsx", sheet = "WTP")
      if (input$Method=="CWR")
        df<-read_excel("/Users/rsklanu/Rshiny/Myproject/Data.xlsx", sheet = "CWR")
      if (input$Method=="CWPH")
        df<-read_excel("/Users/rsklanu/Rshiny/Myproject/Data.xlsx", sheet = "CWPH")
      if (input$Method=="Intake")
        df<-read_excel("/Users/rsklanu/Rshiny/Myproject/Data.xlsx", sheet = "Intake")
    df_coords=as.data.frame(cbind(df$Lat,df$Lon))
    map= google_map(location = coord,data =df_coords, zoom = 4, key=key)%>%
      clear_markers() %>%
      add_markers(data = df_coords,lat = "V1", lon = "V2")

  })
  
  observe({
    print("hiiii")
    updateSelectInput(session,"Method",selected = input$Method)
    
  
  })
  
  
  
  randomVals <- eventReactive(input$run, {
  if(input$Method=="CWR")
  {
    Shortlist <- read_excel("Shortlist1.xlsx", 
                                       sheet = "CWR", col_types = c("skip", 
                                                                    "skip", "text", "skip", "text", 
                                                                    "numeric", "numeric", "text", "skip", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "numeric", "numeric"));
    cwr<- Shortlist[c(2,3,6,7)]
     PCapacity = as.numeric(input$Capacity);
     PSesimicZone = input$Zone;
     resultPCC<-lm(PCC ~ `Capacity(inKL)` + SesimicZone, data=cwr)
     pred_grid <- expand.grid(`Capacity(inKL)` = PCapacity, SesimicZone = PSesimicZone)
     predictionPCC <- predict(resultPCC, newdata = pred_grid )
    
     cwrRCC<- Shortlist[c(2,3,6,8)]
     resultRCC<-lm(RCC ~ `Capacity(inKL)` + SesimicZone, data=cwrRCC)
     pred_gridRCC <- expand.grid(`Capacity(inKL)` = PCapacity, SesimicZone = PSesimicZone)
     predictionRCC <- predict(resultRCC, newdata = pred_gridRCC )
     
     
     cwrFw<- Shortlist[c(2,3,6,9)]
     resultFrmwrk<-lm(Formwork ~ `Capacity(inKL)` + SesimicZone, data=cwrFw)
     pred_gridFw <- expand.grid(`Capacity(inKL)` = PCapacity, SesimicZone = PSesimicZone)
     predictionFw <- predict(resultFrmwrk, newdata = pred_gridFw )
     
     cwrSteel<- Shortlist[c(2,3,6,10)]
     resultSteel<-lm(`R/fSteel` ~ `Capacity(inKL)` + SesimicZone, data=cwrSteel)
     pred_gridSteel<- expand.grid(`Capacity(inKL)` = PCapacity, SesimicZone = PSesimicZone)
     predictionSteel <- predict(resultSteel, newdata = pred_gridSteel )
     paste("\nPCC = ",predictionPCC,"\nRCC = ",predictionRCC,"\nFormwork = ",predictionFw,"\nr/fSteel = ",predictionSteel)
  }
    
    ####CWPH
    if(input$Method=="CWPH")
    {
      Shortlist <- read_excel("Shortlist1.xlsx",
                              sheet = "CWPH", col_types = c("skip",
                                                            "skip", "text", "skip", "text",
                                                            "numeric", "numeric", "text", "skip",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric"));
      cwph<- Shortlist[c(2,3,6,7)]
      Parea = as.numeric(input$area)
      PWind = as.numeric(input$Windspeed)
      
      resultPCCh<-lm(PCC ~ `Area(inSqm)`, data=cwph)
      pred_gridh <- expand.grid(`Area(inSqm)` = Parea)
      predictionPCCh <- predict(resultPCCh, newdata = pred_gridh )
      
      cwrRCCh<- Shortlist[c(2,3,6,8)]
      resultRCCh<-lm(RCC ~ `Area(inSqm)` + WindSpeed, data=cwrRCCh)
      pred_gridRCCh <- expand.grid(`Area(inSqm)` = Parea, WindSpeed = PWind)
      predictionRCCh <- predict(resultRCCh, newdata = pred_gridRCCh )
      print(predictionRCCh)
      
      cwrFw<- Shortlist[c(2,3,6,9)]
      resultFrmwrk<-lm(Formwork  ~ `Area(inSqm)` + WindSpeed, data=cwrFw)
      pred_gridF <- expand.grid(`Area(inSqm)` = Parea, WindSpeed = PWind)
      predictionF <- predict(resultFrmwrk, newdata = pred_gridF )
      print(predictionF)
      
      cwphF<- Shortlist[c(2,3,6,10)]
      resultSteel<-lm(`R/fSteel` ~ `Area(inSqm)` + WindSpeed, data=cwphF)
      pred_gridF <- expand.grid(`Area(inSqm)` = Parea, WindSpeed = PWind)
      predictionSteel <- predict(resultSteel, newdata = pred_gridF )
      paste("\nPCC = ",predictionPCCh,"\nRCC = ",predictionRCCh,"\nFormwork = ",predictionF,"\nr/fSteel = ",predictionSteel)
    }
    
    
    
  })
  
  
}
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1080))



