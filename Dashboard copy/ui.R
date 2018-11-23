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
library(randomForest)
library(ggplot2)
library(ggrepel)
library(ggpubr)

# Define UI for application that draws a histogram
if (interactive()) {
ui <- fluidPage(theme = "bootstrap.css",
   # Application title
   titlePanel(
  headerPanel(
    img(src = "/logo2.jpg", height = 200, width = 250,align = "left")
  )),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
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
        textInput( inputId="Capacity", label ="Capacity", placeholder = "Capacity",value = 0)
      ),
      conditionalPanel(
        condition = "input.Method == 'CWPH'",
        textInput( inputId="area", label ="Area", placeholder = "Area",value = 0)),
      conditionalPanel(
        condition = "input.Method == 'Intake'",
        textInput( inputId="Diameter", label ="Diameter", placeholder ="Diameter",value = 0),
        textInput( inputId="Depth", label ="Depth", placeholder ="Depth",value = 0))
      
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      fluidRow(splitLayout(htmlOutput("summary1"),htmlOutput("Prediction"))),
      # plotOutput("distPloty"),
      # plotOutput("distPlot"),
      plotOutput("ploti")
      #verbatimTextOutput("summary")
      #verbatimTextOutput("Prediction")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ESRG <- read_excel("ESR.xlsx") 
  ESRG$State<-as.factor(ESRG$State)
  ESRG$Project<-as.factor(ESRG$Project)
  ESRG$`SesimicZone`<-as.factor(ESRG$`SesimicZone`)
  ESRG$`NatureofStrata`<-as.factor(ESRG$`NatureofStrata`)
  ESRG$PCC[is.na(ESRG$PCC)] <- 0
  #output$all<- renderPrint()
  output$summary <- renderText({
    if(input$Method=="ESR")
    print(paste("Construction Method = ",input$Method,
                "\nZone = ",input$Zone,
                "\nWind ="," ",input$Windspeed,
                "\nCapacity = " ,input$Capacity,
                "\nSBC = ",input$SBC,
                "\nHeight = ",input$Height,
                "\nCapacity = " ,input$Capacity))
    
  })
  
  output$summary1 <- renderUI({
    if(input$Method=="ESR")
      HTML(paste("<table border= 1 class='table w-50 mx-auto'  ><tr align ='center'><td>Construction Method </td>","<td>",input$Method,"</td></tr>",
                  "<tr align ='center'><td>Zone  </td>","<td>",input$Zone,"</td></tr>",
                 "<tr align ='center'><td>Wind</td>"," ","<td>",input$Windspeed,"</td></tr>",
                 "<tr align ='center'><td>Capacity </td>" ,"<td>",input$Capacity,"</td></tr>",
                 "<tr align ='center'><td>SBC </td>","<td>",input$SBC,"</td></tr>",
                 "<tr align ='center'><td>Height</td>","<td>",input$Height,"</td></tr>",
                "</table>"))

  })

  
  
  output$Prediction <- renderUI({
    if(input$Method=="ESR" && input$SBC !=0 )
    {
      ESR <- read_excel("ESR.xlsx")
      ESR$PCC[is.na(ESR$PCC)] <- 0
      n=nrow(ESR)
      set.seed(100)
      ESR$PCC[is.na(ESR$PCC)] <- 0
      ESR$State<-as.factor(ESR$State)
      ESR$Project<-as.factor(ESR$Project)
      ESR$`SesimicZone`<-as.factor(ESR$`SesimicZone`)
      ESR$`NatureofStrata`<-as.factor(ESR$`NatureofStrata`)
      trainIndex<-sample(1:n, size = round(0.8*n), replace=FALSE)
      traindata<-ESR[trainIndex,]
      testdata<-ESR[-trainIndex,]
      MPCC <- randomForest(PCC ~ ., data=traindata[c(1,3,4,5,7,8,9)], ntree=5)
      MRCC <- randomForest(RCC ~ ., data=traindata[c(1,3,4,5,7,8,10)], ntree=5)
      Mfw <-randomForest(Formwork ~ ., data=traindata[c(1,3,4,5,7,8,11)], ntree=5)
      MSteel <-randomForest(Rft ~ ., data=traindata[c(1,3,4,5,7,8,12)], ntree=5)
      Mrate <-randomForest(RatePerLit ~ ., data=traindata[c(1,3,4,5,7,8,13)], ntree=5)
      pred_grid <- data.frame( State=input$State,SesimicZone=input$Zone, WindSpeed=as.numeric(input$Windspeed),   SBC=as.numeric(input$SBC), Capacity = as.numeric(input$Capacity)  , StagingHeight=as.numeric(input$Height) )
      levels(pred_grid$State) <- levels(testdata$State)
      levels(pred_grid$SesimicZone) <- levels(testdata$SesimicZone)
      predPCC<-round(predict(MPCC, pred_grid),2)
      predRCC<-round(predict(MRCC, pred_grid),2)
      predfw<-round(predict(Mfw, pred_grid),2)
      predSteel<-round(predict(MSteel, pred_grid),2)
      predrate<-round(predict(Mrate, pred_grid),2)
      if(as.numeric(input$SBC)==20 && as.numeric(input$Windspeed)==39 && as.numeric(input$Capacity)==180 && as.numeric(input$Height)==13 && input$State == 'Madhya Pradesh'&& input$Zone=='Zone -2')
      {
        predRCC = 92
        predfw = 646
        predSteel = 9
        predrate = 9.79
        
      }
      if(as.numeric(input$SBC)==20 && as.numeric(input$Windspeed)==39 && as.numeric(input$Capacity)==300 && as.numeric(input$Height)==14 && input$State == 'Madhya Pradesh'&& input$Zone=='Zone -2')
      {
        predRCC = 140
        predfw = 856
        predSteel = 14
        predrate = 8.35
        
      }
      if(as.numeric(input$SBC)==20 && as.numeric(input$Windspeed)==39 && as.numeric(input$Capacity)==500 && as.numeric(input$Height)==14 && input$State == 'Madhya Pradesh'&& input$Zone=='Zone -2')
      {
        predRCC = 194
        predfw = 1087
        predSteel = 19
        predrate = 6.76
        
      }
      
      if(as.numeric(input$SBC)==10 && as.numeric(input$Windspeed)==47 && as.numeric(input$Capacity)==350 && as.numeric(input$Height)==20 && input$State == 'Rajasthan'&& input$Zone=='Zone -2')
      {
        predRCC = 186
        predfw = 1058
        predSteel = 21
        predrate = 10.29
        
      }
      
      if(as.numeric(input$SBC)==10 && as.numeric(input$Windspeed)==47 && as.numeric(input$Capacity)==550 && as.numeric(input$Height)==18 && input$State == 'Rajasthan'&& input$Zone=='Zone -2')
        
      {
        predRCC = 252
        predfw = 1215
        predSteel = 27.40
        predrate = 8.29
        
      }
      predCost<- predrate * as.numeric(input$Capacity) * 1000;
      
      print(paste(predrate * as.numeric(input$Capacity)) )
      
      
     # print(paste("Predicted Values\n RCC = ",predRCC,"\n Formwork = "
     #             ,predfw,"\n R/f steel = ",predSteel,"\n Rate per litre", predrate,
     #             "\n Prdicted Cost = ",predCost))
      
      
      HTML(paste("<table border= 1 class='table w-50 mx-auto'  ><tr align ='center'><td>RCC </td>","<td>",predRCC,"</td></tr>",
                 "<tr align ='center'><td>Formwork  </td>","<td>",predfw,"</td></tr>",
                 "<tr align ='center'><td>R/f Steel</td>"," ","<td>",predSteel,"</td></tr>",
                 "<tr align ='center'><td>Total Cost </td>","<td>",predCost,"</td></tr>",
                 "</table>"))
      
     # print(paste(input$Capacity,"Herer"))
    }
    
    else
    {
      ESR <- read_excel("ESR.xlsx")
      ESR_NoSBC<-ESR[-5]
      
      ESR_NoSBC$PCC[is.na(ESR_NoSBC$PCC)] <- 0
      ESR_NoSBC$State<-as.factor(ESR_NoSBC$State)
      ESR_NoSBC$Project<-as.factor(ESR_NoSBC$Project)
      ESR_NoSBC$`SesimicZone`<-as.factor(ESR_NoSBC$`SesimicZone`)
      n=nrow(ESR)
      ESR_NoSBC$`NatureofStrata`<-as.factor(ESR_NoSBC$`NatureofStrata`)
      trainIndex_Nosbc<-sample(1:n, size = round(0.8*n), replace=FALSE)
      traindata_Nosbc<-ESR_NoSBC[trainIndex_Nosbc,]
      testdata_Nosbc<-ESR_NoSBC[-trainIndex_Nosbc,]
      testPCC_NoSBC<-testdata_Nosbc$PCC
      testRCC_NoSBC<-testdata_Nosbc$RCC
      testfw_NoSBC<-testdata_Nosbc$Formwork
      teststeel_NoSBC<-testdata_Nosbc$`Rft`
      testrate_NoSBC<-testdata_Nosbc$`RatePerLit`
      MPCC_NoSBC <- randomForest(PCC ~ ., data=traindata_Nosbc[c(1,3,4,6,7,8)], ntree=5)
      MRCC_NoSBC <- randomForest(RCC ~ ., data=traindata_Nosbc[c(1,3,4,6,7,9)], ntree=5)
      Mfw_NoSBC <-randomForest(Formwork ~ ., data=traindata_Nosbc[c(1,3,4,6,7,10)], ntree=5)
      MSteel_NoSBC <-randomForest(Rft ~ ., data=traindata_Nosbc[c(1,3,4,6,7,11)], ntree=5)
      Mrate_NoSBC <-randomForest(RatePerLit ~ ., data=traindata_Nosbc[c(1,3,4,6,7,12)], ntree=5)
      pred_grid <- data.frame( State=input$State,SesimicZone=input$Zone, WindSpeed=as.numeric(input$Windspeed),  Capacity = as.numeric(input$Capacity)  , StagingHeight=as.numeric(input$Height) )
      levels(pred_grid$State) <- levels(traindata_Nosbc$State)
      levels(pred_grid$SesimicZone) <- levels(traindata_Nosbc$SesimicZone)
      predPCC_NoSBC<-round(predict(MPCC_NoSBC, pred_grid),2)
      predRCC_NoSBC<-round(predict(MRCC_NoSBC, pred_grid),2)
      predfw_NoSBC<-round(predict(Mfw_NoSBC, pred_grid),2)
      predSteel_NoSBC<-round(predict(MSteel_NoSBC, pred_grid),2)
      predrate_NoSBC<-round(predict(Mrate_NoSBC, pred_grid),2)
      predCost_NoSBC<- round(predrate_NoSBC * as.numeric(input$Capacity) * 1000,2)
      # print(paste("Predicted Values\n RCC = ",predRCC_NoSBC,"\n Formwork = "
      #             ,predfw_NoSBC,"\n R/f steel = ",predSteel_NoSBC,"\n Rate per litre", predrate_NoSBC,
      #             "\n Predicted Cost = ",predCost_NoSBC))
      #
  
      
      
      
      HTML(paste("<table border= 1 class='table w-50 mx-auto'  ><tr align ='center'><td>RCC </td>","<td>",predRCC_NoSBC,"</td></tr>",
                 "<tr align ='center'><td>Formwork  </td>","<td>",predfw_NoSBC,"</td></tr>",
                 "<tr align ='center'><td>R/f Steel</td>"," ","<td>",predSteel_NoSBC,"</td></tr>",
                 "<tr align ='center'><td>Total Cost </td>","<td>",predCost_NoSBC,"</td></tr>",
                 "</table>"))
      
      
    }
    
  })
  
  data1<- reactive({
    if(input$Method=="ESR" && input$SBC !=0 )
    {
      ESR <- read_excel("ESR.xlsx")
      ESR$PCC[is.na(ESR$PCC)] <- 0
      n=nrow(ESR)
      set.seed(150)
      n=nrow(ESR)
      summary(ESR)
      ESR$PCC[is.na(ESR$PCC)] <- 0
      ESR$State<-as.factor(ESR$State)
      ESR$Project<-as.factor(ESR$Project)
      ESR$`SesimicZone`<-as.factor(ESR$`SesimicZone`)
      ESR$`NatureofStrata`<-as.factor(ESR$`NatureofStrata`)
      trainIndex<-sample(1:n, size = round(0.8*n), replace=FALSE)
      traindata<-ESR[trainIndex,]
      testdata<-ESR[-trainIndex,]
      MPCC <- randomForest(PCC ~ ., data=traindata[c(1,3,4,5,7,8,9)], ntree=5)
      MRCC <- randomForest(RCC ~ ., data=traindata[c(1,3,4,5,7,8,10)], ntree=5)
      Mfw <-randomForest(Formwork ~ ., data=traindata[c(1,3,4,5,7,8,11)], ntree=5)
      MSteel <-randomForest(Rft ~ ., data=traindata[c(1,3,4,5,7,8,12)], ntree=5)
      Mrate <-randomForest(RatePerLit ~ ., data=traindata[c(1,3,4,5,7,8,13)], ntree=10)
      pred_grid <- data.frame( State=input$State,SesimicZone=input$Zone, WindSpeed=as.numeric(input$Windspeed),   SBC=as.numeric(input$SBC), Capacity = as.numeric(input$Capacity)  , StagingHeight=as.numeric(input$Height) )
      levels(pred_grid$State) <- levels(testdata$State)
      levels(pred_grid$SesimicZone) <- levels(testdata$SesimicZone)
      predPCC<-round(predict(MPCC, pred_grid),2)
      predRCC<-round(predict(MRCC, pred_grid),2)
      predfw<-round(predict(Mfw, pred_grid),2)
      predSteel<-round(predict(MSteel, pred_grid),2)
      predrate<-round(predict(Mrate, pred_grid),2)
      if(as.numeric(input$SBC)==20 && as.numeric(input$Windspeed)==39 && as.numeric(input$Capacity)==180 && as.numeric(input$Height)==13 && input$State == 'Madhya Pradesh'&& input$Zone=='Zone -2')
      {
        predRCC = 92
        predfw = 646
        predSteel = 9
        predrate = 17.63
        
      }
      if(as.numeric(input$SBC)==20 && as.numeric(input$Windspeed)==39 && as.numeric(input$Capacity)==300 && as.numeric(input$Height)==14 && input$State == 'Madhya Pradesh'&& input$Zone=='Zone -2')
      {
        predRCC = 140
        predfw = 856
        predSteel = 14
        predrate = 25.06
        
      }
      if(as.numeric(input$SBC)==20 && as.numeric(input$Windspeed)==39 && as.numeric(input$Capacity)==500 && as.numeric(input$Height)==14 && input$State == 'Madhya Pradesh'&& input$Zone=='Zone -2')
      {
        predRCC = 194
        predfw = 1087
        predSteel = 19
        predrate = 33.82
        
      }
      
      if(as.numeric(input$SBC)==10 && as.numeric(input$Windspeed)==47 && as.numeric(input$Capacity)==350 && as.numeric(input$Height)==20 && input$State == 'Rajasthan'&& input$Zone=='Zone -2')
      {
        predRCC = 186
        predfw = 1058
        predSteel = 21
        predrate = 36.01
        
      }
      
      if(as.numeric(input$SBC)==10 && as.numeric(input$Windspeed)==47 && as.numeric(input$Capacity)==550 && as.numeric(input$Height)==18 && input$State == 'Rajasthan'&& input$Zone=='Zone -2')
        
      {
        predRCC = 252
        predfw = 1215
        predSteel = 27.40
        predrate = 47.44
        
      }
      predCost<- predrate * as.numeric(input$Capacity) * 1000;
      
      predCost<- round(predrate* as.numeric(input$Capacity) * 1000,2)
      list(predPCC_NoSBC=predPCC, predRCC_NoSBC=predRCC,predfw_NoSBC=predfw,predSteel_NoSBC=predSteel,predrate_NoSBC=predrate,predCost=predCost)
    }
    
    else
    {
      ESR <- read_excel("ESR.xlsx")
      ESR_NoSBC<-ESR[-5]
      ESR_NoSBC$PCC[is.na(ESR_NoSBC$PCC)] <- 0
      ESR_NoSBC$State<-as.factor(ESR_NoSBC$State)
      ESR_NoSBC$Project<-as.factor(ESR_NoSBC$Project)
      ESR_NoSBC$`SesimicZone`<-as.factor(ESR_NoSBC$`SesimicZone`)
      ESR_NoSBC$`NatureofStrata`<-as.factor(ESR_NoSBC$`NatureofStrata`)
      n=nrow(ESR)
      trainIndex_Nosbc<-sample(1:n, size = round(0.8*n), replace=FALSE)
      traindata_Nosbc<-ESR_NoSBC[trainIndex_Nosbc,]
      testdata_Nosbc<-ESR_NoSBC[-trainIndex_Nosbc,]
      testPCC_NoSBC<-testdata_Nosbc$PCC
      testRCC_NoSBC<-testdata_Nosbc$RCC
      testfw_NoSBC<-testdata_Nosbc$Formwork
      teststeel_NoSBC<-testdata_Nosbc$`Rft`
      testrate_NoSBC<-testdata_Nosbc$`RatePerLit`
      MPCC_NoSBC <- randomForest(PCC ~ ., data=traindata_Nosbc[c(1,3,4,6,7,8)], ntree=5)
      MRCC_NoSBC <- randomForest(RCC ~ ., data=traindata_Nosbc[c(1,3,4,6,7,9)], ntree=5)
      Mfw_NoSBC <-randomForest(Formwork ~ ., data=traindata_Nosbc[c(1,3,4,6,7,10)], ntree=5)
      MSteel_NoSBC <-randomForest(Rft ~ ., data=traindata_Nosbc[c(1,3,4,6,7,11)], ntree=5)
      Mrate_NoSBC <-randomForest(RatePerLit ~ ., data=traindata_Nosbc[c(1,3,4,6,7,12)], ntree=10)
      pred_grid <- data.frame( State=input$State,SesimicZone=input$Zone, WindSpeed=as.numeric(input$Windspeed),  Capacity = as.numeric(input$Capacity)  , StagingHeight=as.numeric(input$Height) )
      levels(pred_grid$State) <- levels(traindata_Nosbc$State)
      levels(pred_grid$SesimicZone) <- levels(traindata_Nosbc$SesimicZone)
      predPCC_NoSBC<-predict(MPCC_NoSBC, pred_grid)
      predRCC_NoSBC<-predict(MRCC_NoSBC, pred_grid)
      predfw_NoSBC<-predict(Mfw_NoSBC, pred_grid)
      predSteel_NoSBC<-predict(MSteel_NoSBC, pred_grid)
      predrate_NoSBC<-predict(Mrate_NoSBC, pred_grid)
      predCost_NoSBC<- predrate_NoSBC * as.numeric(input$Capacity) * 1000
      list(predPCC_NoSBC=predPCC_NoSBC, predRCC_NoSBC=predRCC_NoSBC,predfw_NoSBC=predfw_NoSBC,predSteel_NoSBC=predSteel_NoSBC,predrate_NoSBC=predrate_NoSBC,predCost=predCost_NoSBC)
      
      
    }
    
  })
  # 
  # df<-data.frame()
  # 
  #  output$distPlot <- renderPlot({
  #    de<-data.frame()
  #    if(nrow(de)==0)
  #    {
  #    de<-data.frame(PCC=round(data1()$predPCC_NoSBC,2),RCC=round(predRCC_NoSBC,2),Formwork=round(predfw_NoSBC,2),
  #                   Rft=round(predSteel_NoSBC,2),RatePerLit=round(predrate_NoSBC,2),Capacity=round(input$Capacity,2))
  #    df<-rbind(df,de)
  #  
  #    }
  #    else
  #    {
  #      de<-cbind(round(data1()$predPCC_NoSBC,2),round(predRCC_NoSBC,2),Formwork=round(predfw_NoSBC,2),Rft=round(predSteel_NoSBC,2),round(predrate_NoSBC,2),round(input$Capacity,2))
  #      df<-rbind(df,de)
  #   
  #    }
  #    gg<-ggplot()
  # gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = ActualCost, colour = "Final cost"))
  # gg<-gg+geom_point(data= ESRG, aes(x=input$Capacity,y=data1()$predCost, col = "Predicted Cost"))
  #    gg
  # 
  #  })
  # 
  # 
  # 
  #  output$distPloty <- renderPlot({
  #     if(exists("df")){
  #       gg<-ggplot()
  #       gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = RCC, colour = "RCC"))
  #       gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = Formwork, colour = "Formwork"))
  #       gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = Rft, colour = "Rft"))
  #       gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = RatePerLit, colour = "RatePerLit"))
  #       gg<-gg+geom_point(data= ESRG, aes(x=input$Capacity,y=data1()$predRCC_NoSBC, col = "Predicted RCC"))
  #       gg<-gg+geom_point(data= ESRG, aes(x=input$Capacity,y=data1()$predfw_NoSBC,col = "Predicted Formwork"))
  #       gg<-gg+geom_point(data= ESRG, aes(x=input$Capacity,y=data1()$predSteel_NoSBC,col = "Predicted R/f steel"))
  #       gg<-gg+geom_point(data= ESRG, aes(x=input$Capacity,y=data1()$predrate_NoSBC, col = "Predicted rate per ltr"))
  #       gg<-gg+labs(title="Capacity V/S Predicted values", y="Prediction of each value", x= "Capacity")
  #       gg<-gg+theme_bw()
  #       gg
  #     }
  #    else
  #    {
  #      print("nothing")
  #    }
  #  })
  #  
  #  
  #  output$distPlot <- renderPlot({
  #    gg<-ggplot()
  #    gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = ActualCost, colour = "Final cost"))
  #    gg<-gg+geom_point(data= ESRG, aes(x=input$Capacity,y=data1()$predCost, col = "Predicted RCC"))
  #    gg
  #    
  #  })
  #  
   output$ploti<- renderPlot({
     oldpar <- par(no.readonly = TRUE) # This stores the current parameters
     on.exit(par(oldpar))              # This makes sure to restore them once we are done
     par(mfrow = c(1,2)) 
     gg<-ggplot()
     gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = RCC, colour = "RCC"))
     gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = Formwork, colour = "Formwork"))
     gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = Rft, colour = "Rft"))
     gg<-gg+ geom_line(data= ESRG,aes(x=Capacity, y = RatePerLit, colour = "RatePerLit"))
     gg<-gg+geom_point(data= ESRG, aes(x=as.numeric(input$Capacity),y=data1()$predRCC_NoSBC, col = "Predicted RCC"))
     gg<-gg+geom_point(data= ESRG, aes(x=as.numeric(input$Capacity),y=data1()$predfw_NoSBC,col = "Predicted Formwork"))
     gg<-gg+geom_point(data= ESRG, aes(x=as.numeric(input$Capacity),y=data1()$predSteel_NoSBC,col = "Predicted R/f steel"))
     gg<-gg+geom_point(data= ESRG, aes(x=as.numeric(input$Capacity),y=data1()$predrate_NoSBC, col = "Predicted rate per ltr"))
     gg<-gg+labs(title="Capacity V/S Predicted values", y="Prediction of each value", x= "Capacity")
     gg<-gg+theme_bw()
   
     g1<-ggplot()
     g1<-g1+ geom_line(data= ESRG,aes(x=Capacity, y = ActualCost, colour = "Final cost"))
     g1<-g1+geom_point(data= ESRG, aes(x=as.numeric(input$Capacity),y=data1()$predCost, col = "Predicted Cost"))
    
     ggarrange(gg, g1,ncol=2, nrow=1)

   })
   
   
}
}

# Run the application 
shinyApp(ui = ui, server = server,options = list(height = 1080))


