summary(ShortlistIntake)
ShortlistIntake$SesimicZone<-as.factor(ShortlistIntake$SesimicZone)
ShortlistIntake$NatureofStrata<-as.factor(ShortlistIntake$NatureofStrata)
summary(ShortlistIntake)
#########PCC
Intake<- ShortlistIntake[c(1:8)]
summary(Intake)
Intakenull<-lm(PCC~1, data=Intake)
highfit<-lm(PCC~., data=Intake)
high.fit2 <- step(Intakenull, scope=list(lower=Intakenull, upper=highfit), direction="forward")
##PCC ~ SBC + SesimicZone + Diameter
bckCredit = step(highfit)
##PCC ~ SesimicZone + SBC + Diameter + Depth + Capacity
high.fit3 <- step(Intakenull, scope=list(lower=Intakenull, upper=highfit), direction="both")
####PCC ~ SBC + SesimicZone + Diameter
resultPCC<-lm(PCC~ SBC + SesimicZone + Diameter, data=Intake)
###R2-99.39% and 99.76 
library(leaps)
hight.fit4=regsubsets(PCC~., data=Intake, nbest=3)
summary(hight.fit4)
plot(hight.fit4, scale="adjr2")
plot(hight.fit4, scale="bic")
resultPCC<-lm(PCC ~ SesimicZone + SBC + Diameter + Depth + Capacity, data=Intake)
resultPCC1<-lm(PCC ~ SBC + SesimicZone + Diameter, data=Intake)
summary(resultPCC)
anova(resultPCC)
sz<-('Zone-3')
sbc <-c(15)
diameter<-c(10)
depth<-c(32)
capcity<-c(2513)
pred_grid <- expand.grid(SesimicZone=sz,  SBC=sbc , Diameter=diameter,  Depth=depth,  Capacity=capcity)
predictionPCC <- predict(resultPCC, newdata = pred_grid )
print(predictionPCC)

#####RCC

Intake<- ShortlistIntake[c(1:9)]
Intake<-Intake[-8]
summary(Intake)
IntakenullR<-lm(RCC~1, data=Intake)
highfitR<-lm(RCC~., data=Intake)
high.fit2 <- step(IntakenullR, scope=list(lower=IntakenullR, upper=highfitR), direction="forward")
##RCC ~ Depth + Diameter + SBC + WindSpeed + Capacity
bckCredit = step(highfitR)
##RCC ~ SesimicZone + SBC + Depth + Capacity
high.fit3 <- step(IntakenullR, scope=list(lower=IntakenullR, upper=highfitR), direction="both")
##RCC ~ Depth + SBC + WindSpeed + Capacity
resultRCC<-lm(RCC ~ Depth + Diameter + SBC + WindSpeed + Capacity, data=Intake)
resultRCC1<-lm(RCC ~ SesimicZone + SBC + Depth + Capacity, data=Intake)
resultRCC2<-lm(RCC~Depth + SBC + WindSpeed + Capacity, data=Intake)
resultRCC<-glm(RCC ~ `Area(inSqm)` + WindSpeed, data=Intake)
summary(resultRCC2)
anova(resultRCC)
depth <-55
diameter <-7
sBC <-15
windSpeed <- 47
capacity<-2217
pred_grid <- expand.grid(Depth = depth, Diameter= diameter, SBC=sBC , WindSpeed=windSpeed , Capacity=capacity)
prediction <- predict(resultRCC, newdata = pred_grid )
print(prediction)
#####Formwork

Intake<- ShortlistIntake[c(1:10)]
Intake<- Intake[-9]
Intake<- Intake[-8]
summary(Intake)
IntakenullF<-lm(Formwork~1, data=Intake)
highfitF<-lm(Formwork~., data=Intake)
high.fit2 <- step(IntakenullF, scope=list(lower=IntakenullF, upper=highfitF), direction="forward")
##Formwork ~ Depth + Diameter + SBC + WindSpeed + Capacity
bckCredit = step(highfitF)
##Formwork ~ Depth + SBC + WindSpeed + Capacity
high.fit3 <- step(IntakenullF, scope=list(lower=IntakenullF, upper=highfitF), direction="both")
##Formwork ~ Depth + SBC + WindSpeed + Capacity
resultFrmwrk<-lm(Formwork ~ Depths + Diameter + SBC + WindSpeed + Capacity, data=Intake)
resultFrmwrk1<-lm(Formwork ~ Depth + SBC + WindSpeed + Capacity, data=Intake)
summary(resultFrmwrk)
anova(resultFrmwrk)
Depth 
SBC 
WindSpeed 
Capacity
pred_grid <- expand.grid(`Area(inSqm)` = Area, WindSpeed = Windspeed)
prediction <- predict(resultFrmwrk, newdata = pred_grid )
print(prediction)

#######r/fsteel
Intake<- ShortlistIntake[c(1:11)]
Intake<- Intake[-10]
Intake<- Intake[-9]
Intake<- Intake[-8]

summary(Intake)
IntakenullS<-lm(  `R/fSteel` ~1, data=Intake)
highfitS<-lm(  `R/fSteel` ~., data=Intake)
high.fit2 <- step(IntakenullS, scope=list(lower=IntakenullS, upper=highfitS), direction="forward")
##`R/fSteel` ~ Capacity + Depth + SBC + WindSpeed
bckCredit = step(highfitS)
##`R/fSteel` ~ SesimicZone + SBC + Depth + Capacity
high.fit3 <- step(IntakenullS, scope=list(lower=IntakenullS, upper=highfitF), direction="both")
##`R/fSteel` ~ Capacity + Depth + SBC + WindSpeed
resultSteel<- lm(`R/fSteel` ~ Capacity + Depth + SBC + WindSpeed, data=Intake)

resultSteel1<- lm(`R/fSteel` ~ SesimicZone + SBC + Depth + Capacity, data=Intake)
resultSteel2<-lm(`R/fSteel` ~ Capacity + Depth + SBC + WindSpeed, data=Intake)


resultSteel<-glm(`R/fSteel` ~ Capacity + Depth + SBC + WindSpeed, data=Intake)
summary(resultSteel)
anova(resultSteel)
capacity
depth
windspeed 
sBC

pred_grid <- expand.grid(`Area(inSqm)` = Area, WindSpeed = Windspeed)
prediction <- predict(resultSteel, newdata = pred_grid )
print(prediction)



