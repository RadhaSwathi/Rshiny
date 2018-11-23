summary(ShortlistCWPH)
ShortlistCWPH$SesimicZone<-as.factor(ShortlistCWPH$SesimicZone)
ShortlistCWPH$NatureofStrata<-as.factor(ShortlistCWPH$NatureofStrata)
summary(ShortlistCWPH)
#########PCC
cwph<- ShortlistCWPH[c(2,3,5,6)]
summary(cwph)
cwphnull<-lm(PCC~1, data=cwph)
highfit<-lm(PCC~., data=cwph)
high.fit2 <- step(cwphnull, scope=list(lower=cwphnull, upper=highfit), direction="forward")
bckCredit = step(highfit)
high.fit3 <- step(cwphnull, scope=list(lower=cwphnull, upper=highfit), direction="both")
resultPCC<-lm(PCC ~ `Area(inSqm)`, data=cwph)
summary(resultPCC)
anova(resultPCC)
Area <-c(300) 
pred_grid <- expand.grid(`Area(inSqm)` = Area)
predictionPCC <- predict(resultPCC, newdata = pred_grid )
print(predictionPCC)

#####RCC

cwph<- ShortlistCWPH[c(2,3,5,7)]
summary(cwph)
cwphnullR<-lm(RCC~1, data=cwph)
highfitR<-lm(RCC~., data=cwph)
high.fit2 <- step(cwphnullR, scope=list(lower=cwphnullR, upper=highfitR), direction="forward")
bckCredit = step(highfitR)
high.fit3 <- step(cwphnullR, scope=list(lower=cwphnullR, upper=highfitR), direction="both")
resultRCC<-glm(RCC ~ `Area(inSqm)` + WindSpeed, data=cwph)
summary(resultRCC)
anova(resultRCC)
Area <-c(300) 
Windspeed <- c(39)
pred_grid <- expand.grid(`Area(inSqm)` = Area, WindSpeed = Windspeed)
prediction <- predict(resultRCC, newdata = pred_grid )
print(prediction)
#####Formwork

cwph<- ShortlistCWPH[c(2,3,5,8)]
summary(cwph)
cwphnullF<-lm(Formwork~1, data=cwph)
highfitF<-lm(Formwork~., data=cwph)
high.fit2 <- step(cwphnullF, scope=list(lower=cwphnullF, upper=highfitF), direction="forward")
bckCredit = step(highfitF)
high.fit3 <- step(cwphnullF, scope=list(lower=cwphnullF, upper=highfitF), direction="both")
resultFrmwrk<-glm(Formwork  ~ `Area(inSqm)` + WindSpeed, data=cwph)
summary(resultFrmwrk)
anova(resultFrmwrk)
Area <-c(300) 
Windspeed <- c(39)
pred_grid <- expand.grid(`Area(inSqm)` = Area, WindSpeed = Windspeed)
prediction <- predict(resultFrmwrk, newdata = pred_grid )
print(prediction)

#######r/fsteel

cwph<- ShortlistCWPH[c(2,3,5,9)]
summary(cwph)
cwphnullS<-lm(  `R/fSteel` ~1, data=cwph)
highfitS<-lm(  `R/fSteel` ~., data=cwph)
high.fit2 <- step(cwphnullS, scope=list(lower=cwphnullS, upper=highfitS), direction="forward")
bckCredit = step(highfitS)
high.fit3 <- step(cwphnullS, scope=list(lower=cwphnullS, upper=highfitF), direction="both")
resultSteel<-glm(`R/fSteel` ~ `Area(inSqm)` + WindSpeed, data=cwph)
summary(resultSteel)
anova(resultSteel)
Area <-c(300) 
Windspeed <- c(39)
pred_grid <- expand.grid(`Area(inSqm)` = Area, WindSpeed = Windspeed)
prediction <- predict(resultSteel, newdata = pred_grid )
print(prediction)



