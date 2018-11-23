
library(readxl)
Shortlist <- read_excel("~/Desktop/Shortlist.xlsx", 
                        sheet = "CWR", col_types = c("blank", 
                                                     "blank", "text", "blank", "text", 
                                                     "numeric", "numeric", "text", "blank", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric"))
View(Shortlist)
summary(Shortlist)
Shortlist$Project<-as.factor(Shortlist$Project)
Shortlist$SesimicZone<-as.factor(Shortlist$SesimicZone)
Shortlist$NatureofStrata<-as.factor(Shortlist$NatureofStrata)
#########PCC
cwr<- Shortlist[c(2,3,4,6,7)]
summary(cwr)
##model making
cwrnull<-lm(PCC~1, data=cwr)
highfit<-lm(PCC~., data=cwr)
high.fit2 <- step(cwrnull, scope=list(lower=cwrnull, upper=highfit), direction="forward")
bckCredit = step(highfit)
high.fit3 <- step(cwrnull, scope=list(lower=cwrnull, upper=highfit), direction="both")

##model
resultPCC<-lm(PCC ~ `Capacity(inKL)` + SesimicZone+SBC, data=cwr)
summary(resultPCC)
anova(resultPCC)
Capacity <-c(1000) 
Zone <- c('Zone-3')
pred_grid <- expand.grid(`Capacity(inKL)` = Capacity, SesimicZone = Zone)
predictionPCC <- predict(resultPCC, newdata = pred_grid )
print(predictionPCC)

#####RCC

cwr<- Shortlist[c(2,3,6,8)]
summary(cwr)
cwrnullR<-lm(RCC~1, data=cwr)
highfitR<-lm(RCC~., data=cwr)
high.fit2 <- step(cwrnullR, scope=list(lower=cwrnullR, upper=highfitR), direction="forward")
bckCredit = step(highfitR)
high.fit3 <- step(cwrnullR, scope=list(lower=cwrnullR, upper=highfitR), direction="both")
resultRCC<-lm(RCC ~ `Capacity(inKL)` + SesimicZone, data=cwr)
summary(resultRCC)
anova(resultRCC)
Capacity <-c(1000) 
Zone <- c('Zone-3')
pred_grid <- expand.grid(`Capacity(inKL)` = Capacity, SesimicZone = Zone)
prediction <- predict(resultRCC, newdata = pred_grid )
print(prediction)
#####Formwork

cwr<- Shortlist[c(2,3,6,9)]
summary(cwr)
cwrnullF<-lm(Formwork~1, data=cwr)
highfitF<-lm(Formwork~., data=cwr)
high.fit2 <- step(cwrnullF, scope=list(lower=cwrnullF, upper=highfitF), direction="forward")
bckCredit = step(highfitF)
high.fit3 <- step(cwrnullF, scope=list(lower=cwrnullF, upper=highfitF), direction="both")
resultFrmwrk<-lm(Formwork ~ `Capacity(inKL)` + SesimicZone, data=cwr)
summary(resultFrmwrk)
anova(resultFrmwrk)
CapacityF <-c(1000) 
ZoneF <- c('Zone-3')
pred_grid <- expand.grid(`Capacity(inKL)` = CapacityF, SesimicZone = ZoneF)
prediction <- predict(resultFrmwrk, newdata = pred_grid )
print(prediction)

#######r/fsteel

cwr<- Shortlist[c(2,3,6,10)]
summary(cwr)
cwrnullS<-lm(  `R/fSteel` ~1, data=cwr)
highfitS<-lm(  `R/fSteel` ~., data=cwr)
high.fit2 <- step(cwrnullS, scope=list(lower=cwrnullS, upper=highfitS), direction="forward")
bckCredit = step(highfitS)
high.fit3 <- step(cwrnullS, scope=list(lower=cwrnullS, upper=highfitF), direction="both")
resultSteel<-lm(`R/fSteel` ~ `Capacity(inKL)` + SesimicZone, data=cwr)
summary(resultSteel)
anova(resultSteel)
CapacityS <-c(1000) 
ZoneS <- c('Zone-3')
pred_grid <- expand.grid(`Capacity(inKL)` = CapacityS, SesimicZone = ZoneS)
prediction <- predict(resultSteel, newdata = pred_grid )
print(prediction)



