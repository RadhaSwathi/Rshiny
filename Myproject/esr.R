library(readxl)
library(randomForest)
Fulldata <- read_excel("Rshiny/Myproject/Fulldata.xlsx", 
                       col_types = c("blank", "text", "blank", 
                                     "blank", "blank", "text", "numeric", 
                                     "numeric", "blank", "blank", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"))
View(Fulldata)

### building model

esrPCC<-Fulldata[c(1:7)]
summary(esrPCC)
esrPCC$State=as.factor(esrPCC$State)
esrPCC$`Sesimic Zone`=as.factor(esrPCC$`Sesimic Zone`)
esrnull<-lm(PCC~1, data=esrPCC)
esrhighfit<-lm(PCC~., data=esrPCC)
high.fit2 <- step(esrnull, scope=list(lower=esrnull, upper=esrhighfit), direction="forward")
bckCredit = step(esrhighfit)
high.fit3 <- step(esrnull, scope=list(lower=esrnull, upper=esrhighfit), direction="both")
resultPCC<-lm((PCC^1/3)  ~ Capacity + State, data=esrPCC)
resultPCC<-glm((PCC^1/3) ~ Capacity +`Staging Height`+SBC, data=esrPCC)
plot(resultPCC)

summary(resultPCC)
anova(resultPCC)
capacity <- 150
state<- 'Madhya Pradesh'
sh<-12
sbc=15
pred_grid <- expand.grid( Capacity=capacity,State=state,SBC=sbc,`Staging Height`=sh)
prediction <- predict(resultPCC, newdata = pred_grid )
print(prediction)
#####

esrRCC<-Fulldata[c(1:8)]
esrRCC=esrRCC[-7]
summary(esrRCC)
esrRCC$State=as.factor(esrRCC$State)
esrRCC$`Sesimic Zone`=as.factor(esrRCC$`Sesimic Zone`)
esrnullr<-lm(RCC~1, data=esrRCC)
esrhighfitr<-lm(RCC~., data=esrRCC)
high.fit2 <- step(esrnullr, scope=list(lower=esrnullr, upper=esrhighfitr), direction="forward")
bckCredit = step(esrhighfitr)
high.fit3 <- step(esrnullr, scope=list(lower=esrnullr, upper=esrhighfitr), direction="both")
resultRCC<-lm(RCC ~ Capacity + `Staging Height` + `Sesimic Zone`, data=esrRCC)
resultRCC<-lm(RCC ~ State + Capacity + `Staging Height`+ `Sesimic Zone`, data=esrRCC)

resultRCC<-lm(RCC ~ State + Capacity + `Staging Height`, data=esrRCC)
plot(resultRCC)
summary(resultRCC)
anova(resultRCC)
capacity <- 200
state<- 'Madhya Pradesh'
sh<-12
sz='Zone-3'
pred_grid <- expand.grid( Capacity=capacity,`Staging Height`=sh,`Sesimic Zone`=sz,State=state)
prediction <- predict(resultRCC, newdata = pred_grid )
print(prediction)

