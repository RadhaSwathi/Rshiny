WTP<-Fulldata[c(1:9)]
summary(WTP)
WTP[-2]
WTP$State=as.factor(WTP$State)
WTP$`Sesimic Zone`=as.factor(WTP$`Sesimic Zone`)
WTP$`Nature of Strata`<-as.factor(WTP$`Nature of Strata`)
WTP$Project=as.factor(WTP$Project)
WTP$Location=as.factor(WTP$Location)
WTPnull=lm(PCC~1, data=WTP)
WTPhigh=lm(PCC~., data=WTP)
high.fit2 <- step(WTPnull, scope=list(lower=WTPnull, upper=WTPhigh), direction="forward")
bckCredit = step(WTPhigh)
high.fit3 <- step(WTPnull, scope=list(lower=WTPnull, upper=WTPhigh), direction="both")
