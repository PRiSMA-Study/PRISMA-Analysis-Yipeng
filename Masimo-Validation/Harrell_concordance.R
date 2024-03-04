df2 <- temp_df %>% filter(!is.na(M06_SPHB_LBORRES)&!is.na(M08_CBC_HB_LBORRES))

#library(dynpred)
#model<-survfit(Surv(M08_CBC_HB_LBORRES,rep(1,nrow(df2))) ~ M06_SPHB_LBORRES + cluster(MOMID),data = df2)
#summary(model)
#dynpred::cindex(Surv(M08_CBC_HB_LBORRES,rep(1,nrow(df2))) ~ M06_SPHB_LBORRES + cluster(MOMID),data = df2)

#library(pec)
#model<-coxph(Surv(M08_CBC_HB_LBORRES,rep(1,nrow(df2))) ~ M06_SPHB_LBORRES + cluster(MOMID),data = df2)
#risk_scores <- predict(model, newdata = df2, type = "risk")
#c_index_value <- pec::cindex(Surv(df2$M08_CBC_HB_LBORRES,rep(1,nrow(df2))) ~ risk_scores)

library(survival)
library(Hmisc)
#fit a cox model and consider CBC measurements as the event time, SpHb measurements as the covariates and a cluster structure based on MOMID.
model1<-coxph(Surv(M08_CBC_HB_LBORRES,rep(1,nrow(df2))) ~ M06_SPHB_LBORRES + cluster(MOMID),data = df2)
#the estimated parameter of SpHb is expected to be negative, since large SpHb measurements expect large CBC measurements, hence less hazard.
summary(model1)
#predict the risk for each observation and calculate the Harrel's C accordingly.
#rep(1, nrow(df2)) is used for censored status since there is no censored observation, all of the event time(CBC measurement) is observed.
risk_scores <- predict(model1, newdata = df2, type = "risk")
c_index_value <- rcorr.cens(x = risk_scores, S = Surv(df2$M08_CBC_HB_LBORRES, rep(1, nrow(df2))))
print(c_index_value)

#Same idea here but assume a gamma frailty for MOMID
model2<-coxph(Surv(M08_CBC_HB_LBORRES,rep(1,nrow(df2))) ~ M06_SPHB_LBORRES + frailty(MOMID,dist="gamma"),data = df2)
risk_scores <- predict(model2, newdata = df2, type = "risk")
c_index_value <- rcorr.cens(x = risk_scores, S = Surv(df2$M08_CBC_HB_LBORRES, rep(1, nrow(df2))))
print(c_index_value)