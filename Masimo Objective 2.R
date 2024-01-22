#Define outcome variable for anemia
temp_df <- temp_df %>% filter(!is.na(M06_SPHB_LBORRES)&!is.na(M08_CBC_HB_LBORRES))%>%
  mutate(CBC_anemia=ifelse(M08_CBC_HB_LBORRES < 11,1,0),
         SPHB_anemia=ifelse(M06_SPHB_LBORRES < 11,1,0))

#Observation level contingency table
table(temp_df$CBC_anemia,temp_df$SPHB_anemia)

#bootstrap
PPA<-sum(temp_df$SPHB_anemia==1&temp_df$CBC_anemia==1)/sum(temp_df$CBC_anemia==1)
NPA<-sum(temp_df$SPHB_anemia==0&temp_df$CBC_anemia==0)/sum(temp_df$CBC_anemia==0)

#specify the population for bootstrap
temp_boot<-temp_df %>% 
  distinct (MOMID,.keep_all = TRUE)
n<-nrow(temp_boot)

#number of bootstrap
boot_n<-200

PPA_matrix<-NULL
NPA_matrix<-NULL

for (i in 1:boot_n) {
  boot_index<-sample(c(1:n),n,replace=TRUE)
  
  boot_df<-NULL
  for (j in boot_index) {
    index<-which(temp_df[,"MOMID"]==temp_boot[j,"MOMID"])
    boot_df<-rbind(temp_df[index,],boot_df)
  }
  
  PPA_boot<-sum(boot_df$SPHB_anemia==1&boot_df$CBC_anemia==1)/sum(boot_df$CBC_anemia==1)
  NPA_boot<-sum(boot_df$SPHB_anemia==0&boot_df$CBC_anemia==0)/sum(boot_df$CBC_anemia==0)
  
  PPA_matrix<-c(PPA_matrix,PPA_boot)
  NPA_matrix<-c(NPA_matrix,NPA_boot) 
}

#Bootstrap sample
se_NPA_boot<-sd(NPA_matrix)
se_PPA_boot<-sd(PPA_matrix)

#glmm
temp_df1 <- temp_df %>%
  filter(CBC_anemia==1) %>%
  mutate(Response=ifelse(SPHB_anemia==1,1,0))

temp_df0 <- temp_df %>%
  filter(CBC_anemia==0) %>%
  mutate(Response=ifelse(SPHB_anemia==0,1,0))

model1<-glmer(Response ~ (1|MOMID), data = temp_df1, family = binomial)
summary(model1)

model0<-glmer(Response ~ (1|MOMID), data = temp_df0, family = binomial)
summary(model0)

#point estimate and SE derived by delta method
point_NPA<-sigmoid(fixef(model0))
se_NPA<-sigmoid(fixef(model0))*(1-sigmoid(fixef(model0)))*sqrt(vcov(model0))

point_PPA<-sigmoid(fixef(model1))
se_PPA<-sigmoid(fixef(model1))*(1-sigmoid(fixef(model1)))*sqrt(vcov(model1))

Result1<-matrix(round(c(PPA,as.numeric(point_PPA),NPA,as.numeric(point_NPA)),4),1,4)
colnames(Result1)<-c("PPA","PPA_glmm","NPA","NPA_glmm")

Result2<-matrix(round(c(se_PPA_boot,as.numeric(se_PPA),se_NPA_boot,as.numeric(se_NPA)),6),1,4)
colnames(Result2)<-c("PPA_boot","PPA_glmm","NPA_boot","NPA_glmm")

#latex code output
library(xtable)
xtable(Result1,caption = "Point estimate of PPA and NPA",digits = 4)
xtable(Result2,caption = "Standard deviation of PPA and NPA",digits = 6)