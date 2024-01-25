########point estimate####
#Define outcome variable for anemia
temp_df <- temp_df %>% filter(!is.na(M06_SPHB_LBORRES)&!is.na(M08_CBC_HB_LBORRES))%>%
  mutate(CBC_anemia=ifelse(M08_CBC_HB_LBORRES < 11 | M08_CBC_HB_LBORRES > 13,1,0),
         SPHB_anemia=ifelse(M06_SPHB_LBORRES < 11 | M06_SPHB_LBORRES > 13,1,0))

#Observation level contingency table
table(temp_df$CBC_anemia,temp_df$SPHB_anemia)

temp_df1 <- temp_df %>%
  filter(CBC_anemia==1) %>%
  mutate(Response=ifelse(SPHB_anemia==1,1,0))

temp_df0 <- temp_df %>%
  filter(CBC_anemia==0) %>%
  mutate(Response=ifelse(SPHB_anemia==0,1,0))

CBC0_uniq<-unique(temp_df0$MOMID)
CBC1_uniq<-unique(temp_df1$MOMID)

#NPA and PPA observational level
PPA<-sum(temp_df$SPHB_anemia==1&temp_df$CBC_anemia==1)/sum(temp_df$CBC_anemia==1)
NPA<-sum(temp_df$SPHB_anemia==0&temp_df$CBC_anemia==0)/sum(temp_df$CBC_anemia==0)

#NPA and PPA average of subject level
NPA_sub_matrix<-rep(0,length(CBC0_uniq))
for (i in 1:length(CBC0_uniq)){
  NPA_sub_matrix[i]<-1-sum(temp_df0[temp_df0[,"MOMID"]==CBC0_uniq[i],"SPHB_anemia"])/nrow(temp_df0[temp_df0[,"MOMID"]==CBC0_uniq[i],])
}
NPA_average<-mean(NPA_sub_matrix)

PPA_sub_matrix<-rep(0,length(CBC1_uniq))
for (i in 1:length(CBC1_uniq)){
  PPA_sub_matrix[i]<-sum(temp_df1[temp_df1[,"MOMID"]==CBC1_uniq[i],"SPHB_anemia"])/nrow(temp_df1[temp_df1[,"MOMID"]==CBC1_uniq[i],])
}
PPA_average<-mean(PPA_sub_matrix)

######bootstrap#########
#specify the population for bootstrap
temp_boot<-temp_df %>% 
  distinct (MOMID,.keep_all = TRUE)
n<-nrow(temp_boot)

#number of bootstrap
boot_n<-200

PPA_matrix<-NULL
PPA_average_matrix<-NULL
NPA_matrix<-NULL
NPA_average_matrix<-NULL

for (i in 1:boot_n) {
  boot_index<-sample(c(1:n),n,replace=TRUE)
  
  boot_df<-NULL
  for (j in boot_index) {
    index<-which(temp_df[,"MOMID"]==temp_boot[j,"MOMID"])
    boot_df<-rbind(temp_df[index,],boot_df)
  }
  
  PPA_boot<-sum(boot_df$SPHB_anemia==1&boot_df$CBC_anemia==1)/sum(boot_df$CBC_anemia==1)
  NPA_boot<-sum(boot_df$SPHB_anemia==0&boot_df$CBC_anemia==0)/sum(boot_df$CBC_anemia==0)
  
  boot_df1 <- boot_df %>%
    filter(CBC_anemia==1) %>%
    mutate(Response=ifelse(SPHB_anemia==1,1,0))
  
  boot_df0 <- boot_df %>%
    filter(CBC_anemia==0) %>%
    mutate(Response=ifelse(SPHB_anemia==0,1,0))
  
  CBC0_uniq<-unique(boot_df0$MOMID)
  CBC1_uniq<-unique(boot_df1$MOMID)
  
  NPA_sub_matrix<-rep(0,length(CBC0_uniq))
  for (i in 1:length(CBC0_uniq)){
    NPA_sub_matrix[i]<-1-sum(boot_df0[boot_df0[,"MOMID"]==CBC0_uniq[i],"SPHB_anemia"])/nrow(boot_df0[boot_df0[,"MOMID"]==CBC0_uniq[i],])
  }
  NPA_average<-mean(NPA_sub_matrix)
  
  PPA_sub_matrix<-rep(0,length(CBC1_uniq))
  for (i in 1:length(CBC1_uniq)){
    PPA_sub_matrix[i]<-sum(boot_df1[boot_df1[,"MOMID"]==CBC1_uniq[i],"SPHB_anemia"])/nrow(boot_df1[boot_df1[,"MOMID"]==CBC1_uniq[i],])
  }
  PPA_average<-mean(PPA_sub_matrix)
  
  PPA_average_matrix<-c(PPA_average_matrix,PPA_average)
  PPA_matrix<-c(PPA_matrix,PPA_boot)
  NPA_average_matrix<-c(NPA_average_matrix,NPA_average)
  NPA_matrix<-c(NPA_matrix,NPA_boot) 
}

#Bootstrap sample
se_NPA_boot<-sd(NPA_matrix)
se_PPA_boot<-sd(PPA_matrix)

se_NPA_average_boot<-sd(NPA_average_matrix)
se_PPA_average_boot<-sd(PPA_average_matrix)

##########glmm########
#random intercept for MOMID
model1<-glmer(Response ~ (1|MOMID), data = temp_df1, family = binomial)
summary(model1)

model0<-glmer(Response ~ (1|MOMID), data = temp_df0, family = binomial)
summary(model0)

#random intercept for MOMID and SITE
model1_site<-glmer(Response ~ (1|MOMID)+(1|SITE), data = temp_df1, family = binomial)
summary(model1_site)

model0_site<-glmer(Response ~ (1|MOMID)+(1|SITE), data = temp_df0, family = binomial)
summary(model0_site)

#point estimate and SE derived by delta method
point_NPA<-sigmoid(fixef(model0))
se_NPA<-sigmoid(fixef(model0))*(1-sigmoid(fixef(model0)))*sqrt(vcov(model0))

point_PPA<-sigmoid(fixef(model1))
se_PPA<-sigmoid(fixef(model1))*(1-sigmoid(fixef(model1)))*sqrt(vcov(model1))

point_NPA_site<-sigmoid(fixef(model0_site))
se_NPA_site<-sigmoid(fixef(model0_site))*(1-sigmoid(fixef(model0_site)))*sqrt(vcov(model0_site))

point_PPA_site<-sigmoid(fixef(model1_site))
se_PPA_site<-sigmoid(fixef(model1_site))*(1-sigmoid(fixef(model1_site)))*sqrt(vcov(model1_site))

###confidence interval####
ci_low_PPA_boot<-PPA-1.96*se_PPA_boot
ci_low_PPA_average_boot<-PPA_average-1.96*se_PPA_average_boot
ci_low_point_PPA<-as.numeric(point_PPA)-1.96*as.numeric(se_PPA)
ci_low_PPA_site<-as.numeric(point_PPA_site)-1.96*as.numeric(se_PPA_site)

ci_up_PPA_boot<-PPA+1.96*se_PPA_boot
ci_up_PPA_average_boot<-PPA_average+1.96*se_PPA_average_boot
ci_up_point_PPA<-as.numeric(point_PPA)+1.96*as.numeric(se_PPA)
ci_up_PPA_site<-as.numeric(point_PPA_site)+1.96*as.numeric(se_PPA_site)

ci_low_NPA_boot<-NPA-1.96*se_NPA_boot
ci_low_NPA_average_boot<-NPA_average-1.96*se_NPA_average_boot
ci_low_point_NPA<-as.numeric(point_NPA)-1.96*as.numeric(se_NPA)
ci_low_NPA_site<-as.numeric(point_NPA_site)-1.96*as.numeric(se_NPA_site)

ci_up_NPA_boot<-NPA+1.96*se_NPA_boot
ci_up_NPA_average_boot<-NPA_average+1.96*se_NPA_average_boot
ci_up_point_NPA<-as.numeric(point_NPA)+1.96*as.numeric(se_NPA)
ci_up_NPA_site<-as.numeric(point_NPA_site)+1.96*as.numeric(se_NPA_site)
######Result########
Result1<-matrix(round(c(PPA,PPA_average,as.numeric(point_PPA),as.numeric(point_PPA_site),NPA,NPA_average,as.numeric(point_NPA),as.numeric(point_NPA_site)),3),2,4,byrow = TRUE)
colnames(Result1)<-c("observation","subject","glmm(subject)","glmm(subject, site)")
rownames(Result1)<-c("PPA","NPA")

Result2<-matrix(round(c(se_PPA_boot,se_PPA_average_boot,as.numeric(se_PPA),as.numeric(se_PPA_site),se_NPA_boot,se_NPA_average_boot,as.numeric(se_NPA),as.numeric(se_NPA_site)),4),2,4,byrow = TRUE)
colnames(Result2)<-c("observation","subject","glmm(subject)","glmm(subject, site)")
rownames(Result2)<-c("PPA","NPA")

Result3<-matrix(0,2,4,byrow = TRUE)
Result3[1,1]<-paste0("(",round(ci_low_PPA_boot,digits=3),",",round(ci_up_PPA_boot,digits=3),")")
Result3[1,2]<-paste0("(",round(ci_low_PPA_average_boot,digits=3),",",round(ci_up_PPA_average_boot,digits=3),")")
Result3[1,3]<-paste0("(",round(ci_low_point_PPA,digits=3),",",round(ci_up_point_PPA,digits=3),")")
Result3[1,4]<-paste0("(",round(ci_low_PPA_site,digits=3),",",round(ci_up_PPA_site,digits=3),")")
Result3[2,1]<-paste0("(",round(ci_low_NPA_boot,digits=3),",",round(ci_up_NPA_boot,digits=3),")")
Result3[2,2]<-paste0("(",round(ci_low_NPA_average_boot,digits=3),",",round(ci_up_NPA_average_boot,digits=3),")")
Result3[2,3]<-paste0("(",round(ci_low_point_NPA,digits=3),",",round(ci_up_point_NPA,digits=3),")")
Result3[2,4]<-paste0("(",round(ci_low_NPA_site,digits=3),",",round(ci_up_NPA_site,digits=3),")")

colnames(Result3)<-c("observation","subject","glmm(subject)","glmm(subject, site)")
rownames(Result3)<-c("PPA","NPA")

#latex code output
library(xtable)
xtable(Result1,caption = "Point estimate of PPA and NPA",digits = 3)
xtable(Result2,caption = "Standard deviation of PPA and NPA",digits = 4)
xtable(Result3,caption = "Confidence interval of PPA and NPA",digits = 4)
