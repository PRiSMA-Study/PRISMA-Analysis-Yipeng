#PNC-0_Zambia
table1<-matrix(0,3,3)
table1[1,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind==1&df_infant_tcb_Zambia$TCB_15_PNC_1_ind==1,na.rm=TRUE)
table1[1,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind==0&df_infant_tcb_Zambia$TCB_15_PNC_1_ind==1,na.rm=TRUE)
table1[2,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind==1&df_infant_tcb_Zambia$TCB_15_PNC_1_ind==0,na.rm=TRUE)
table1[2,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind==0&df_infant_tcb_Zambia$TCB_15_PNC_1_ind==0,na.rm=TRUE)
table1[1,3]<-sum(table1[1,1],table1[1,2])
table1[2,3]<-sum(table1[2,1],table1[2,2])
table1[3,1]<-sum(table1[1,1],table1[2,1])
table1[3,2]<-sum(table1[1,2],table1[2,2])
table1[3,3]<-sum(table1[3,1],table1[3,2])
colnames(table1)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table1)<-c("TcB >= 15","TcB < 15", "Total")

table2<-matrix(0,3,3)
table2[1,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7==1,na.rm=TRUE)
table2[1,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7==1,na.rm=TRUE)
table2[2,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7==0,na.rm=TRUE)
table2[2,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7==0,na.rm=TRUE)
table2[1,3]<-sum(table2[1,1],table2[1,2])
table2[2,3]<-sum(table2[2,1],table2[2,2])
table2[3,1]<-sum(table2[1,1],table2[2,1])
table2[3,2]<-sum(table2[1,2],table2[2,2])
table2[3,3]<-sum(table2[3,1],table2[3,2])
colnames(table2)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table2)<-c("Jaundice present","Jaundice absent", "Total")


table3<-matrix(0,3,3)
table3[1,1]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_1_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7==1,na.rm=TRUE)
table3[1,2]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_1_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7==1,na.rm=TRUE)
table3[2,1]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_1_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7==0,na.rm=TRUE)
table3[2,2]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_1_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7==0,na.rm=TRUE)
table3[1,3]<-sum(table3[1,1],table3[1,2])
table3[2,3]<-sum(table3[2,1],table3[2,2])
table3[3,1]<-sum(table3[1,1],table3[2,1])
table3[3,2]<-sum(table3[1,2],table3[2,2])
table3[3,3]<-sum(table3[3,1],table3[3,2])
colnames(table3)<-c("TcB >= 15","TcB < 15", "Total")
rownames(table3)<-c("Jaundice present","Jaundice absent", "Total")

print(xtable(table1,digits=0,caption = "PNC-0-Zambia AAP threshold vs TcB $>$= 15"),table.placement="H")
print(xtable(table2,digits=0,caption = "PNC-0-Zambia AAP threshold vs Jaundice diagnosis"),table.placement="H")
print(xtable(table3,digits=0,caption = "PNC-0-Zambia TcB $>$= 15 vs Jaundice diagnosis"),table.placement="H")

#PNC-1_Zambia
table1<-matrix(0,3,3)
table1[1,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind==1&df_infant_tcb_Zambia$TCB_15_PNC_2_ind==1,na.rm=TRUE)
table1[1,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind==0&df_infant_tcb_Zambia$TCB_15_PNC_2_ind==1,na.rm=TRUE)
table1[2,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind==1&df_infant_tcb_Zambia$TCB_15_PNC_2_ind==0,na.rm=TRUE)
table1[2,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind==0&df_infant_tcb_Zambia$TCB_15_PNC_2_ind==0,na.rm=TRUE)
table1[1,3]<-sum(table1[1,1],table1[1,2])
table1[2,3]<-sum(table1[2,1],table1[2,2])
table1[3,1]<-sum(table1[1,1],table1[2,1])
table1[3,2]<-sum(table1[1,2],table1[2,2])
table1[3,3]<-sum(table1[3,1],table1[3,2])
colnames(table1)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table1)<-c("TcB >= 15","TcB < 15", "Total")

table2<-matrix(0,3,3)
table2[1,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8==1,na.rm=TRUE)
table2[1,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8==1,na.rm=TRUE)
table2[2,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8==0,na.rm=TRUE)
table2[2,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8==0,na.rm=TRUE)
table2[1,3]<-sum(table2[1,1],table2[1,2])
table2[2,3]<-sum(table2[2,1],table2[2,2])
table2[3,1]<-sum(table2[1,1],table2[2,1])
table2[3,2]<-sum(table2[1,2],table2[2,2])
table2[3,3]<-sum(table2[3,1],table2[3,2])
colnames(table2)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table2)<-c("Jaundice present","Jaundice absent", "Total")


table3<-matrix(0,3,3)
table3[1,1]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_2_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8==1,na.rm=TRUE)
table3[1,2]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_2_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8==1,na.rm=TRUE)
table3[2,1]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_2_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8==0,na.rm=TRUE)
table3[2,2]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_2_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8==0,na.rm=TRUE)
table3[1,3]<-sum(table3[1,1],table3[1,2])
table3[2,3]<-sum(table3[2,1],table3[2,2])
table3[3,1]<-sum(table3[1,1],table3[2,1])
table3[3,2]<-sum(table3[1,2],table3[2,2])
table3[3,3]<-sum(table3[3,1],table3[3,2])
colnames(table3)<-c("TcB >= 15","TcB < 15", "Total")
rownames(table3)<-c("Jaundice present","Jaundice absent", "Total")

print(xtable(table1,digits=0,caption = "PNC-1-Zambia AAP threshold vs TcB $>$= 15"),table.placement="H")
print(xtable(table2,digits=0,caption = "PNC-1-Zambia AAP threshold vs Jaundice diagnosis"),table.placement="H")
print(xtable(table3,digits=0,caption = "PNC-1-Zambia TcB $>$= 15 vs Jaundice diagnosis"),table.placement="H")

#PNC-4_Zambia
table1<-matrix(0,3,3)
table1[1,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_3_ind==1&df_infant_tcb_Zambia$TCB_15_PNC_3_ind==1,na.rm=TRUE)
table1[1,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_3_ind==0&df_infant_tcb_Zambia$TCB_15_PNC_3_ind==1,na.rm=TRUE)
table1[2,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_3_ind==1&df_infant_tcb_Zambia$TCB_15_PNC_3_ind==0,na.rm=TRUE)
table1[2,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_3_ind==0&df_infant_tcb_Zambia$TCB_15_PNC_3_ind==0,na.rm=TRUE)
table1[1,3]<-sum(table1[1,1],table1[1,2])
table1[2,3]<-sum(table1[2,1],table1[2,2])
table1[3,1]<-sum(table1[1,1],table1[2,1])
table1[3,2]<-sum(table1[1,2],table1[2,2])
table1[3,3]<-sum(table1[3,1],table1[3,2])
colnames(table1)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table1)<-c("TcB >= 15","TcB < 15", "Total")

table2<-matrix(0,3,3)
table2[1,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_3_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_9==1,na.rm=TRUE)
table2[1,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_3_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_9==1,na.rm=TRUE)
table2[2,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_3_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_9==0,na.rm=TRUE)
table2[2,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_PNC_3_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_9==0,na.rm=TRUE)
table2[1,3]<-sum(table2[1,1],table2[1,2])
table2[2,3]<-sum(table2[2,1],table2[2,2])
table2[3,1]<-sum(table2[1,1],table2[2,1])
table2[3,2]<-sum(table2[1,2],table2[2,2])
table2[3,3]<-sum(table2[3,1],table2[3,2])
colnames(table2)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table2)<-c("Jaundice present","Jaundice absent", "Total")


table3<-matrix(0,3,3)
table3[1,1]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_3_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_9==1,na.rm=TRUE)
table3[1,2]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_3_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_9==1,na.rm=TRUE)
table3[2,1]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_3_ind==1&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_9==0,na.rm=TRUE)
table3[2,2]<-sum(df_infant_tcb_Zambia$TCB_15_PNC_3_ind==0&df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_9==0,na.rm=TRUE)
table3[1,3]<-sum(table3[1,1],table3[1,2])
table3[2,3]<-sum(table3[2,1],table3[2,2])
table3[3,1]<-sum(table3[1,1],table3[2,1])
table3[3,2]<-sum(table3[1,2],table3[2,2])
table3[3,3]<-sum(table3[3,1],table3[3,2])
colnames(table3)<-c("TcB >= 15","TcB < 15", "Total")
rownames(table3)<-c("Jaundice present","Jaundice absent", "Total")

print(xtable(table1,digits=0,caption = "PNC-4-Zambia AAP threshold vs TcB $>$= 15"),table.placement="H")
print(xtable(table2,digits=0,caption = "PNC-4-Zambia AAP threshold vs Jaundice diagnosis"),table.placement="H")
print(xtable(table3,digits=0,caption = "PNC-4-Zambia TcB $>$= 15 vs Jaundice diagnosis"),table.placement="H")

#Birth outcome
table1<-matrix(0,3,3)
table1[1,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind==1&df_infant_tcb_Zambia$TCB_15_birth_outcome_ind==1,na.rm=TRUE)
table1[1,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind==0&df_infant_tcb_Zambia$TCB_15_birth_outcome_ind==1,na.rm=TRUE)
table1[2,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind==1&df_infant_tcb_Zambia$TCB_15_birth_outcome_ind==0,na.rm=TRUE)
table1[2,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind==0&df_infant_tcb_Zambia$TCB_15_birth_outcome_ind==0,na.rm=TRUE)
table1[1,3]<-sum(table1[1,1],table1[1,2])
table1[2,3]<-sum(table1[2,1],table1[2,2])
table1[3,1]<-sum(table1[1,1],table1[2,1])
table1[3,2]<-sum(table1[1,2],table1[2,2])
table1[3,3]<-sum(table1[3,1],table1[3,2])
colnames(table1)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table1)<-c("TcB >= 15","TcB < 15", "Total")

table2<-matrix(0,3,3)
table2[1,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind==1&df_infant_tcb_Zambia$M11_JAUND_CEOCCUR==1,na.rm=TRUE)
table2[1,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind==0&df_infant_tcb_Zambia$M11_JAUND_CEOCCUR==1,na.rm=TRUE)
table2[2,1]<-sum(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind==1&df_infant_tcb_Zambia$M11_JAUND_CEOCCUR==0,na.rm=TRUE)
table2[2,2]<-sum(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind==0&df_infant_tcb_Zambia$M11_JAUND_CEOCCUR==0,na.rm=TRUE)
table2[1,3]<-sum(table2[1,1],table2[1,2])
table2[2,3]<-sum(table2[2,1],table2[2,2])
table2[3,1]<-sum(table2[1,1],table2[2,1])
table2[3,2]<-sum(table2[1,2],table2[2,2])
table2[3,3]<-sum(table2[3,1],table2[3,2])
colnames(table2)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table2)<-c("Jaundice present","Jaundice absent", "Total")


table3<-matrix(0,3,3)
table3[1,1]<-sum(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind==1&df_infant_tcb_Zambia$M11_JAUND_CEOCCUR==1,na.rm=TRUE)
table3[1,2]<-sum(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind==0&df_infant_tcb_Zambia$M11_JAUND_CEOCCUR==1,na.rm=TRUE)
table3[2,1]<-sum(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind==1&df_infant_tcb_Zambia$M11_JAUND_CEOCCUR==0,na.rm=TRUE)
table3[2,2]<-sum(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind==0&df_infant_tcb_Zambia$M11_JAUND_CEOCCUR==0,na.rm=TRUE)
table3[1,3]<-sum(table3[1,1],table3[1,2])
table3[2,3]<-sum(table3[2,1],table3[2,2])
table3[3,1]<-sum(table3[1,1],table3[2,1])
table3[3,2]<-sum(table3[1,2],table3[2,2])
table3[3,3]<-sum(table3[3,1],table3[3,2])
colnames(table3)<-c("TcB >= 15","TcB < 15", "Total")
rownames(table3)<-c("Jaundice present","Jaundice absent", "Total")

print(xtable(table1,digits=0,caption = "Birth outcome-Zambia AAP threshold vs TcB $>$= 15"),table.placement="H")
print(xtable(table2,digits=0,caption = "Birth outcome-Zambia AAP threshold vs Jaundice diagnosis"),table.placement="H")
print(xtable(table3,digits=0,caption = "Birth outcome-Zambia TcB $>$= 15 vs Jaundice diagnosis"),table.placement="H")

###Percentage table
#Birth
paste0(sum(na.omit(df_infant_tcb_Zambia$M11_JAUND_CEOCCUR_6)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$M11_JAUND_CEOCCUR_6)==1)/length(na.omit(df_infant_tcb_Zambia$M11_JAUND_CEOCCUR_6))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind))*100,2),"%",")")

#PNC-0
paste0(sum(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7)==1)/length(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_1_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_1_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_1_ind))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind))*100,2),"%",")")

#PNC-1
paste0(sum(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8)==1)/length(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_2_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_2_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_2_ind))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind))*100,2),"%",")")

###Missing Percentage table
#Birth
paste0(length(na.omit(df_infant_tcb_Zambia$M11_JAUND_CEOCCUR_6)),"(",round(length(na.omit(df_infant_tcb_Zambia$M11_JAUND_CEOCCUR_6))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind)),"(",round(length(na.omit(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind)),"(",round(length(na.omit(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")

#PNC-0
paste0(length(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7)),"(",round(length(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_1_ind)),"(",round(length(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_1_ind))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind)),"(",round(length(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")

#PNC-1
paste0(length(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8)),"(",round(length(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_2_ind)),"(",round(length(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_2_ind))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind)),"(",round(length(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind))/nrow(df_infant_tcb_Zambia)*100,2),"%",")")

