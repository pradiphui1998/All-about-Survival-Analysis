library(foreign)
df1<- read.spss("F:/Actrec/New folder/gurudutt/dataset.sav",to.data.frame=TRUE, use.value.labels = TRUE)
attach(df1)
df1$Status_OS1=ifelse(df1$Status_OS=="alive",0,1)

library(ggsurvfit)
library(survminer)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(dplyr)


# overall survival
survfit(Surv(OS_m,Status_OS)~1,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~1,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv")

#------------------------------------------------------------------------------------
#OS for age
survfit(Surv(OS_m,Status_OS1)~Age_Group_med_55,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~Age_Group_med_55,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for Age",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("less than 45", "greater than 45"))

df1$Age_Group_med_55 <- factor(df1$Age_Group_med_55, levels = c("less than 45", "greater than 45"))
coxph(Surv(OS_m,Status_OS1==1)~Age_Group_med_55,data=df1) %>% tbl_regression(exp = TRUE)


#-------------------------------------------------------------------------------------
#OS for sex
survfit(Surv(OS_m,Status_OS1)~Sex,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~Sex,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for Sex",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("Male", "Female"))


df1$Sex<- factor(df1$Sex, levels = c("Male", "Female"))
coxph(Surv(OS_m,Status_OS1==1)~Sex,data=df1) %>% tbl_regression(exp = TRUE)



#--------------------------------------------------------------------------------------
# OS for ASA
survfit(Surv(OS_m,Status_OS1)~ASA,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~ASA,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for ASA",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("1", "2","3"))


df1$ASA<- factor(df1$ASA, levels = c("1", "2","3"))
coxph(Surv(OS_m,Status_OS1==1)~ASA,data=df1) %>% tbl_regression(exp = TRUE)

#-------------------------------------------------------------------------------------------
#OS for Blood Group Loss'
survfit(Surv(OS_m,Status_OS1)~Bloodloss_grp,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~Bloodloss_grp,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for Blood Loss",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("less than 500", "500 - 1000","greater than 1000"))


df1$Bloodloss_grp<- factor(df1$Bloodloss_grp, levels = c("less than 500", "500 - 1000","greater than 1000"))
coxph(Surv(OS_m,Status_OS1==1)~Bloodloss_grp,data=df1) %>% tbl_regression(exp = TRUE)

#------------------------------------------------------------------------------------------
#OS for T stage
survfit(Surv(OS_m,Status_OS1)~Tstage_group,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~Tstage_group,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for T stage",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("T1/T2", "T3/T4"))


df1$Tstage_group<- factor(df1$Tstage_group, levels = c("T1/T2", "T3/T4"))
coxph(Surv(OS_m,Status_OS1==1)~Tstage_group,data=df1) %>% tbl_regression(exp = TRUE)

#-----------------------------------------------------------------------------------------------
#OS for Margins
survfit(Surv(OS_m,Status_OS1)~Margins,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~Margins,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for Margins",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("free", "involved"))


df1$Margins<- factor(df1$Margins, levels = c("free", "involved"))
coxph(Surv(OS_m,Status_OS1==1)~Margins,data=df1) %>% tbl_regression(exp = TRUE)

#------------------------------------------------------------------------------------------
#OS for LVE
survfit(Surv(OS_m,Status_OS1)~LVE,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~LVE,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for LVE",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("present", "absent"))

#------------------------------------------------------------------------------------------
#OS for PNI
survfit(Surv(OS_m,Status_OS1)~PNI,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~PNI,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for PNI",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("present", "absent"))
df1$PNI<- factor(df1$PNI, levels = c("present", "absent"))
coxph(Surv(OS_m,Status_OS1==1)~PNI,data=df1) %>% tbl_regression(exp = TRUE)


#----------------------------------------------------------------------------------
# OS for N stage
survfit(Surv(OS_m,Status_OS1)~NSTAGE,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~NSTAGE,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for NSTAGE",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("N0", "N+"))
df1$NSTAGE<- factor(df1$NSTAGE, levels = c("N0", "N+"))
coxph(Surv(OS_m,Status_OS1==1)~NSTAGE,data=df1) %>% tbl_regression(exp = TRUE)

#----------------------------------------------------------------------------------
# OS for Adjacent Tissue
survfit(Surv(OS_m,Status_OS1)~AdjacentTissue,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~AdjacentTissue,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for AdjacentTissue",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("involved", "not involved","PanIN","Chronic pancreatitis"))
df1$AdjacentTissue<- factor(df1$AdjacentTissue, levels = c("involved", "not involved","PanIN","Chronic pancreatitis"))
coxph(Surv(OS_m,Status_OS1==1)~AdjacentTissue,data=df1) %>% tbl_regression(exp = TRUE)

#----------------------------------------------------------------------------------
# OS for Post.op.CA19.9.raised
survfit(Surv(OS_m,Status_OS1)~Post.op.CA19.9.raised,data=df1)
ggsurvplot(
  fit = survfit(Surv(OS_m,Status_OS1==1)~Post.op.CA19.9.raised,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for Post OP CA19 raised",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("Yes","No"))
df1$Post.op.CA19.9.raised<- factor(df1$Post.op.CA19.9.raised, levels = c("Yes","No"))
coxph(Surv(OS_m,Status_OS1==1)~Post.op.CA19.9.raised,data=df1) %>% tbl_regression(exp = TRUE)
