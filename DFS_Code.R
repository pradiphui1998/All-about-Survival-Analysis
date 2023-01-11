library(foreign)
df1<- read.spss("F:/Actrec/New folder/gurudutt/dataset.sav",to.data.frame=TRUE, use.value.labels = TRUE)
attach(df1)


library(ggsurvfit)
library(survminer)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(dplyr)


# disease free survival
survfit(Surv(DFS_m,DFS_Status)~1,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~1,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv")

#------------------------------------------------------------------------------------
#OS for age
survfit(Surv(DFS_m,DFS_Status=="Yes")~Age_Group_med_55,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~Age_Group_med_55,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for Age",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("less than 45", "greater than 45"))

df1$Age_Group_med_55 <- factor(df1$Age_Group_med_55, levels = c("less than 45", "greater than 45"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~Age_Group_med_55,data=df1) %>% tbl_regression(exp = TRUE)


#-------------------------------------------------------------------------------------
#OS for sex
survfit(Surv(DFS_m,DFS_Status=="Yes")~Sex,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~Sex,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS  Survival for Sex",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("Male", "Female"))


df1$Sex<- factor(df1$Sex, levels = c("Male", "Female"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~Sex,data=df1) %>% tbl_regression(exp = TRUE)



#--------------------------------------------------------------------------------------
# OS for ASA
survfit(Surv(DFS_m,DFS_Status=="Yes")~ASA,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~ASA,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for ASA",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("1", "2","3"))


df1$ASA<- factor(df1$ASA, levels = c("1", "2","3"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~ASA,data=df1) %>% tbl_regression(exp = TRUE)

#-------------------------------------------------------------------------------------------
#OS for Blood Group Loss'
survfit(Surv(DFS_m,DFS_Status=="Yes")~Bloodloss_grp,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~Bloodloss_grp,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for Blood Loss",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("less than 500", "500 - 1000","greater than 1000"))


df1$Bloodloss_grp<- factor(df1$Bloodloss_grp, levels = c("less than 500", "500 - 1000","greater than 1000"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~Bloodloss_grp,data=df1) %>% tbl_regression(exp = TRUE)

#------------------------------------------------------------------------------------------
#OS for T stage
survfit(Surv(DFS_m,DFS_Status=="Yes")~Tstage_group,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~Tstage_group,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for T stage",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("T1/T2", "T3/T4"))


df1$Tstage_group<- factor(df1$Tstage_group, levels = c("T1/T2", "T3/T4"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~Tstage_group,data=df1) %>% tbl_regression(exp = TRUE)

#-----------------------------------------------------------------------------------------------
#OS for Margins
survfit(Surv(DFS_m,DFS_Status=="Yes")~Margins,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~Margins,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for Margins",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("free", "involved"))


df1$Margins<- factor(df1$Margins, levels = c("free", "involved"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~Margins,data=df1) %>% tbl_regression(exp = TRUE)

#------------------------------------------------------------------------------------------
#OS for LVE
survfit(Surv(DFS_m,DFS_Status=="Yes")~LVE,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~LVE,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for LVE",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("present", "absent"))

df1$LVE<- factor(df1$LVE, levels = c("present", "absent"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~LVE,data=df1) %>% tbl_regression(exp = TRUE)

#------------------------------------------------------------------------------------------
#OS for PNI
survfit(Surv(DFS_m,DFS_Status=="Yes")~PNI,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~PNI,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for PNI",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("present", "absent"))
df1$PNI<- factor(df1$PNI, levels = c("present", "absent"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~PNI,data=df1) %>% tbl_regression(exp = TRUE)


#----------------------------------------------------------------------------------
# OS for N stage
survfit(Surv(DFS_m,DFS_Status=="Yes")~NSTAGE,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~NSTAGE,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for NSTAGE",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("N0", "N+"))
df1$NSTAGE<- factor(df1$NSTAGE, levels = c("N0", "N+"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~NSTAGE,data=df1) %>% tbl_regression(exp = TRUE)

#----------------------------------------------------------------------------------
# OS for Adjacent Tissue
survfit(Surv(DFS_m,DFS_Status=="Yes")~AdjacentTissue,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~AdjacentTissue,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for AdjacentTissue",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("involved", "not involved","PanIN","Chronic pancreatitis"))
df1$AdjacentTissue<- factor(df1$AdjacentTissue, levels = c("involved", "not involved","PanIN","Chronic pancreatitis"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~AdjacentTissue,data=df1) %>% tbl_regression(exp = TRUE)

#----------------------------------------------------------------------------------
# OS for Post.op.CA19.9.raised
survfit(Surv(DFS_m,DFS_Status=="Yes")~Post.op.CA19.9.raised,data=df1)
ggsurvplot(
  fit = survfit(Surv(DFS_m,DFS_Status=="Yes")~Post.op.CA19.9.raised,data=df1),
  xlab = "Years",
  ylab = "Cumulative survival probability",
  title = "DFS Survival for Post OP CA19 raised",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("Yes","No"))
df1$Post.op.CA19.9.raised<- factor(df1$Post.op.CA19.9.raised, levels = c("Yes","No"))
coxph(Surv(DFS_m,DFS_Status=="Yes")~Post.op.CA19.9.raised,data=df1) %>% tbl_regression(exp = TRUE)
