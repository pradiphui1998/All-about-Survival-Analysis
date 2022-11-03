## R-Code For Survival graph creating 
## Overall Survival

ggsurvplot(
  fit = survfit(Surv(truncated_OS,truncated_os_rec) ~rec_EOR, data =data1),
  xlab = "Months",
  ylab = "Cumulative survival probability",
  title = "Overall Survival for RT taken",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("GTR+NTR", "STR"))



## Progression Free Survival
ggsurvplot(
  fit = survfit(Surv(truncated_pfs, truncated_pfs_status) ~RT_taken, data =data1),
  xlab = "Months",
  ylab = "Cumulative survival probability",
  title = "Progression Free Survival for RT taken",
  risk.table = T,
  pval = TRUE,
  break.x.by = 12,
  surv.median.line = "hv",
  legend.labs = c("no", "yes"))




### For median finding value with confidence interval

#############Median survival,Survival probability and P value
survivalfunc<-defmacro(fitDFS, time, status, factor, expr=
                         {
                           fitDFS <- surv_fit(Surv(time,status==1) ~ factor ,data = data)
                           fitDFS
                           n<-as.data.frame(summary(fitDFS)$table)
                           n_new <- n[c(1,4,7,8,9)]
                           n_new
                           # p<-as.data.frame(surv_pvalue(fitDFS))
                           # p_value <- p[c(4)]
                           # out <- cbind(n_new,p_value)
                           # fitDFS <- prodlim(Hist(time,status)~factor,data=data)
                           # t<-as.data.frame(publish(fitDFS,times=c(12),org=TRUE))
                           # t_new <- t[c(1,5,6)]
                           # out <- cbind(n_new,t_new,p_value)
                           return(n_new)
                         })



attach(group3)                                                 
sink("compare.txt")
a=survivalfunc(fitDFS,group3$truncated_OS,group3$truncates_osstatus,group3$rec_EOR)
sink()



### For median survival rate for x years.
### OS
survfit(Surv(truncated_OS,truncated_os_rec)~age_group,data=data1)
summary(survfit(Surv(truncated_OS,truncated_os_rec)~age_group,data=data1),time=36)
summary(survfit(Surv(truncated_OS,truncated_os_rec)~age_group,data=data1),time=60)

##PFS
survfit(Surv(truncated_OS,truncated_pfs_status)~rec_EOR,data=data1)
summary(survfit(Surv(truncated_OS,truncated_pfs_status)~molecular_sugroup,data=data1),time=36)
summary(survfit(Surv(truncated_OS,truncated_pfs_status)~molecular_sugroup,data=data1),time=60)



#### For hazard ratio
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condSURV)

data1$molecular_sugroup <- factor(data1$molecular_sugroup, levels = c("WNT","SHH","group3","group4"))

coxph(Surv(truncated_pfs, truncated_pfs_status) ~molecular_sugroup, data =data1) %>% 
  tbl_regression(exp = TRUE) 




data1$rec_EOR <- factor(data1$rec_EOR, levels = c("GTR+NTR","STR"))

coxph(Surv(truncated_pfs, truncated_pfs_status) ~rec_EOR, data =data1) %>% 
  tbl_regression(exp = TRUE) 


####### Here we can give the reference group according to our need
data1$molecular_sugroup <- factor(data1$molecular_sugroup, levels = c("WNT","SHH","group3","group4"))

coxph(Surv(truncated_OS, truncated_os_rec) ~molecular_sugroup, data =data1) %>% 
  tbl_regression(exp = TRUE) 








