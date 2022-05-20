rm(list = ls())
options(stringsAsFactors = FALSE)
load('./result/compar.Rdata')
alldata<-rbind(testset_for_survival,train_for_survival)

##survival time
library(survival)
library(survminer)
library(ggsci)
alldata$Label<-ifelse(alldata$Label=="Alive",1,2)
table(alldata$Label)
str(alldata)

f1<-survfit(Surv(Survival_months, Label) ~ 1, data = alldata)
f1
summary(survfit(Surv(Survival_months, Label) ~ 1, data = alldata), times = 60)

plot(f1)
f2<-survfit(Surv(Survival_months, Label) ~ risk, data = alldata)
f2
plot(f2)
summary(survfit(Surv(Survival_months, Label) ~ risk, data = alldata), times = 60)

ggsurvplot(f2,
           pval = TRUE, conf.int = TRUE,
           risk.table = F, # Add risk table
           #risk.table.col = "strata",# Change risk table color by groups
           break.time.by = 12,
           xlab = "Time in Months",
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#B2182B","#2166AC"))

library(export)
graph2tif(file="result/survival for all")


