rm(list = ls())
options(stringsAsFactors = FALSE)
load('./result/compar.Rdata')
library(dplyr)
library(tableone)
library(survival)
load('./script/seer_autoscor.Rdata')

dput(names(seer_autoscor))

seer_autoscor$pathid<-rownames(seer_autoscor)
seer_autoscor<-select(seer_autoscor,c("pathid","Stage","ajcc8", "bclc"))

train_for_survival<-merge(train_for_survival,seer_autoscor,by ="pathid", all.x = T)
testset_for_survival<-merge(testset_for_survival,seer_autoscor,by ="pathid", all.x = T)

train_for_survival<-select(train_for_survival,c(3:11,13:15))
testset_for_survival<-select(testset_for_survival,c(3:11,13:15))

train_for_survival$type<-c("trainset")
testset_for_survival$type<-c("testset")

alldata<-rbind(train_for_survival,testset_for_survival)

dput(names(alldata))
## 需要汇总的变量
myvars<-c("Label", "tumor_size", "Age", "surging", "garde", "maritalstatus", 
          "Race", "vascular_invasion", "Survival_months", "Stage","ajcc8", 
          "bclc", "type")

## 需要转为分类变量的变量
catVars <- c("Label","surging", "garde", "maritalstatus", 
             "Race", "vascular_invasion","Stage", "ajcc8", 
             "bclc","type")
tab <- CreateTableOne(vars = myvars, strata = "type",data = alldata, factorVars = catVars)
print(tab, showAllLevels = TRUE)
save(tab,file = 'result/table.Rdata')

load('./result/table.Rdata')
print(tab)

rm(list = ls())
