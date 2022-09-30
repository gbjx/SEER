rm(list = ls())
options(stringsAsFactors = FALSE)
getwd()
library(dplyr)
library(tidyr)
load('./script/liver.Rdata')
table(seer$`Derived AJCC N, 7th ed (2010-2015)`)
table(seer$`Derived AJCC M, 7th ed (2010-2015)`)

seer$ajcc8<-ifelse(seer$`Derived AJCC N, 7th ed (2010-2015)`=="N1","IV","A")
seer$ajcc8<-ifelse(seer$`Derived AJCC M, 7th ed (2010-2015)`==" M1","IV",seer$ajcc8)
table(seer$`CS tumor size (2004-2015)`)
table(seer$nodules)
table( seer$vascular_invasion)

table(seer$ajcc8)

seer$ajcc8<-ifelse(seer$`CS tumor size (2004-2015)`<=20 & seer$nodules=="one" & 
                     seer$`Derived AJCC N, 7th ed (2010-2015)`!="N1","I",seer$ajcc8)

seer$ajcc8<-ifelse(seer$`CS tumor size (2004-2015)`>20 & seer$nodules=="one" & seer$vascular_invasion=="no" & 
                     seer$`Derived AJCC N, 7th ed (2010-2015)`!="N1","I",seer$ajcc8)

seer$ajcc8<-ifelse(seer$`CS tumor size (2004-2015)`>20 & seer$nodules=="one" & seer$vascular_invasion=="yes" & 
                     seer$`Derived AJCC N, 7th ed (2010-2015)`!="N1","II",seer$ajcc8)

seer$ajcc8<-ifelse(seer$`CS tumor size (2004-2015)`<50 & seer$nodules=="more"  & 
                     seer$`Derived AJCC N, 7th ed (2010-2015)`!="N1","II",seer$ajcc8)

seer$ajcc8<-ifelse(seer$`CS tumor size (2004-2015)`>=50 & seer$nodules=="more"  & 
                     seer$`Derived AJCC N, 7th ed (2010-2015)`!="N1","III",seer$ajcc8)

table(seer$vascular_invasion)
table(seer$`Derived AJCC M, 7th ed (2010-2015)`)

seer$bclc<-ifelse(seer$nodules=="one" & seer$vascular_invasion=="no" & seer$`Derived AJCC M, 7th ed (2010-2015)`=="M0",
                  "I","A")
table(seer$bclc)

seer$bclc<-ifelse(seer$nodules=="more" & seer$`CS tumor size (2004-2015)`<=30 & 
                    seer$vascular_invasion=="no" & seer$`Derived AJCC M, 7th ed (2010-2015)`=="M0","I",seer$bclc)

table(seer$bclc) 

seer$bclc<-ifelse(seer$nodules=="more" & seer$`CS tumor size (2004-2015)`>30 & 
                    seer$vascular_invasion=="no" & seer$`Derived AJCC M, 7th ed (2010-2015)`=="M0","II",seer$bclc)
table(seer$bclc) 

seer$bclc<-ifelse(seer$vascular_invasion=="yes" | seer$`Derived AJCC M, 7th ed (2010-2015)`=="M1","III",seer$bclc)
table(seer$bclc) 

seer<-select(seer,-1)
plot(table(seer$`Year of diagnosis`)) 

##
##{2010 2011 2012 2013 2014 2015 
##202  206  203  223  228  263 }

seer<-select(seer,-1)

table(seer$`CS version derived (2004-2015)`)
seer<-select(seer,-1)

table(seer$`Behavior code ICD-O-3`)
seer<-select(seer,-1)


table(seer$Grade)
table(seer$`CS tumor size (2004-2015)`)
table(seer$`CS extension (2004-2015)`)
seer<-select(seer,-`CS extension (2004-2015)`)
table(seer$`CS lymph nodes (2004-2015)`)
table(seer$`CS mets at dx (2004-2015)`)
seer<-select(seer,-`CS mets at dx (2004-2015)`)
table(seer$`CS Mets Eval (2004-2015)`)
seer<-select(seer,-`CS Mets Eval (2004-2015)`)

table(seer$`RX Summ--Surg Prim Site (1998+)`)
seer$`RX Summ--Surg Prim Site (1998+)`<-factor(seer$`RX Summ--Surg Prim Site (1998+)`,
                                               levels = c("RFA","segmental_resection","Hepatectomy","transplant"),labels = c(1,2,3,4),ordered = T)

table(seer$`Type of Reporting Source`)
seer<-select(seer,-`Type of Reporting Source`)

table(seer$`Patient ID`)
seer<-select(seer,-`Patient ID`)

table(seer$`RX Summ--Surg/Rad Seq`)##是否有放疗
seer$`RX Summ--Surg/Rad Seq`<-ifelse(seer$`RX Summ--Surg/Rad Seq`=="No radiation and/or cancer-directed surgery",1,2) 

table(seer$`Chemotherapy recode (yes, no/unk)`)##是否有化疗
seer$`Chemotherapy recode (yes, no/unk)`<-ifelse(seer$`Chemotherapy recode (yes, no/unk)`=="Yes",2,1) 

table(seer$`RX Summ--Systemic/Sur Seq`)##是否有系统治疗
seer<-subset(seer,seer$`RX Summ--Systemic/Sur Seq` !="Sequence unknown")
seer$`RX Summ--Systemic/Sur Seq`<-ifelse(seer$`RX Summ--Systemic/Sur Seq`=="No systemic therapy and/or surgical procedures",1,2)

table(seer$`Marital status at diagnosis`)##婚姻状况
seer<-subset(seer,seer$`Marital status at diagnosis` !="Unknown")
seer$`Marital status at diagnosis`<-factor(seer$`Marital status at diagnosis`,
                                           levels = c("Single (never married)","Unmarried or Domestic Partner","Separated",
                                                      "Widowed","Divorced","Married (including common law)"),
                                           labels = c(1:6),ordered = T)

table(seer$`Race recode (White, Black, Other)`)##种族

seer$`Race recode (White, Black, Other)`<-ifelse(seer$`Race recode (White, Black, Other)`=="Black",1,
                                                 seer$`Race recode (White, Black, Other)`)
seer$`Race recode (White, Black, Other)`<-ifelse(seer$`Race recode (White, Black, Other)`=="Other (American Indian/AK Native, Asian/Pacific Islander)",2,
                                                 seer$`Race recode (White, Black, Other)`)
seer$`Race recode (White, Black, Other)`<-ifelse(seer$`Race recode (White, Black, Other)`=="White",3,
                                                 seer$`Race recode (White, Black, Other)`)


table(seer$`Age recode with single ages and 100+`)
table(seer$`Race/ethnicity`)
seer<-select(seer,-`Race/ethnicity`)

table(seer$`Derived AJCC Stage Group, 7th ed (2010-2015)`)
table(seer$`Derived AJCC T, 7th ed (2010-2015)`)
table(seer$`Derived AJCC N, 7th ed (2010-2015)`)
table(seer$`Derived AJCC M, 7th ed (2010-2015)`)


table(seer$`COD to site recode`)
seer$`COD to site recode`<-ifelse(seer$`COD to site recode`=="Liver","Dead",seer$`COD to site recode`)

table(seer$`SEER cause-specific death classification`)
seer<-select(seer,-`SEER cause-specific death classification`)

table(seer$`Survival months`)
table(seer$`Survival months flag`)
seer<-select(seer,-`Survival months flag`)

table(seer$`COD to site rec KM`)
seer<-select(seer,-`COD to site rec KM`)


table(seer$`Vital status recode (study cutoff used)`)
seer<-select(seer,-`Vital status recode (study cutoff used)`)

table(seer$`First malignant primary indicator`)
seer<-select(seer,-`First malignant primary indicator`)


table(seer$`Total number of in situ/malignant tumors for patient`)
table(seer$`CS Tumor Size/Ext Eval (2004-2015)`)
seer<-select(seer,-`CS Tumor Size/Ext Eval (2004-2015)`)

table(seer$nodules)
table(seer$lobe)
table(seer$vascular_invasion)
table(seer$AFP)
table(seer$Fibrosis_score)
seer$nodules<-factor(seer$nodules,levels = c("one","more"),labels = c(1,2),ordered = T)
seer$vascular_invasion<-factor(seer$vascular_invasion,levels = c("no","yes"),labels = c(1,2),ordered = T)
seer$AFP<-factor(seer$AFP,levels = c("Negative","Positive"),labels = c(1,2),ordered = T)
seer$Fibrosis_score<-factor(seer$Fibrosis_score,levels = c("moderate_fibrosis","Severe_fibrosis"),labels = c(1,2),ordered = T)
seer$lobe<-factor(seer$lobe,levels = c("one","more"),labels = c(1,2),ordered = T)

colnames(seer)
names(seer)<-c("garde","tumor_size","lymph_nodes","surging","surg_rad","Chemotherapy","Systemic_treatment","maritalstatus",
               "Race","Age","Stage","T","N","M","label","Survival_months","number_of_tumors","nodules",
               "lobe","vascular_invasion","AFP","Fibrosis_score","ajcc8","bclc")

                       
table(seer$Stage)
table(seer$T)
seer$T<-ifelse(seer$T %in% c("T3a","T3b","T3NOS"),"T3",seer$T)
seer$T<-factor(seer$T,levels = c("T1","T2", "T3","T4"),labels = c(1:4),ordered = T)
table(seer$N)
seer$N<-factor(seer$N,levels = c("N0","N1"),labels = c(1:2),ordered = T)
table(seer$M)
seer$M<-factor(seer$M,levels = c("M0","M1"),labels = c(1:2),ordered = T)
seer$Stage<-ifelse(seer$Stage %in% c("IIIA","IIIB","IIIC","IIINOS"),"III",seer$Stage)
seer$Stage<-ifelse(seer$Stage %in% c("IVA","IVB"),"IV",seer$Stage)
table(seer$Stage)
seer$Stage<-factor(seer$Stage,levels = c("I","II","III","IV"),labels = c(1:4),ordered = TRUE)

table(seer$ajcc8)
seer$ajcc8<-factor(seer$ajcc8,levels = c("I","II","III","IV"),labels = c(1:4),ordered = TRUE)

table(seer$bclc)
seer$bclc<-factor(seer$bclc,levels = c("I","II","III"),labels = c(1:3),ordered = TRUE)

seer_autoscor<-seer
class(seer_autoscor)
str(seer_autoscor)


seer_autoscor<-select(seer_autoscor,c(1:14,16:24),"label")




for(i in names(seer_autoscor)[1:23]){seer_autoscor[,i]<-as.numeric(seer_autoscor[,i])}
str(seer_autoscor)
seer_autoscor$label<-as.factor(seer_autoscor$label)
str(seer_autoscor)
save(seer_autoscor,file = './script/seer_autoscor.Rdata')

#三线表
rm(list = ls())
load('./script/seer_autoscor.Rdata')
library(tableone)
dput(names(seer_autoscor))

myvars<-c("garde", "tumor_size", "lymph_nodes", "surging", "surg_rad", 
          "Chemotherapy", "Systemic_treatment", "maritalstatus", "Race", 
          "Age", "Stage", "T", "N", "M", "Survival_months", "number_of_tumors", 
          "nodules", "lobe", "vascular_invasion", "AFP", "Fibrosis_score","ajcc8", "bclc", 
          "label")

## 需要转为分类变量的变量
catVars <- c("garde",  "lymph_nodes", "surging", "surg_rad", 
             "Chemotherapy", "Systemic_treatment", "maritalstatus", "Race", 
              "Stage", "T", "N", "M", "number_of_tumors",  
             "nodules", "lobe", "vascular_invasion", "AFP", "Fibrosis_score", "ajcc8", "bclc",
             "label")
summary(seer_autoscor)
#library(modelsummary)


tab <- CreateTableOne(vars = myvars, data = seer_autoscor, factorVars = catVars)
print(tab, showAllLevels = TRUE)

save(tab,file = 'result/tableforall.Rdata')

load('./result/tableforall.Rdata')
print(tab)
rm(list = ls())
