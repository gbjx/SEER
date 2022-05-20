rm(list = ls())
options(stringsAsFactors = FALSE)
getwd()
library(data.table)
library(dplyr)
library(tidyr)
liver<-fread('./seerliver.txt',data.table = F)
seer<-liver

head(seer)

colnames(seer)

##
table(seer$`Type of Reporting Source`)
seer<-subset(seer,seer$`Type of Reporting Source` !="Autopsy only")
seer<-subset(seer,seer$`Type of Reporting Source` !="Death certificate only")
table(seer$`Type of Reporting Source`)

##
table(seer$`Histology ICD-O-2`)
seer<-subset(seer,seer$`Histology ICD-O-2` !="9713")
seer<-subset(seer,seer$`Histology ICD-O-2` !="9715")
seer<-subset(seer,seer$`Histology ICD-O-2` !="9720")
seer<-subset(seer,seer$`Histology ICD-O-2` !="9723")
seer<-subset(seer,seer$`Histology ICD-O-2` !="9821")
table(seer$`Histology ICD-O-2`)

##
table(seer$`Year of diagnosis`)

##
table(seer$`First malignant primary indicator`)
seer<-subset(seer,seer$`First malignant primary indicator`=="Yes")

##
table(seer$`CS version derived (2004-2015)`)  ##Blank(s)时TNM为空白。
seer<-subset(seer,seer$`CS version derived (2004-2015)`=="20550")
table(seer$`CS version derived (2004-2015)`)

##
table(seer$`Behavior code ICD-O-3`)


##
table(seer$`RX Summ--Surg Prim Site (1998+)`)
seer<-subset(seer,seer$`RX Summ--Surg Prim Site (1998+)` %in% c(16,20:26,30:38,50:61))

table(seer$`RX Summ--Surg Prim Site (1998+)`)
seer$`RX Summ--Surg Prim Site (1998+)`<-ifelse(seer$`RX Summ--Surg Prim Site (1998+)`==16,"RFA",seer$`RX Summ--Surg Prim Site (1998+)`)
seer$`RX Summ--Surg Prim Site (1998+)`<-ifelse(seer$`RX Summ--Surg Prim Site (1998+)` %in% c(20:26),"segmental_resection",seer$`RX Summ--Surg Prim Site (1998+)`)
seer$`RX Summ--Surg Prim Site (1998+)`<-ifelse(seer$`RX Summ--Surg Prim Site (1998+)` %in% c(30:60),"Hepatectomy",seer$`RX Summ--Surg Prim Site (1998+)`)
seer$`RX Summ--Surg Prim Site (1998+)`<-ifelse(seer$`RX Summ--Surg Prim Site (1998+)`==61,"transplant",seer$`RX Summ--Surg Prim Site (1998+)`)
table(seer$`RX Summ--Surg Prim Site (1998+)`)


##
table(seer$`Vital status recode (study cutoff used)`)
table(seer$`SEER other cause of death classification`)
seer<-subset(seer,seer$`SEER other cause of death classification`=="Alive or dead due to cancer") ##生存与死亡都与疾病相关
seer<-select(seer,-"SEER other cause of death classification")

##
table(seer$Grade)
seer<-subset(seer,seer$Grade !="B-cell; pre-B; B-precursor")
seer<-subset(seer,seer$Grade !="NK cell; natural killer cell (1995+)")
seer<-subset(seer,seer$Grade !="Null cell; non T-non B")
seer<-subset(seer,seer$Grade !="T-cell")
seer<-subset(seer,seer$Grade !="Unknown")
table(seer$Grade)

seer$Grade<-ifelse(seer$Grade=="Well differentiated; Grade I","1",seer$Grade)
seer$Grade<-ifelse(seer$Grade=="Moderately differentiated; Grade II","2",seer$Grade)
seer$Grade<-ifelse(seer$Grade=="Poorly differentiated; Grade III","3",seer$Grade)
seer$Grade<-ifelse(seer$Grade=="Undifferentiated; anaplastic; Grade IV","4",seer$Grade)
table(seer$Grade)






##
table(seer$`Age recode with <1 year olds`)
table(seer$`Age recode with single ages and 100+`)
table(seer$`Age recode with single ages and 85+`)
seer<-select(seer,-"Age recode with <1 year olds")
seer<-select(seer,-"Age recode with single ages and 85+")
table(seer$`Age recode with single ages and 100+`)
#seer<-subset(seer,seer$`Age recode with single ages and 100+` !="100+ years")
seer<-separate(seer,"Age recode with single ages and 100+",into = c("Age recode with single ages and 100+","year"),
               sep = " ",remove = T )
table(seer$`Age recode with single ages and 100+`)
seer<-select(seer,-year)
seer$`Age recode with single ages and 100+`<-as.numeric(seer$`Age recode with single ages and 100+`)
seer<-subset(seer,seer$`Age recode with single ages and 100+`>=18)
table(seer$`Age recode with single ages and 100+`)

##
table(seer$`Lymph-vascular Invasion (2004+ varying by schema)`)

seer<-select(seer,-"Lymph-vascular Invasion (2004+ varying by schema)")

##
table(seer$`CS tumor size (2004-2015)`)
seer$`CS tumor size (2004-2015)`<-as.numeric(seer$`CS tumor size (2004-2015)`)
seer<-subset(seer,seer$`CS tumor size (2004-2015)` <990)


##
#{
table(seer$`CS extension (2004-2015)`)
#seer$`CS extension (2004-2015)`<-as.numeric(seer$`CS extension (2004-2015)`)
#seer<-subset(seer,seer$`CS extension (2004-2015)` !=999)
#seer<-subset(seer,seer$`CS extension (2004-2015)` !=855)
#seer<-subset(seer,seer$`CS extension (2004-2015)` !=810)
#seer<-subset(seer,seer$`CS extension (2004-2015)` !=999)

#seer<-subset(seer,seer$`CS extension (2004-2015)` %in% c(100,150,200,250,300,350,370,380,390,400,420,440,
#                                                         510,520,530,540,550,560,630,635,639,640,650,
#                                                        660,670,700,750,755,760,770,800))
#
seer$nodules<-ifelse(seer$`CS extension (2004-2015)` %in% c(100,150,200,250,350,370,380,510,520,530,540,550),"one","more")
seer$lobe<-ifelse(seer$`CS extension (2004-2015)` %in% c(100,150,200,300,350,370,390,400,420,440,510,520),"one","more")
seer$vascular_invasion<-ifelse(seer$`CS extension (2004-2015)` %in% c(200,300,350,370,380,390,400,420,520,630,635,660),"yes","no")



##
table(seer$`CS lymph nodes (2004-2015)`)
seer<-subset(seer,seer$`CS lymph nodes (2004-2015)` !=999)
seer<-subset(seer,seer$`CS lymph nodes (2004-2015)` !=988)
seer<-subset(seer,seer$`CS lymph nodes (2004-2015)` !=500)
seer<-subset(seer,seer$`CS lymph nodes (2004-2015)` !=310)
seer<-subset(seer,seer$`CS lymph nodes (2004-2015)` !=140)
seer<-subset(seer,seer$`CS lymph nodes (2004-2015)` !=130)
seer<-subset(seer,seer$`CS lymph nodes (2004-2015)` !=120)
seer<-subset(seer,seer$`CS lymph nodes (2004-2015)` !=110)
table(seer$`CS lymph nodes (2004-2015)`)
seer$`CS lymph nodes (2004-2015)`<-as.numeric(seer$`CS lymph nodes (2004-2015)`)
seer$`CS lymph nodes (2004-2015)`<-ifelse(seer$`CS lymph nodes (2004-2015)`>0,"1","0")
table(seer$`CS lymph nodes (2004-2015)`)


##
table(seer$`Regional nodes positive (1988+)`)
seer<-select(seer,-`Regional nodes positive (1988+)`)


##
table(seer$`Regional nodes examined (1988+)`)
seer<-select(seer,-`Regional nodes examined (1988+)`)


##
table(seer$`CS Mets Eval (2004-2015)`)#远处转移


##
table(seer$`CS site-specific factor 1 (2004+ varying by schema)`)
str(seer$`CS site-specific factor 1 (2004+ varying by schema)`)

seer<-subset(seer,seer$`CS site-specific factor 1 (2004+ varying by schema)`%in% c("10","20"))
seer$`CS site-specific factor 1 (2004+ varying by schema)`<-as.numeric(seer$`CS site-specific factor 1 (2004+ varying by schema)`)
table(seer$`CS site-specific factor 1 (2004+ varying by schema)`)
seer$AFP<-ifelse(seer$`CS site-specific factor 1 (2004+ varying by schema)`==10,"Positive","Negative")
table(seer$AFP)
seer<-select(seer,-`CS site-specific factor 1 (2004+ varying by schema)`)


##
table(seer$`CS site-specific factor 2 (2004+ varying by schema)`)
seer<-subset(seer,seer$`CS site-specific factor 2 (2004+ varying by schema)` %in% c("0","1"))
seer$Fibrosis_score<-as.numeric(seer$`CS site-specific factor 2 (2004+ varying by schema)`)
seer$Fibrosis_score<-ifelse(seer$Fibrosis_score==0,"moderate_fibrosis","Severe_fibrosis")
table(seer$Fibrosis_score)
seer<-select(seer,-`CS site-specific factor 2 (2004+ varying by schema)`)

##
table(seer$`CS site-specific factor 3 (2004+ varying by schema)`)
seer<-select(seer,-`CS site-specific factor 3 (2004+ varying by schema)`)


##
table(seer$`CS site-specific factor 4 (2004+ varying by schema)`)
table(seer$`CS site-specific factor 5 (2004+ varying by schema)`)
table(seer$`CS site-specific factor 6 (2004+ varying by schema)`)
table(seer$`CS site-specific factor 7 (2004+ varying by schema)`)
table(seer$`CS site-specific factor 8 (2004+ varying by schema)`)
seer<-select(seer,-c(11:15))

##
table(seer$`RX Summ--Surg Prim Site (1998+)`)

##
table(seer$`RX Summ--Scope Reg LN Sur (2003+)`)
table(seer$`RX Summ--Surg Oth Reg/Dis (2003+)`)
table(seer$`Reason no cancer-directed surgery`)
seer<-select(seer,-c(12:14))
table(seer$`Type of Reporting Source`)


##
table(seer$`Patient ID`)
table(seer$`RX Summ--Surg/Rad Seq`)
table(seer$`Radiation recode`)
seer<-select(seer,-`Radiation recode`)

table(seer$`Chemotherapy recode (yes, no/unk)`)


##
table(seer$`RX Summ--Systemic/Sur Seq`)##系统治疗


##
table(seer$`Marital status at diagnosis`)##婚姻状况

##
table(seer$`Race recode (White, Black, Other)`)
seer<-subset(seer,seer$`Race recode (White, Black, Other)` !="Unknown")


##
table(seer$`Derived AJCC Stage Group, 7th ed (2010-2015)`)
seer<-subset(seer,seer$`Derived AJCC Stage Group, 7th ed (2010-2015)` !="Blank(s)")
table(seer$`Derived AJCC T, 7th ed (2010-2015)`)
table(seer$`Derived AJCC N, 7th ed (2010-2015)`)
table(seer$`Derived AJCC M, 7th ed (2010-2015)`)

##
table(seer$`COD to site recode`)##死因
seer<-subset(seer,seer$`COD to site rec KM` %in% c("Alive","Liver"))
table(seer$`COD to site recode`)


##
table(seer$`SEER cause-specific death classification`)


##
table(seer$`Survival months`)
seer<-subset(seer,seer$`Survival months` !=0)

##
table(seer$`Survival months flag`)

table(seer$`COD to site rec KM`)
table(seer$`Vital status recode (study cutoff used)`)
table(seer$`First malignant primary indicator`)
table(seer$`Total number of in situ/malignant tumors for patient`)#肿瘤个数
table(seer$`CS Tumor Size/Ext Eval (2004-2015)`)
table(seer$nodules)
table(seer$vascular_invasion)
table(seer$AFP)
table(seer$Fibrosis_score)

save(seer,file = "./script/liver.Rdata")

rm(list = ls())

