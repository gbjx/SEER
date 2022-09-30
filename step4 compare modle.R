rm(list = ls())
options(stringsAsFactors = FALSE)
load('./result/compar.Rdata')
alldata<-rbind(testset_for_survival,train_for_survival)




load('./script/seer_autoscor.Rdata')
seer_autoscor$pathid<-rownames(seer_autoscor)
library(dplyr)
dput(names(seer_autoscor))
seer_autoscor<-select(seer_autoscor,c("pathid","Stage","ajcc8", "bclc"))
dataforcompare<-merge(alldata,seer_autoscor,by ="pathid", all.x = T)

library(survival)
library(ggDCA)
library(rms)

pbc<-na.omit(dataforcompare)
pbc$died<-pbc$Label=="Dead"
pbc$days<-as.numeric(pbc$Survival_months)
dd<-datadist(pbc)
options(datadist="dd")
options(na.action="na.delete")

#####生成模型
colnames(pbc)
units(pbc$days)<-"month"

##3年
coxmodle1<-cph(Surv(days,died)~ risk,data = pbc,x = T,y = T,surv = T,time.inc = 36)
cal1<-calibrate(coxmodle1,
                cmethod = "KM",
                method = "boot",
                u = 36,#与coxmodle1中time.inc一致
                m = 431,#约等于样本数除3
                B = 1000)
coxmodle2<-cph(Surv(days,died)~ Stage,data = pbc,x = T,y = T,surv = T,time.inc = 36)
cal2<-calibrate(coxmodle2,
                cmethod = "KM",
                method = "boot",
                u = 36,#与coxmodle1中time.inc一致
                m = 431,#约等于样本数除3
                B = 1000)

coxmodle3<-cph(Surv(days,died)~ ajcc8,data = pbc,x = T,y = T,surv = T,time.inc = 36)
cal3<-calibrate(coxmodle3,
                cmethod = "KM",
                method = "boot",
                u = 36,#与coxmodle1中time.inc一致
                m = 431,#约等于样本数除3
                B = 1000)

coxmodle4<-cph(Surv(days,died)~ bclc,data = pbc,x = T,y = T,surv = T,time.inc = 36)
cal4<-calibrate(coxmodle4,
                cmethod = "KM",
                method = "boot",
                u = 36,#与coxmodle1中time.inc一致
                m = 431,#约等于样本数除3
                B = 1000)

library("RColorBrewer")
display.brewer.all()

color<-brewer.pal(n = 4, name = "Set1")

plot(cal1,lwd=2,errbar.col =color[1],
     bty = "l", 
     lty=1,xlim=c(0,1),ylim=c(0,1),
     xlab = "nomogram predicted probability of 3 year OS",
     ylab = "actual 3 year OS",
     col = color[1],cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6)

lines(cal1[,c("mean.predicted","KM")],type="b",lwd=2,col=color[1],pch=16)



plot(cal2,lwd = 2,lty = 1,errbar.col = color[2],
     xlim = c(0,1),ylim= c(0,1),col = color[2],add = T)
lines(cal2[,c('mean.predicted',"KM")],
      type = 'b', lwd = 1, col = color[2], pch = 16)

plot(cal3,lwd = 2,lty = 1,errbar.col = color[3],
     xlim = c(0,1),ylim= c(0,1),col =color[3],add = T)
lines(cal3[,c('mean.predicted',"KM")],
      type = 'b', lwd = 1, col =color[3], pch = 16)

plot(cal4,lwd = 2,lty = 1,errbar.col = color[4],
     xlim = c(0,1),ylim= c(0,1),col =  color[4],add = T)
lines(cal4[,c('mean.predicted',"KM")],
      type = 'b', lwd = 1, col = color[4], pch = 16)


abline(0,1, lwd = 2, lty = 3, col = c("#224444"))

legend("topleft", #图例的位置
       legend = c("modle","AJCC7","AJCC8","BCLC"), #图例文字
       col =color, #图例线的颜色，与文字对应
       lwd = 2,#图例中线的粗细
       cex = 1.2,#图例字体大小
       bty = "n")#不显示图例边框
library(export)
graph2tif(file="result/3years calibration")
dev.off()

##5year
coxmodle5<-cph(Surv(days,died)~ risk,data = pbc,x = T,y = T,surv = T,time.inc = 60)
cal5<-calibrate(coxmodle5,
                cmethod = "KM",
                method = "boot",
                u = 60,#与coxmodle1中time.inc一致
                m = 431,#约等于样本数除3
                B = 1000)
coxmodle6<-cph(Surv(days,died)~ Stage,data = pbc,x = T,y = T,surv = T,time.inc = 60)
cal6<-calibrate(coxmodle6,
                cmethod = "KM",
                method = "boot",
                u = 60,#与coxmodle1中time.inc一致
                m = 431,#约等于样本数除3
                B = 1000)

coxmodle7<-cph(Surv(days,died)~ ajcc8,data = pbc,x = T,y = T,surv = T,time.inc = 60)
cal7<-calibrate(coxmodle7,
                cmethod = "KM",
                method = "boot",
                u = 60,#与coxmodle1中time.inc一致
                m = 431,#约等于样本数除3
                B = 1000)

coxmodle8<-cph(Surv(days,died)~ bclc,data = pbc,x = T,y = T,surv = T,time.inc = 60)
cal8<-calibrate(coxmodle8,
                cmethod = "KM",
                method = "boot",
                u =60,#与coxmodle1中time.inc一致
                m = 431,#约等于样本数除3
                B = 1000)

library("RColorBrewer")
display.brewer.all()

color<-brewer.pal(n = 4, name = "Set1")

plot(cal5,lwd=2,errbar.col = color[1],
     bty = "l", 
     lty=1,xlim=c(0,1),ylim=c(0,1),
     xlab = "nomogram predicted probability of 5 year OS",
     ylab = "actual 5 year OS",
     col = color[1],cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6)

lines(cal5[,c("mean.predicted","KM")],type="b",lwd=2,col=color[1],pch=16)



plot(cal6,lwd = 2,lty = 1,errbar.col = color[2],
     xlim = c(0,1),ylim= c(0,1),col = color[2],add = T)
lines(cal6[,c('mean.predicted',"KM")],
      type = 'b', lwd = 1, col = color[2], pch = 16)


plot(cal7,lwd = 2,lty = 1,errbar.col = color[3],
     xlim = c(0,1),ylim= c(0,1),col = color[3],add = T)
lines(cal6[,c('mean.predicted',"KM")],
      type = 'b', lwd = 1, col = color[3], pch = 16)


plot(cal8,lwd = 2,lty = 1,errbar.col = color[4],
     xlim = c(0,1),ylim= c(0,1),col = color[4],add = T)
lines(cal6[,c('mean.predicted',"KM")],
      type = 'b', lwd = 1, col = color[4], pch = 16)

abline(0,1, lwd = 2, lty = 3, col = c("#224444"))



legend("topleft", #图例的位置
       legend = c("modle","AJCC7","AJCC8","BCLC"), #图例文字
       col =color, #图例线的颜色，与文字对应
       lwd = 2,#图例中线的粗细
       cex = 1.2,#图例字体大小
       bty = "n")#不显示图例边框
library(export)
graph2tif(file="result/5years calibration")

##c-index
library(pec)
library(rms)

##C指数统计
modle1<-coxph(Surv(days,died)~ risk,data = pbc)
modle2<-cph(Surv(days,died)~ Stage,data = pbc)
modle3<-cph(Surv(days,died)~ ajcc8,data = pbc)
modle4<-cph(Surv(days,died)~ bclc,data = pbc)

anova(modle1,modle2)

anova(modle1,modle2,modle3,modle4)

cindexmodle1<-cph(Surv(days,died)~ risk,data = pbc,x = T,y = T,surv = T)
cindexmodle2<-cph(Surv(days,died)~ Stage,data = pbc,x = T,y = T,surv = T)
cindexmodle3<-cph(Surv(days,died)~ ajcc8,data = pbc,x = T,y = T,surv = T)
cindexmodle4<-cph(Surv(days,died)~ bclc,data = pbc,x = T,y = T,surv = T)

set.seed(1234)
c_index<-cindex(list("Cox X1"=cindexmodle1,
                     "Cox X2"=cindexmodle2,
                     "Cox X3"=cindexmodle3,
                     "Cox X4"=cindexmodle4),
                eval.times=seq(12,60,12),#计算不同时间点的Cindex
                cens.modle="cox",
                confInt = T,
                confLevel = 0.95,
                splitMethod = "bootcv",
                B=1000,
                keep.pvalues=T)
c_index
plot(c_index,
     xlim = c(36,60),
     legend.x=1,
     legend.y=1,
     legend.cex=0.8,col=color)
legend("topleft", #图例的位置
       legend = c("modle","AJCC7","AJCC8","BCLC"), #图例文字
       col =color, #图例线的颜色，与文字对应
       lwd = 2,#图例中线的粗细
       cex = 1.2,#图例字体大小
       bty = "n")#不显示图例边框
graph2tif(file="result/cindex")

dev.off()
rm(list = ls())
