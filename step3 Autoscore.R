##参考：https://cran.r-project.org/web/packages/AutoScore/vignettes/Guide_book.html#prepare-training-and-test-datasets

rm(list = ls())
options(stringsAsFactors = FALSE)
getwd()
load('./script/seer_autoscor.Rdata')
library(AutoScore)
colnames(seer_autoscor)

library(dplyr)
seer_autoscor1<-select(seer_autoscor,-c(11:15))


#AutoScore preprocessing (Users to check the following)
sample_data<-seer_autoscor1
check_data(sample_data)

#Compute descriptive table (usually Table 1 in medical literature) for the dataset
compute_descriptive_table(sample_data)


#Perform univariable analysis and generate the result table with odd ratios
uni_table<-compute_uni_variable_table(sample_data)
print(uni_table)

##Perform multivariable analysis and generate the result table with adjusted odd ratios.
multi_table<-compute_multi_variable_table(sample_data)
print(multi_table)

##AutoScore
#Prepare training, validation, and test datasets
set.seed(1234)
out_split <- split_data(data = sample_data, ratio = c(0.7, 0, 0.3), cross_validation = TRUE)
train_set <- out_split$train_set
validation_set <- out_split$validation_set
test_set <- out_split$test_set

#Generate variable ranking list (AutoScore Module 1)
ranking <- AutoScore_rank(train_set, ntree = 100)
rank<-as.data.frame(ranking)
rank$name<-rownames(rank)
rank<-select(rank,"name",everything())
library(forcats)
library(ggplot2)
library(ggsci)
library(dplyr)
data<-rank
colnames(data)
data %>%
  mutate(name = fct_reorder(name, ranking)) %>%
  ggplot( aes(x=name, y=ranking)) + ggtitle("Relative importance of variables")+
  geom_bar(stat="identity", fill="#f68060", alpha=1.6, width=.6) +
  coord_flip() +
  xlab("") +
  theme_bw(base_size = 18)+scale_color_jama()
library(export)
graph2tif(file="result/Relative importance of variables")

#Select the best model with parsimony plot (AutoScore Modules 2+3+4)
AUC <- AutoScore_parsimony(
  train_set,
  validation_set,
  rank = ranking,
  max_score = 100,
  n_min = 1,
  n_max = 20,
  cross_validation = TRUE,
  categorize = "quantile",
  fold = 10,
  quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),do_trace = F
  
)


write.csv(data.frame(AUC), file = "./result/AUC.csv")
graph2tif(file="result/number of variables")




# Example 1: Top 7 variables are selected
num_var <- 7
final_variables <- names(ranking[1:num_var])
final_variables

# {Example 2: Top 9 variables are selected
#num_var <- 9
#final_variables <- names(ranking[1:num_var])

# {Example 3: Top 6 variables, the 9th and 10th variable are selected
#num_var <- 4
#final_variables <- names(ranking[c(1:num_var, 6)])
#final_variables

##Generate initial scores with the final list of variables(Re-run AutoScore Modules 2+3)
cut_vec <- AutoScore_weighting( 
  train_set,
  validation_set,
  final_variables,
  max_score = 100,
  categorize = "quantile",
  quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
)

##Fine-tune the initial score generated in STEP(iii) (AutoScore Module 5 & Re-run AutoScore Modules 2+3) 微调参数
## Current cutoffs:`c(35, 49, 76, 89)`. We can fine tune the cutoffs as follows:
# Example 1: rounding up to a nice number
cut_vec$tumor_size<- c(13, 20, 60, 130)

cut_vec$Age <- c(45, 55, 65, 75)


scoring_table <- AutoScore_fine_tuning(train_set,
                                       validation_set,
                                       final_variables,
                                       cut_vec,
                                       max_score = 100)

graph2tif(file="result/ROC TRAINSET")


##Evaluate final risk scores on test dataset (AutoScore Module 6)
pred_score <-
  AutoScore_testing(
    test_set,
    final_variables,
    cut_vec,
    scoring_table,
    threshold = 45,
    with_label = TRUE
  )
head(pred_score)
graph2tif(file="result/ROC testset")

write.csv(pred_score, file = "script/pred_score.csv")

pred_score1 <-
  AutoScore_testing(
    train_set,
    final_variables,
    cut_vec,
    scoring_table,
    threshold = "best",
    with_label = TRUE
  )
head(pred_score1)
write.csv(pred_score1, file = "script/pred_score1.csv")

print_roc_performance(pred_score$Label, pred_score$pred_score, threshold = 45)


testset_for_survival<-cbind(pred_score,test_set)
testset_for_survival<-select(testset_for_survival,c(1,2,final_variables))
survivalmonth<-select(seer_autoscor,15)
survivalmonth$pathid<-rownames(survivalmonth)
testset_for_survival$pathid<-rownames(testset_for_survival)
testset_for_survival<-merge(x = testset_for_survival,y = survivalmonth,
                            by = "pathid",all.x = T)


train_for_survival<-cbind(pred_score1,train_set)
train_for_survival<-select(train_for_survival,c(1,2,final_variables))
train_for_survival$pathid<-rownames(train_for_survival)
train_for_survival<-merge(x =train_for_survival,y = survivalmonth,
                            by = "pathid",all.x = T)


###survival
##KM  trainset
library(survival)
library(survminer)
train_for_survival$risk<-ifelse(train_for_survival$pred_score>=45,"High","Low")

kmfit2 <- survfit(Surv(Survival_months, Label=="Dead")~risk, data=train_for_survival)
ggsurvplot(kmfit2, conf.int=F, pval=TRUE,
           legend.labs=c("High","Lower"), legend.title="RISK",  
           title=c("Kaplan-Meier Curve for trainset by risk stratify of trainset"))
library(export)
graph2tif(file="result/kmplot of trainset")
summary(kmfit2)

###logrank

mySurv=with(train_for_survival,Surv(Survival_months, Label=="Dead"))

data.survdiff=survdiff(mySurv~risk,data=train_for_survival)
data.survdiff
pValue=1-pchisq(data.survdiff$chisq,df=length(data.survdiff$n)-1)
sfit1=survfit(Surv(Survival_months, Label=="Dead")~risk, data=train_for_survival)
ggsurvplot(sfit1,pval = T,pval.method = 1,legend.labs=c("High","Lower"), legend.title="RISK",
           title=c("Logrank Curve for trainset by risk stratify of trainset"))
graph2tif(file="result/logrank plot of trainset")

summary(survfit(Surv(Survival_months, Label=="Dead")~risk, data=train_for_survival))
summary(survfit(Surv(Survival_months, Label=="Dead")~risk, data=train_for_survival),times = 3*12)
summary(survfit(Surv(Survival_months, Label=="Dead")~risk, data=train_for_survival),times = 5*12)

##KM  testset
library(survival)
library(survminer)
testset_for_survival$risk<-ifelse(testset_for_survival$pred_score>=45,"High","Low")

kmfit <- survfit(Surv(Survival_months, Label=="Dead")~risk, data=testset_for_survival)
ggsurvplot(kmfit, conf.int=F, pval=TRUE,
           legend.labs=c("High","Lower"), legend.title="RISK",  
           title=c("Kaplan-Meier Curve for testset by risk stratify of testsetset"))
library(export)
graph2tif(file="result/kmplot of testsetset")

###logrank

mySurv1=with(testset_for_survival,Surv(Survival_months, Label=="Dead"))

data.survdiff1=survdiff(mySurv1~risk,data=testset_for_survival)
data.survdiff1
pValue=1-pchisq(data.survdiff1$chisq,df=length(data.survdiff1$n)-1)
sfit2=survfit(Surv(Survival_months, Label=="Dead")~risk, data=testset_for_survival)
ggsurvplot(sfit2,pval = T,pval.method = 1,legend.labs=c("High","Lower"), legend.title="RISK",
           title=c("Logrank Curve for testset by risk stratify of testsetset"))
graph2tif(file="result/logrank plot of testsetset")

summary(survfit(Surv(Survival_months, Label=="Dead")~risk, data=testset_for_survival),times = 3*12)
summary(survfit(Surv(Survival_months, Label=="Dead")~risk, data=testset_for_survival),times = 5*12)

##整体数据情况
dataforall<-rbind(train_for_survival,testset_for_survival)

mySurv2=with(dataforall,Surv(Survival_months, Label=="Dead"))

data.survdiff2=survdiff(mySurv2~risk,data=dataforall)
data.survdiff2
pValue=1-pchisq(data.survdiff2$chisq,df=length(data.survdiff2$n)-1)
sfit3=survfit(Surv(Survival_months, Label=="Dead")~risk, data=dataforall)
ggsurvplot(sfit3,pval = T,pval.method = 1,legend.labs=c("High","Lower"), legend.title="RISK",
           title=c("Logrank Curve for testset by risk stratify of dataset"))
graph2tif(file="result/logrank plot of dataset")

summary(survfit(Surv(Survival_months, Label=="Dead")~risk, data=dataforall),times = 3*12)
summary(survfit(Surv(Survival_months, Label=="Dead")~risk, data=dataforall),times = 5*12)

save(testset_for_survival,train_for_survival,file = "./result/compar.Rdata")
rm(list = ls())
