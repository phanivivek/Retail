getwd()
setwd("D:\\Edvancer\\R Projects\\Project 2-Retail")
data_train <- read.csv("store_train.csv",stringsAsFactors = FALSE)
data_test<- read.csv("store_test.csv",stringsAsFactors = FALSE)
data_test$store<-NA
data_train$data='train'
data_test$data='test'
data_all=rbind(data_train,data_test)
data_all$storecode=substr(data_all$storecode,1,5)
apply(data_all, 2,function(x) sum(is.na(x)))
library(dplyr)
data_all<-filter(data_all,country !="NA" & population !="NA")
data_all$Id=NULL
data_all$salesall<- data_all$sales0+data_all$sales1+data_all$sales2+data_all$sales3+data_all$sales4
data_all$sales0=NULL
data_all$sales1=NULL
data_all$sales2=NULL
data_all$sales3=NULL
data_all$sales4=NULL
data_all$country=as.character(data_all$country)
data_all$store=as.factor(data_all$store)
data_all$storecode=as.factor(data_all$storecode)
data_all$store_Type=as.factor(data_all$store_Type)
str(data_all)
View(data_all)
glimpse(data_all)
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
cat_cal= c("countyname","Areaname","countytownname","state_alpha","country")

for(col in cat_cal){
  data_all=CreateDummies(data_all,col,50)
  # we are using frequency cutoff as 50, there is no magic number here,
  # lower cutoffs will simply result in more number of dummy vars
}
glimpse(data_all)
library(dplyr)
data_train=data_all%>%filter(data=='train')%>%select(-data)
data_test=data_all%>%filter(data=='test')%>%select(-data,-store)
set.seed(123)
library(dplyr)
s=sample(1:nrow(data_train),nrow(data_train)*0.8)
t1=data_train[s,]
t2=data_train[-s,]
t1=t1[-14]
t2=t2[-14]
library(randomForest)
library(cvTools)
param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10))
subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}

mycost_auc=function(y,what){
  roccurve=pROC::roc(y,what)
  score=pROC::auc(roccurve)
  return(score)
}
num_trails=50
my_params=subset_paras(param,num_trails)
myauc=0
for (i in 1:num_trails){
  params=my_params[i,]
  k=cvTuning(randomForest,store~.,
             data=t1,
             tuning = params,
             folds = cvFolds(nrow(t1),K=10,type = "random"),
             cost = mycost_auc,seed=2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  if(score.this>myauc){
    #print(params)
    #uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    #uncomment the line above to keep track of progress
    best_params=params
  }
  #print('DONE")
  #uncomment the line above to keep track of progress
}
myauc
best_params
ci.rf.final=randomForest(store~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=t1
)
test.score=predict(ci.rf.final,newdata=t2,type="prob")[,2]
test.score
fit=randomForest(store~.,data=t1)
fit
library(car)
prob_pred = predict(fit,newdata = t2,"prob")[,2] 
prob_pred
library(pROC)
auc_score=auc(roc(t2$store,test.score))
auc_score
# ROC-AUC Curve
#install.packages("ROCR")
library(ROCR)
ROCRPred <- prediction(predictions = test.score,t2$store)
ROCRPerf <- performance(ROCRPred, measure ="tpr", x.measure ="fpr")
plot(ROCRPerf)
plot(ROCRPerf, colorize = TRUE)
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
abline(a=0, b=1)
auc <- performance(ROCRPred, measure = "auc")
auc <- auc@y.values[[1]]
auc
auc <- round(auc, 4)
legend (.5,.4,auc, title = "AUC", cex =1)
test.score1=predict(ci.rf.final,newdata=data_test)
test.score1
write.csv(test.score1,"PhaniVivek_U_project2_part2.csv")
