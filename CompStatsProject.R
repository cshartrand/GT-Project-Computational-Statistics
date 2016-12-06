------------------------------------------------------------------------------------------------------------------------------  
#Comp Stats
#Find lambda via cross-validation   

chrisx = read.csv("C:/Users/Toyya/Desktop/SP2016/7401/Project/6yeardata.csv")
chrisx = chrisx[,-1]
attach(chrisx)

#Looking at chart decided to choose 18 entered variables

chrisvars=match(c("DEP_INC_AVG","RPY_3YR_RT_SUPP","WDRAW_DEBT_MDN","INEXPFTE","PCIP14","PCIP12",
                  "IND_INC_AVG","UGDS_ASIAN","CUML_DEBT_P25","PCTPELL","PCIP50","TUITFTE","PCIP47","GRAD_DEBT_N",
                  "PCIP49","NOTFIRSTGEN_DEBT_N","PCIP39", "IND_INC_N"), names(chrisx))

reduceddata = as.data.frame(cbind(md_earn_wne_p6,chrisx[c(chrisvars)]))

chrisreg = lm(md_earn_wne_p6~., data = reduceddata) 
summary(chrisreg) #R2A:=74.4

#Checking for Normality and outliers 
qqnorm(chrisreg$residuals) #Data looks like Cauchy
qqline(chrisreg$residuals)

res = rstandard(chrisreg) 
halfnorm(res) #Half norm plots, shows outliers (1039,147)
chrisdata_outlier= reduceddata[-c(1471,1039),]
chrisdata_outlier= as.data.frame(chrisdata_outlier)
chrisreg_outlier = lm(md_earn_wne_p6~., data = chrisdata_outlier) 
summary(chrisreg_outlier) #R2 = 76.8


CS_interact6 = lm(md_earn_wne_p6~.^2,data=chrisdata_outlier)#R2A:81.9
CS_interact.step6=step(CS_interact6) #R2A:82.1
summary(CS_interact.step6)

#chrisreg<-lm(md_earn_wne_p6~DEP_INC_AVG+RPY_3YR_RT_SUPP+WDRAW_DEBT_MDN+INEXPFTE+PCIP14+PCIP12
#+IND_INC_AVG+UGDS_ASIAN+CUML_DEBT_P25+PCTPELL+PCIP50+TUITFTE+PCIP47+GRAD_DEBT_N+PCIP49+
#NOTFIRSTGEN_DEBT_N+IND_INC_N+GRAD_DEBT_N+PCIP39, data =chrisx)

chrisdata = cbind(chrisx$md_earn_wne_p6,chrisx[,chrisvars])
colnames(chrisdata[1]) = "md_earn_wne_p6"
View(round(cor(chrisdata[,-1]),2))

---------------------------------------------------------------------------------------------------------------------
#Logistic for Data 
  
data(toyyalog6)  
attach(toyyalog6)
toyya6 = toyyalog6[,-1]
detach(toyyalog6)

classmod6 = glm(V20~.,family=binomial,data=toyya6)
summary(classmod6)
predprob6<-predict(classmod6,type='response')
class6<-vector()
for(i in 1:3046){
  if(predprob6[i]>=0.5){
    class6<-append(class6,1)
  } else{
    class6<-append(class6,0)
  }
}

table6 = cbind(toyya6$V20, class6)

count6=0
for(i in 1:3046){
  temp = abs(table6[i,1]-table6[i,2])
  count6 = count6 + temp
}

#count6 =332
#count10 = 346
---------------------------------------------------------------------------------------------------------------------
#KNN for Classification
toyya6data = toyyalog6[,-1]
knn_size = floor(0.75 * nrow(toyya6data))
set.seed(123)# set the seed to make your partition reproductible
train_knni <- sample(seq_len((nrow(toyya6data[,-1]))), size = knn_size)

knntrain <- toyya6data[train_knni,]
knntest <- toyya6data[-train_knni,]

install.packages("class")
library(class)

cl = factor(knntrain[,"V20"])
result= knn(knntrain[,-19],knntest[,-19],cl)

tableknn6 = cbind(knntest[,"V20"], result)
fixtable6= sweep(tableknn6,2,c(0,1))

countknn6=0
for(i in 1:762){
  temp = abs(fixtable6[i,1]-fixtable6[i,2])
  countknn6 = countknn6 + temp
}

#131 errors =>82.3% CR

#Try with cross validation: same result
clcv = factor(toyya6data[,"V20"])
resultcv = knn.cv(toyya6data[,-19,],clcv)  

tableknncv6 = cbind(toyya6data[,"V20"], resultcv)
fixtablecv6= sweep(tableknncv6,2,c(0,1))  

countknncv6=0
for(i in 1:762){
  temp = abs(fixtablecv6[i,1]-fixtablecv6[i,2])
  countknncv6 = countknncv6 + temp
}

---------------------------------------------------------------------------------------------------------------------

