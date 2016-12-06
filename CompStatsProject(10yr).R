#Comp Stats (18 variables)
chrisx10 = read.csv("C:/Users/Toyya/Desktop/SP2016/7401/Project/10yeardata.csv")
attach(chrisx10)

#Looking at chart decided to choose 1st 19 entered variables
chrisvars10=match(c("DEP_INC_AVG","RPY_3YR_RT_SUPP","HIGHDEG","APPL_SCH_PCT_GE4","WDRAW_DEBT_MDN","PCIP12",
                  "INEXPFTE","PCIP14","UGDS_ASIAN","PCTPELL","CUML_DEBT_P25","IND_INC_AVG","GRAD_DEBT_N",
                  "PCIP52","PCIP45","PCIP50","PCIP47","CIP11BACHL"), names(chrisx10))

reduceddata10 = as.data.frame(cbind(md_earn_wne_p10,chrisx10[c(chrisvars10)]))

chrisreg10 = lm(md_earn_wne_p10~., data = reduceddata10) 
summary(chrisreg10) #R2A:=77.87

#Checking for Normality and outliers 
library(faraway)
res = rstandard(chrisreg10) 
qqnorm(chrisreg10$residuals) #Data looks like Cauchy
qqline(chrisreg10$residuals)

halfnorm(res) #Half norm plots, shows outliers (1039,147)
chrisdata_outlier10= reduceddata10[-c(1471,1039),]
#chrisdata_outlier= as.data.frame(chrisdata_outlier)

chrisreg_outlier10 = lm(md_earn_wne_p10~., data = chrisdata_outlier10) 
summary(chrisreg_outlier10) #R2 = 79.3

#Interaction Terms
CS_interact10 = lm(md_earn_wne_p10~.^2,data=chrisdata_outlier10)#R2A:81.9
CS_interact.step10=step(CS_interact10) #R2A:83.9
summary(CS_interact.step10)

#chrisreg<-lm(md_earn_wne_p6~DEP_INC_AVG+RPY_3YR_RT_SUPP+WDRAW_DEBT_MDN+INEXPFTE+PCIP14+PCIP12
#+IND_INC_AVG+UGDS_ASIAN+CUML_DEBT_P25+PCTPELL+PCIP50+TUITFTE+PCIP47+GRAD_DEBT_N+PCIP49+
#NOTFIRSTGEN_DEBT_N+IND_INC_N+GRAD_DEBT_N+PCIP39, data =chrisx)

chrisdata = cbind(chrisx$md_earn_wne_p6,chrisx[,chrisvars])
colnames(chrisdata[1]) = "md_earn_wne_p6"
View(round(cor(chrisdata[,-1]),2))


---------------------------------------------------------------------------------------------------------------------
  #Logistic for Data 
  
  attach(toyyalog10)
toyya = toyyalog10[,-1]
detach(toyyalog10)

classmod = glm(V20~.,family=binomial,data=toyya)
summary(classmod)
predprob<-predict(classmod,type='response')
class10<-vector()
for(i in 1:3046){
  if(predprob[i]>=0.5){
    class10<-append(class10,1)
  } else{
    class10<-append(class10,0)
  }
}

table10 = cbind(toyya$V20, class10)

count=0
for(i in 1:3046){
  temp = abs(table10[i,1]-table10[i,2])
  count = count + temp
}


#count6 =332
#count10 = 346
---------------------------------------------------------------------------------------------------------------------
  #KNN for Classification
  toyya10data = toyyalog10[,-1]
knn_size10 = floor(0.75 * nrow(toyya10data))
set.seed(123)# set the seed to make your partition reproductible
train_knni10 <- sample(seq_len((nrow(toyya10data))), size = knn_size10)

knntrain10 <- toyya10data[train_knni10,]
knntest10 <- toyya10data[-train_knni10,]

install.packages("class")
library(class)

cl10 = factor(knntrain10[,"V20"])
result10= knn1(knntrain10[,-19],knntest10[,-19],cl10)

tableknn10 = cbind(knntest10[,"V20"], result10)
fixtable10= sweep(tableknn10,2,c(0,1))

countknn10=0
for(i in 1:nrow(fixtable10)){
  temp = abs(fixtable10[i,1]-fixtable10[i,2])
  countknn10 = countknn10 + temp
}

#138 errors =>81.9% CR

--------------------------------------------------------------------------------------------------------------------


