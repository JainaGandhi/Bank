#Thera Bank Personal Loan

setwd("E:/GitHub_Repositories/DataMining_TheraBank")
library(readxl)
mydata = read_xlsx("Thera Bank_Personal_Loan.xlsx",sheet = 2)


summary(mydata)

#replace negative values with 0
mydata$`Experience (in years)` =ifelse(mydata$`Experience (in years)`<0,0,mydata$`Experience (in years)`)
View(mydata)
summary(mydata)

# As missing values are less than 3% of overall data, so removed 18 rows which includes missing values

mydata = na.omit(mydata)
summary(mydata)

library(data.table)
setnames(mydata, old=c("Age (in years)","Experience (in years)","Income (in K/month)","ZIP Code","Family members","Personal Loan","Securities Account","CD Account"), new=c("Age","Experience","Income","ZipCode","FamilyMembers","PersonalLoan","SecuritiesAccount","CDAccount"))

str(mydata)

library(corrplot)
CorMatrix1 = cor(mydata[,2:14]) 
CorMatrix1

corrplot(CorMatrix1,type = "upper",method = "number")

#  simple linear regression for the dependent variable with every independent variable 

Model1 = lm(PersonalLoan~.,data = mydata)
summary(Model1)

#  PCA/FA 

library(nFactors)
CorMatrix = cor(mydata[,2:14])  
CorMatrix1

#library(corrplot)
corrplot(CorMatrix,type = "upper",method = "number")  

library(psych)
cortest.bartlett(CorMatrix)
options(scipen=999)  # to convert scientific value to real number
options(scipen = 0)  # to convert real number into scientific value


KMO(CorMatrix)  

A = eigen(CorMatrix)
EV = A$values
EV

#kaiser's rule
plot(EV,ylab = "Eigen Value",col = "BLUE")
lines(EV,col = "Red")

#PCA
principal(CorMatrix,nfactors = 6,rotate = "none")
principal(CorMatrix,nfactors = 6,rotate = "varimax")

#FA
fa1 = fa(r=mydata[,2:14],nfactors = 6,rotate = "none",fm = "pa")
fa1
fa1$scores
fa.diagram(fa1)

#Not able to distinguish so performing orthogonal rotation
fa2 = fa(r=mydata[,2:12],nfactors = 4,rotate = "varimax",fm = "pa")
fa2
fa2$scores
fa.diagram(fa2)

#Name the factors
#PA1 -->  Age & Experience 
#          name = Personal Details

#PA2 -->  Income,PersonalLoan, CCAvg            
#          name = Finances

#PA3 -->  Education                  
#          name = knowledge

#PA4 -->  Securities Account,CDAccount     
#          name = safekeeping

mydata$FamilyMembers = as.factor(mydata$FamilyMembers)
mydata$Education = as.factor(mydata$Education)
mydata$PersonalLoan = as.factor(mydata$PersonalLoan)
mydata$SecuritiesAccount = as.factor(mydata$SecuritiesAccount)
mydata$CDAccount = as.factor(mydata$CDAccount)
mydata$Online = as.factor(mydata$Online)
mydata$CreditCard = as.factor(mydata$CreditCard)

attach(mydata)
str(mydata)

hist(Experience,main = "Maximum Experience",xlab = "Experience in years",col = "Light Blue",breaks = 20,freq = FALSE)

hist(Income,main = "Maximum Income",xlab = "Maximum income in thousands",col = "Light green",breaks = 40)

hist(Income,main = "Average spending on credit card",xlab = "Average spending on credit card in thousands",col = "light yellow",breaks = 20)

boxplot(mydata,main = "Multiple boxplot for comparison")
boxplot(Income~Experience,data = mydata)
boxplot(Experience~Age,data = mydata)

#______________________BUILDING CART MODEL______________________________________

library(caTools)
set.seed(123)
split=sample.split(mydata,SplitRatio=0.7)
carttraindata=subset(mydata,split==TRUE)
carttestdata=subset(mydata,split==FALSE)

#checking distribution of partition data
round(prop.table(table(carttraindata$FamilyMembers)),2)
round(prop.table(table(carttraindata$Education)),2)
round(prop.table(table(carttraindata$PersonalLoan)),2)
round(prop.table(table(carttraindata$SecuritiesAccount)),2)
round(prop.table(table(carttraindata$CDAccount)),2)
round(prop.table(table(carttraindata$Online)),2)
round(prop.table(table(carttraindata$CreditCard)),2)

round(prop.table(table(carttestdata$FamilyMembers)),2)
round(prop.table(table(carttestdata$Education)),2)
round(prop.table(table(carttestdata$PersonalLoan)),2)
round(prop.table(table(carttestdata$SecuritiesAccount)),2)
round(prop.table(table(carttestdata$CDAccount)),2)
round(prop.table(table(carttestdata$Online)),2)
round(prop.table(table(carttestdata$CreditCard)),2)


nrow(carttraindata) #will display no. or records in train set
nrow(carttestdata)

#Q2 - Applying CART <plot the tree>

library(rpart)
library(rpart.plot)
set.seed(123)

tree=rpart(formula = PersonalLoan~.,data = carttraindata,method = "class",
           control = rpart.control(minsplit=100,minbucket = 10,cp=0))
#cp (complexity parameter)
tree
table(carttraindata$PersonalLoan)

printcp(tree)
plotcp(tree)
rpart.plot(tree)

library(rattle)
fancyRpartPlot(tree) # rattle library

#Prune the tree
tree$cptable
tree$cptable[,"xerror"]
min(tree$cptable[,"xerror"])

bestcp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
bestcp

#prune the tree based on best cp
ptree=prune(tree,cp=bestcp)
print(ptree)

rpart.plot(ptree,cex=0.5)

#variable importance
library(caret)
ptree$variable.importance
df_cart=data.frame(ptree$variable.importance)
df_cart

#predict for training sets
carttraindata$predict.class=predict(ptree,carttraindata,type = "class")
carttraindata$predict.score=predict(ptree,carttraindata)

#predictions for test set
carttestdata$predict.class=predict(ptree,carttestdata,type = "class")
carttestdata$predict.score=predict(ptree,carttestdata)


#creating the confusion matrix
tabcart=with(carttraindata,table(PersonalLoan,predict.class))
tabcart

#Model performance measures
CART_train_accuracy=round((tabcart[1,1]+tabcart[2,2])/(tabcart[1,1]+tabcart[2,2]+tabcart[2,1]+tabcart[1,2]),2)
CART_train_accuracy

CART_train_sensitivity=tabcart[2,2]/(tabcart[2,1]+tabcart[2,2]) #TP/(TP+FN)
CART_train_sensitivity

CART_train_specificity=tabcart[1,1]/(tabcart[1,1]+tabcart[1,2]) #TN/ (TN+FP)
CART_train_specificity

#Plotting the ROCR curve
library(ROCR)
pred=prediction(carttraindata$predict.score[,2],carttraindata$PersonalLoan)
perf=performance(pred,"tpr","fpr")
plot(perf,col="blue",main = "ROC plot - training set")
abline(0,1,lty=8,col="red")

cart.train.ks=max(attr(perf,"y.values")[[1]]-attr(perf,"x.values")[[1]])
cart.train.ks

cart.train.auc=performance(pred,"auc")
cart.train.auc=as.numeric(cart.train.auc@y.values)
cart.train.auc

#for testing dataset
tabcarttest=with(carttestdata,table(PersonalLoan,predict.class))
tabcarttest

#model perf measures
CART_test_accuracy=round((tabcarttest[1,1]+tabcarttest[2,2])/(tabcarttest[1,1]+tabcarttest[2,2]+tabcarttest[2,1]+tabcarttest[1,2]),2)
CART_test_accuracy

CART_test_sensitivity=tabcarttest[2,2]/(tabcarttest[2,1]+tabcarttest[2,2])
CART_test_sensitivity

CART_test_specificity=tabcarttest[1,1]/(tabcarttest[1,1]+tabcarttest[1,2])
CART_test_specificity

#Plotting the ROCR curve
library(ROCR)
pred=prediction(carttestdata$predict.score[,2],carttestdata$PersonalLoan)
perf=performance(pred,"tpr","fpr")
plot(perf,col="blue",main = "ROC plot - testing set")
abline(0,1,lty=8,col="red")

cart.test.ks=max(attr(perf,"y.values")[[1]]-attr(perf,"x.values")[[1]])
cart.test.ks

cart.test.auc=performance(pred,"auc")
cart.test.auc=as.numeric(cart.test.auc@y.values)
cart.test.auc


#__________________________Building Random Forest Model_______________________

set.seed(123)
RFTrain_index = sample(1:nrow(mydata),0.70*nrow(mydata))
RFtrain = mydata[RFTrain_index,]
RFtest = mydata[-RFTrain_index,]

library(randomForest)
set.seed(123)

##Build the first RF model
Rforest = randomForest(PersonalLoan~.,data=RFtrain,ntree=800,mtry=7,nodesize=10,importance=TRUE)

print(Rforest)
plot(Rforest, main="Error Rates Random Forest RFtrain")
Rforest$importance

#Tune up the RF model to find out the best mtry
set.seed(123)
tRforest = tuneRF(x=RFtrain[,-c(1,10)],y=RFtrain$PersonalLoan,mtrystart = 10,stepfactor=1.0,ntree=200,improve=T,nodesize=10,trace=TRUE,plot=TRUE,doBest=TRUE,importance=TRUE)

##Build the model based on accurate mtry value
Rforest = randomForest(PersonalLoan~.,data=RFtrain,ntree=200,mtry=6,nodesize=10,importance=TRUE) #here we choose mtry as 6 because it's the least error
print(Rforest)
plot(Rforest)

RFtrain$RF.Pred = predict(Rforest,data=RFtrain,type="class")
RFtrain$RF.Score = predict(Rforest,data=RFtrain,type="prob")[,"1"]

RFtest$RF.Pred = predict(Rforest,RFtest,type="class")
RFtest$RF.Score = predict(Rforest,RFtest,type="prob")[,"1"]

#Confusion Matrix RF Model
tab=table(RFtrain$PersonalLoan, RFtrain$RF.Pred)
tab

RFtrainAccuracy=round((tab[1,1]+tab[2,2])/(tab[1,1]+tab[2,2]+tab[2,1]+tab[1,2]),2)
RFtrainAccuracy

RFtrainSensitivity=tab[2,2]/(tab[2,1]+tab[2,2]) #TP/(TP+FN)
RFtrainSensitivity

RFtrainSpecificity=tab[1,1]/(tab[1,1]+tab[1,2]) #TN/ (TN+FP)
RFtrainSpecificity

#Plotting the ROCR curve
library(ROCR)
pred=prediction(RFtrain$RF.Score,RFtrain$PersonalLoan)
perf=performance(pred,"tpr","fpr")
plot(perf,col="blue",main = "ROC plot - training set")
abline(0,1,lty=8,col="red")

RF.train.ks=max(attr(perf,"y.values")[[1]]-attr(perf,"x.values")[[1]])
RF.train.ks

RF.train.auc=performance(pred,"auc")
RF.train.auc=as.numeric(RF.train.auc@y.values)
RF.train.auc

# Testing Data
#Confusion Matrix
tab=table(RFtest$PersonalLoan, RFtest$RF.Pred)
tab

RFtestAccuracy=round((tab[1,1]+tab[2,2])/(tab[1,1]+tab[2,2]+tab[2,1]+tab[1,2]),2)
RFtestAccuracy

RFtestSensitivity=tab[2,2]/(tab[2,1]+tab[2,2]) #TP/(TP+FN)
RFtestSensitivity

RFtestSpecificity=tab[1,1]/(tab[1,1]+tab[1,2]) #TN/ (TN+FP)
RFtestSpecificity

#Plotting the ROCR curve
library(ROCR)
pred=prediction(RFtest$RF.Score,RFtest$PersonalLoan)
perf=performance(pred,"tpr","fpr")
plot(perf,col="blue",main = "ROC plot - testing set")
abline(0,1,lty=8,col="red")

RF.test.ks=max(attr(perf,"y.values")[[1]]-attr(perf,"x.values")[[1]])
RF.test.ks

RF.test.auc=performance(pred,"auc")
RF.test.auc=as.numeric(RF.test.auc@y.values)
RF.test.auc
