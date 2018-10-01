library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)
getwd()
data<-read.csv("Telco-Customer-Churn.csv",header = TRUE)
data1=data
head(data1)
data[1] <- NULL
data[[1]] <- NULL
data <- data[,-1]
data <- data[-1]
str(data1)
summary(data1)
dim(data1)
data1$SeniorCitizen<-as.factor(data1$SeniorCitizen)
str(data1)
data.frame(colSums(is.na(data1)))
data1[is.na(data1$TotalCharges),19]=mean(data1$TotalCharges,na.rm=T)
data.frame(colSums(is.na(data1)))
num <- data1[,-c(1:4,6:17)]
cat <- data1[,c(1:4,6:17,20)]
head(cat)
head(num)
str(num)
str(cat)
set.seed(144)

spl = sample.split(data1$Churn, 0.7)
data.train = subset(data1, spl == TRUE)
str(data.train)
dim(data.train)


data.test = subset(data1, spl == FALSE)
str(data.test)
dim(data.test)
#model <- glm(Churn~., data=data.train, family=binomial())
model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService + MultipleLines
             + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV 
             + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges 
             + TotalCharges, data=data.train, family=binomial())
summary(model)
model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService 
             + I(MultipleLines=="Yes")+ InternetService + I(OnlineSecurity=="Yes")+ I(OnlineBackup=="Yes") 
             + I(DeviceProtection=="Yes") + I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)
## Remove Partner, PhoneService, Dependents, gender 
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService + I(OnlineSecurity=="Yes")+ I(OnlineBackup=="Yes") 
             + I(DeviceProtection=="Yes") + I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)
## Remove  I(OnlineBackup=="Yes"), I(DeviceProtection=="Yes") , I(OnlineSecurity=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService   +  I(TechSupport=="Yes") + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data.train, family=binomial())
summary(model)
## Remove   I(TechSupport=="Yes")
model <- glm(Churn~ 	SeniorCitizen  + tenure 
             + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + I(PaymentMethod=="Credit card (automatic)")
             + I(PaymentMethod=="Electronic check")+MonthlyCharges +	TotalCharges , data=data.train, family=binomial())
summary(model)
####   FINAL MODEL ####
## Remove  I(PaymentMethod == "Credit card (automatic)")
model1 <- glm(Churn~ 	SeniorCitizen  
              + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
              + I(PaymentMethod=="Electronic check")+ TotalCharges+MonthlyCharges+tenure , data=data.train, family=binomial())
summary(model1)
vif(model1)
# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)



## Remove   MonthlyCharges, tenure
model <- glm(Churn~ 	SeniorCitizen  
             + I(MultipleLines=="Yes")+ InternetService  +tenure  + I(StreamingTV=="Yes") 
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
             + I(PaymentMethod=="Electronic check"), data=data.train, family=binomial())
summary(model)


vif(model)
#------------------------------Checking the overall fitness of the model----------------------------#


# Coefficients (Odds)
model$coefficients
# Coefficients (Odds Ratio)
exp(model$coefficients)

prediction <- predict(model,newdata = data.train,type="response")
prediction

write.csv(prediction,"pred.csv")
data.train$Churn <- as.factor(data.train$Churn)
rocCurve   <- roc(response = data.train$Churn, predictor = prediction, 
                  levels = rev(levels(data.train$Churn)))

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)


#### TESTING ####
# Logistic Regression on full data


modelt <- glm(Churn~ 	SeniorCitizen  
              + I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
              + I(PaymentMethod=="Electronic check")+	TotalCharges , data=data.test, family=binomial())
summary(modelt)

modelt <- glm(Churn~ I(MultipleLines=="Yes")+ InternetService    + I(StreamingTV=="Yes") 
              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
              + I(PaymentMethod=="Electronic check")+	TotalCharges , data=data.test, family=binomial())
summary(modelt)



vif(modelt)


library(car)
library(mlogit)


# Coefficients (Odds)
modelt$coefficients
# Coefficients (Odds Ratio)
exp(modelt$coefficients)
prediction <- predict(modelt,newdata = data.test,type="response")
prediction
data.test$Churn <- as.factor(data.test$Churn)
rocCurve   <- roc(response = data.test$Churn, predictor = prediction, 
                  levels = rev(levels(data.test$Churn)))
predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric
AccuracyRate
Confusion 
plot(rocCurve)
