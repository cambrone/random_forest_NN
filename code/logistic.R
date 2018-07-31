##########################################################################
# TASK: Logistic Regression Analysis
#
# Author: Andres Cambronero
# Project: STAT503-Final Project
# Date Started: April 10, 2018
# Latest Update: July 20, 2018
##########################################################################

#clean environment
rm(list=ls())

#set working environment
setwd("~/Desktop/Michigan/503_multivariate/project/analysis")

#load data
library(nnet)  
library(pROC)

#read data
imb_train<-read.csv("imbalanced_train.csv")
imb_test<-read.csv("imbalanced_test.csv")


#remove the useless variables
imb_train<-imb_train[,-match(c("accident_index", "longitude", "latitude", "hit_object_off_carriageway"), names(imb_train))]
imb_test<-imb_test[,-match(c("accident_index", "longitude", "latitude", "hit_object_off_carriageway"), names(imb_test))]

#change all to factors
for (i in 1:ncol(imb_train)){
  imb_train[,i] <- as.factor(imb_train[,i])
}

#change the few to numeric
imb_train$age_of_casualty<-as.numeric(imb_train$age_of_casualty)
imb_train$age_of_driver<-as.numeric(imb_train$age_of_driver)
imb_train$age_of_vehicle<-as.numeric(imb_train$age_of_vehicle)

sapply(imb_train, class)


#change all to factors
for (i in 1:ncol(imb_test)){
  imb_test[,i] <- as.factor(imb_test[,i])
}


#change the few to numeric
imb_test$age_of_casualty<-as.numeric(imb_test$age_of_casualty)
imb_test$age_of_driver<-as.numeric(imb_test$age_of_driver)
imb_test$age_of_vehicle<-as.numeric(imb_test$age_of_vehicle)

sapply(imb_test, class)

#imb_train$accident_severity<-as.character(imb_train$accident_severity)
#imb_train$accident_severity[which(imb_train$accident_severity=="1")]<-"1"
#imb_train$accident_severity[which(imb_train$accident_severity=="2")]<-"1"
#imb_train$accident_severity[which(imb_train$accident_severity=="3")]<-"0"


#imb_test$accident_severity<-as.character(imb_test$accident_severity)
#imb_test$accident_severity[which(imb_test$accident_severity=="1")]<-"1"
#imb_test$accident_severity[which(imb_test$accident_severity=="2")]<-"1"
#imb_test$accident_severity[which(imb_test$accident_severity=="3")]<-"0"


#imb_train$accident_severity<-as.factor(imb_train$accident_severity)
#imb_test$accident_severity<-as.factor(imb_test$accident_severity)



######################################
# LOGISTIC WITH IMBALANCED DATA    ###
######################################
# original logit 
imb_logistic<- multinom(accident_severity ~ . , imb_train, trace=FALSE)
#summary of imb_logistic
summary(imb_logistic)


#NaN produced means there is an identifiability issue. Will wait to see if goes away in balanced data. As a fix, I dropped two columns with 
#the most levels
#remove the useless variables
#imb_train<-imb_train[,-match(c("vehicle_manoeuvre", "vehicle_type"), names(imb_train))]
#imb_test<-imb_test[,-match(c("vehicle_manoeuvre", "vehicle_type"), names(imb_test))]

# original logit try again
#imb_logistic<- multinom(accident_severity ~ . , imb_train, trace=FALSE)

#summary of imb_logistic
#summary(imb_logistic)


### RESULTS FOR IMBALANCE DATA TRAINING DATA
#confusion matrix for logistic on imbalanced training data
set.seed(1)
imbalance_pred_tr<-predict(imb_logistic, imb_train, type = "class")
table(imb_train$accident_severity, imbalance_pred_tr) #class 1 has 14 true 1s

#training overall error
set.seed(1)
mean(predict(imb_logistic, imb_train, type = "class") != imb_train$accident_severity)
#training recall for class 1 is 1
#training precision for class 1 is 1


#training recall for class 2 is 0.09913793
#training precision for class 2 is  0.6969697



#RESULTS ON TEST
#confusion matrix for logistic on test data
set.seed(1)
imbalance_pred_test<-predict(imb_logistic, imb_test, type = "class")
table(imb_test$accident_severity, imbalance_pred_test) #class 1 has 4 true 1s
set.seed(1)
mean(predict(imb_logistic, imb_test, type = "class") != imb_test$accident_severity) #slightly higher than in training
#test recall for class 1 is 0
#tes precision for class 1 is 0 

#test recall for class 2 is 0
#test precision for class 2 is 0 

########################################
# Testing ROC IMBALANCED for class 1
########################################
roc_data<-imb_test
roc_data$accident_severity<-ifelse(roc_data$accident_severity==1, 1, 0)

pred1<-predict(imb_logistic, roc_data)
roc_data$prediction<-pred1
roc1<-roc(roc_data$accident_severity,as.numeric(roc_data$prediction))
plot(roc1, col = "blue")




#########################################
# LOGISTIC WITH BALANCED DATA DATA    ###
#########################################
bal_train<-read.csv("balanced_train.csv")

#bal_train$accident_severity<-as.character(bal_train$accident_severity)
#bal_train$accident_severity[which(bal_train$accident_severity=="1")]<-"1"
#bal_train$accident_severity[which(bal_train$accident_severity=="2")]<-"1"
#bal_train$accident_severity[which(bal_train$accident_severity=="3")]<-"0"


bal_train$accident_severity<-as.factor(bal_train$accident_severity)


#remove the useless variables
bal_train<-bal_train[,-match(c("accident_index", "longitude", "latitude", "hit_object_off_carriageway"), names(bal_train))]

#change all to factors
for (i in 1:ncol(bal_train)){
  bal_train[,i] <- as.factor(bal_train[,i])
}

#change the few to numeric
bal_train$age_of_casualty<-as.numeric(bal_train$age_of_casualty)
bal_train$age_of_driver<-as.numeric(bal_train$age_of_driver)
bal_train$age_of_vehicle<-as.numeric(bal_train$age_of_vehicle)




# balanced logit 
bal_logistic<- multinom(accident_severity ~ . , bal_train, trace=FALSE)

#summary of bal_logistic THE NA WARNING THAT APPEARED IN IMBALANCED DISAPPEARED
summary(bal_logistic)



### RESULTS FOR BALANCE DATA TRAINING DATA
#confusion matrix for logistic on balanced training data
set.seed(1)
balance_pred_tr<-predict(bal_logistic, bal_train, type = "class")
table(bal_train$accident_severity, balance_pred_tr) #class 1 has 640 true 1s


#training overall error
set.seed(1)
mean(predict(bal_logistic, bal_train, type = "class") != bal_train$accident_severity)
#training recall for class 1 is 460/(460+106+74)=0.71875
#training precision for class 1 is  460/(460+171+96)=0.6327373

#training recall for class 2 is 282/(282+171+187)=0.44
#training precision for class 2 is 282/(282+106+137)=0.5371429



#RESULTS ON TEST
#confusion matrix for balanced logistic on test data
set.seed(1)
balance_pred_test<-predict(bal_logistic, imb_test, type = "class")
table(imb_test$accident_severity, balance_pred_test) #class 1 has 4 true 1s
set.seed(1)
mean(predict(bal_logistic, imb_test, type = "class") != imb_test$accident_severity) #overall training error is much higher
#test recall for class 1 is 2/4=0.5
#test precision for class 1 is 2/(2+26+59)= 0.02298851

#test recall for class 2 is 18/(26+18+14)=0.31
#test precision for class 2 is 18/(2+18+129)=0.1208054

########################################
# Testing ROC BALANCED for class 1
########################################
roc_data2<-imb_test
roc_data2$accident_severity<-ifelse(roc_data2$accident_severity==1, 1, 0)
test<-roc_data2[,-match(c("accident_severity"), names(roc_data2))]

#pred1<-predict(bal_logistic, roc_data2)
pred1<-predict(bal_logistic, test)

#roc_data2$prediction<-pred1
test$prediction<-pred1

test_<-roc(roc_data2$accident_severity,as.numeric(test$prediction))
plot(test_)








##########################################
# LOGISTIC WITH ADJUSTED WEIGHTS DATA  ###
##########################################
# weights logit 
wts<-ifelse(imb_train$accident_severity==1, 10000, 
            ifelse(imb_train$accident_severity==2, 1, 0.001))

weighted_logistic<- multinom(accident_severity ~ . , imb_train, weights = wts, trace=FALSE)

#summary of weighted_logistic THE NA WARNING THAT APPEARED IN IMBALANCED APPEARED AGAIN
summary(weighted_logistic)



### RESULTS FOR WEIGHTED LOGISTIC DATA TRAINING DATA
#confusion matrix for WEIGHTED logistic on training data
set.seed(1)
weighted_pred_tr<-predict(weighted_logistic, imb_train, type = "class")
table(imb_train$accident_severity, weighted_pred_tr) #class 1 has 640 true 1s


#training overall error
set.seed(1)
mean(predict(weighted_logistic, imb_train, type = "class") != imb_train$accident_severity)
#training recall for class 1 is 14/(14)=1
#training precision for class 1 is  14/(14+65)= 0.1772152

#training recall for class 2 is 232/(232)=1
#training precision for class 2 is 232/(1503)=0.15


### RESULTS FOR WEIGHTED LOGISTIC DATA TEST DATA
set.seed(1)
weighted_pred_test<-predict(weighted_logistic, imb_test, type = "class")
table(imb_test$accident_severity, weighted_pred_test) #class 1 has 4 true 1s
set.seed(1)
mean(predict(weighted_logistic, imb_test, type = "class") != imb_test$accident_severity)
#test recall for class 1 is 1/4=0.25
#test precition for class 1 is 1/(1+6+18)=0.04

#test recall for class 2 is 51/(51+1+6)=0.87
#test precision for class 2 is 51/(51+3+377)=0.11



########################################
# Testing ROC WEIGHTED for class 1
########################################
roc_data3<-imb_test
roc_data3$accident_severity<-ifelse(roc_data3$accident_severity==1, 1, 0)

pred1<-predict(weighted_logistic, roc_data2)
roc_data3$prediction<-pred1
roc3<-roc(roc_data3$accident_severity,as.numeric(roc_data3$prediction))
plot(roc3)



