setwd("C:\\Users\\Aditya\\OneDrive - IIT Kanpur\\Teaching\\2023-24\\2023-24-II\\CE687\\Assignments\\Assignment_3\\Assignment_3_ped_data_variables_code")

library(tidyverse)
#install.packages("corrplot")
library(corrplot)
#Import data
pedvars <- read.csv("ped_exposure_data_sample.csv")

colnames(pedvars)

# get training and test data (80-20 split)
n<-nrow(pedvars)
set.seed(12345) ##changing the seed will change the training/test data sets
index <- sample(n, round(n*0.8))
train <- pedvars[index,] #80% of data
test <- pedvars[-index,] #20% of data

##########Initial models#########
#Modeling AnnualEst#########
model_1 <- lm(AnnualEst~PopT,
              data=train)
summary(model_1)


#Modeling AnnualEst#########
model_2 <- lm(AnnualEst~HseHldT,
              data=train)
summary(model_2)

model_3 <- lm(AnnualEst~PopT+HseHldT,
              data=train)
summary(model_3)




###Modeling logAnnualEst#########
model_2 <- lm(logAnnualEst~PopT,
          data=train)
summary(model_2)




####model 1 goodness-of-Fit
logLik(model_1)
AIC(model_1)
BIC(model_1)

#model 1: Prediction on test data
AnnualEst_pred_model_1<-predict(model_1,test)

#model 1: training RMSE
model_1_rmse_train<-(sum(model_1$residuals^2)/nrow(train))^0.5


#model 1: test RMSE
model_1_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_1)^2)/nrow(test))^0.5



####model 2 Goodness-of-Fit
logLik(model_2)
AIC(model_2)
BIC(model_2)

#model 2: Prediction on train and test data and converting back from log
AnnualEst_pred_model_2_train<-exp(predict(model_2,train))
AnnualEst_pred_model_2_test<-exp(predict(model_2,test))



#model 2: training RMSE of not log, but back-transformed estimates
model_2_rmse_train<-(sum((train$AnnualEst-AnnualEst_pred_model_2_train)^2)/nrow(train))^0.5

#model 2: test RMSE
model_2_rmse_test<-(sum((test$AnnualEst-AnnualEst_pred_model_2_test)^2)/nrow(test))^0.5

###corrplot
colnames(train)
vars<-train[,6:21]
corrmat<-cor(vars)
corrplot(corrmat) # default is circle


##group_by summarize
volume_by_signal<-train%>%group_by(Signal) %>%
  summarize(n=n(),mean_AADT=mean(AnnualEst),sd_AADT=sd(AnnualEst))%>%
  as.data.frame()
volume_by_signal
