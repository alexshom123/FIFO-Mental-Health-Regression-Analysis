#NOTE: I SEPARATED MY DATA TO DO THE CV AS IT HELPS ME BE MORE ORGANISED :)

#install.packages("caret")
library("caret")
#install.packages("pROC")
library("pROC")
#install.packages("MuMIn");
library(MuMIn)
#install.packages("dplyr")
library("dplyr")

#read data into r 
dat <- read.csv('FIFO Mental Health.csv')

#select section 1, section 2 and k10 subsets and make one data frame
datdemo <- dat[,2:9]
datfamily <- dat[,32:36]
datk10 <- dat[,37:38]
dat1 <- data.frame(datdemo, datfamily, datk10)

#select sample of 1000
set.seed(10499372)
dat2 <- dat1[sample(1:nrow(dat1),size=1000,replace=FALSE),]

#remove missing responses and label response for LOOCV
datWorking <- na.omit(dat2)
datWorking$K10.2Lv <- factor(datWorking$K10.2Lv,labels=c('1 (Not stressed)','2 (Stressed)'))

#Prepare data for CV
datcv <- datWorking[,-1]
datcv <- datcv[,-13]

#LOOCV FOR DEMOGRAPHIC VARIABLES
pred.vec1 <- c(); resid.vec1 <- c()

for (I in 1:680)
{
  train.dat <- datcv[-I,];train.dat #Training set
  test.dat <- datcv[I,];test.dat #Testing set
  
  #model 4
  mod <- glm(K10.2Lv ~  Depedants + Occupation + Roster,train.dat,family="binomial")#Construct model based on training set
  
  pred <- predict(mod,new=test.dat); #Predict y at the testing set
  resid <- pred-test.dat[,1]; #residual, i.e. difference between predicted and actual
  

  pred.vec1 <- c(pred.vec1,pred) #Combine the predictions from previous iterations
  resid.vec1 <- c(resid.vec1,resid) #Combine residuals from previous iterations
  
}

#CV summary (Bias, SD, PRESS and root mean square error)
Bias1 <- mean(resid.vec1); 
SD1 <- sd(resid.vec1); 
PRESS <- sum(resid.vec1^2);
RMSE1 <- sum(resid.vec1^2)/length(resid.vec1)
summ1 <- data.frame(Bias1,SD1,RMSE1,PRESS); summ1

#LOOCV FOR MODEL WITH DEMOGRAPHIC + STRESS VARIABLES
pred.vec1 <- c(); resid.vec1 <- c()

for (I in 1:680)
{
  train.dat <- datcv[-I,];train.dat #Training set
  test.dat <- datcv[I,];test.dat #Testing set
  
  #model 4
  mod <- glm(K10.2Lv ~  Occupation + Roster + Events + Partner + Finance,train.dat,family="binomial")#Construct model based on training set
  
  pred <- predict(mod,new=test.dat); #Predict y at the testing set
  resid <- pred-test.dat[,1]; #residual, i.e. difference between predicted and actual
  
  pred.vec2 <- c(pred.vec2,pred) #Combine the predictions from previous iterations
  resid.vec2 <- c(resid.vec2,resid) #Combine residuals from previous iterations
  
}

#CV summary (Bias, SD, PRESS and root mean square error)
Bias12 <- mean(resid.vec2); 
SD12 <- sd(resid.vec2);
PRESS2 <- sum(resid.vec2^2);
RMSE12 <- sum(resid.vec2^2)/length(resid.vec2)
summ12 <- data.frame(Bias12,SD12,RMSE12,PRESS2); summ12





