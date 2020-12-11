#function to get mode of categorical data
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



#Below is an imported function to have many plots on the same page
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#LOAD PACKAGES*******IF SOME NOT FOUND PLEASE INSTALL (E.G GGPLOTS2 - COMMENTED OUT)

#install.packages("ggplot2")
library("ggplot2")
#install.packages("caret")
library("caret")
#install.packages("pROC")
library("pROC")
#install.packages("MuMIn");
library(MuMIn)
#install.packages("dplyr")
library("dplyr")
#install.packages("effects")
library(effects)

############################# TASK 1 ######################################################
#read data into r 
dat <- read.csv('FIFO Mental Health.csv')

#decide which other section to analyse (I will do the 5th section)
set.seed(10499372); sample(2:5,size=1)

#select section 1, section 2 and k10 subsets and make one data frame
datdemo <- dat[,2:9]
datfamily <- dat[,32:36]
datk10 <- dat[,37:38]
dat1 <- data.frame(datdemo, datfamily, datk10)

#select sample of 1000
set.seed(10499372)
dat2 <- dat1[sample(1:nrow(dat1),size=1000,replace=FALSE),]

#remove missing responses
datWorking <- na.omit(dat2)

#convert integers to factors for demographics
datWorking$Age.Grp <- factor(datWorking$Age.Grp,labels=c('1 (16-24)','2 (25-34)','3 (35-44)','4 (45-54)','5 (55+)'))
datWorking$Gender <- factor(datWorking$Gender,labels=c('1 (Male)','2 (Female)'))
datWorking$Relationship <- factor(datWorking$Relationship,labels=c('1 (Single)','2 (Defacto/Married)','3 (Divorced/Separated/Widowed'))
datWorking$Depedants <- factor(datWorking$Depedants,labels=c('1 (No children)','2 (One child)','3 (2+ children)'))
datWorking$Education <- factor(datWorking$Education,labels=c('1 (Year 10)','2 (Year 12)','3 (Cert/Diploma','4 (University Degree)'))
datWorking$Occupation <- factor(datWorking$Occupation,labels=c('1 (Managment)','2 (Contractor)'))
datWorking$Sector <- factor(datWorking$Sector,labels=c('1 (Construction)','2 (Underground)','3 (Open Pit)'))
datWorking$Roster <- factor(datWorking$Roster,labels=c('1 (1/1)','2 (2/1)','3 (4/1)', '4 (Other)'))

#convert integers to factors for family variables
datWorking$Events <- factor(datWorking$Events,labels=c('1 (Not stressed)','2 (Slightly stressed)','3 (Stressed)','4 (Very Stressed)'))
datWorking$Partner <- factor(datWorking$Partner,labels=c('1 (Not stressed)','2 (Slightly stressed)','3 (Stressed)','4 (Very Stressed)'))
datWorking$Children <- factor(datWorking$Children,labels=c('1 (Not stressed)','2 (Slightly stressed)','3 (Stressed)','4 (Very Stressed)'))
datWorking$Parents <- factor(datWorking$Parents,labels=c('1 (Not stressed)','2 (Slightly stressed)','3 (Stressed)','4 (Very Stressed)'))
datWorking$Finance <- factor(datWorking$Finance,labels=c('1 (Not stressed)','2 (Slightly stressed)','3 (Stressed)','4 (Very Stressed)'))

#convert integers to factors for k10 variables
datWorking$K10.4Lv <- factor(datWorking$K10.4Lv,labels=c('1 (Not stressed)','2 (Slightly stressed)','3 (Stressed)','4 (Very Stressed)'))
datWorking$K10.2Lv <- factor(datWorking$K10.2Lv,labels=c('1 (Not stressed)','2 (Stressed)'))

##################################### TASK 2 ##########################################################################################################################################

#Generate summary plots
p1 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Gender, fill = datWorking$Gender), show.legend = FALSE) + xlab("Gender") + ggtitle("Gender Frequency (n=680)")
p2 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Age.Grp, fill = datWorking$Age.Grp), show.legend = FALSE) + xlab("Age Group") + ggtitle("Age Group Frequency (n=680)")
p3 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Relationship, fill = datWorking$Relationship), show.legend = FALSE) + xlab("Relationship") + ggtitle("Relationship Frequency (n=680)")
p4 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Depedants, fill = datWorking$Depedants), show.legend = FALSE) + xlab("Dependants") + ggtitle("Dependants Frequency (n=680)")
p5 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Education, fill = datWorking$Education), show.legend = FALSE) + xlab("Education") + ggtitle("Education Frequency (n=680)")
p6 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Occupation, fill = datWorking$Occupation), show.legend = FALSE) + xlab("Occupation") + ggtitle("Occupation Frequency (n=680)")
p7 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Sector, fill = datWorking$Sector), show.legend = FALSE) + xlab("Sector") + ggtitle("Sector Frequency (n=680)")
p8 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Roster, fill = datWorking$Roster), show.legend = FALSE) + xlab('Roster (Weeks on/ Weeks off)') + ggtitle("Roster Frequency (n=680)")

multiplot(p1,p2,p3,p4,p5,p6,p7,p8, cols=2)


#Summarise demographic percentages and counts and frequencies

tableGender <- table(datWorking$Gender) #summary for gender
prop.table(tableGender)

tableAge.grp <- table(datWorking$Age.Grp) #summary for age 
prop.table(tableAge.grp)

tableRelationship <- table(datWorking$Relationship) #summary for relationship
prop.table(tableRelationship)

tableDepedants <- table(datWorking$Depedants) #summary for dependants
prop.table(tableDepedants)

tableEducation <- table(datWorking$Education) #summary for education
prop.table(tableEducation)

tableOccuation <- table(datWorking$Occupation) #summary for occupation
prop.table(tableOccuation)

tableSector <- table(datWorking$Sector) #summary for sector
prop.table(tableSector)

tableRoster <- table(datWorking$Roster) #summary for roster
prop.table(tableRoster)

#Preform chi squared test for k10 against each categorical variable
a <- table(datWorking$K10.4Lv, datWorking$Gender) #independence test for gender
prop.table(a)
chisq.test(datWorking$K10.4Lv, datWorking$Gender)

b <- table(datWorking$K10.4Lv, datWorking$Age.Grp) #independence test for age
prop.table(b)
chisq.test(datWorking$K10.4Lv, datWorking$Age.Grp)

c <- table(datWorking$K10.4Lv, datWorking$Relationship) #independence test for relationship
prop.table(c)
chisq.test(datWorking$K10.4Lv, datWorking$Relationship)

d <- table(datWorking$K10.4Lv, datWorking$Depedants) #independence test for dependence
prop.table(d)
chisq.test(datWorking$K10.4Lv, datWorking$Depedants)

e <- table(datWorking$K10.4Lv, datWorking$Education) #independence test for education
prop.table(e)
chisq.test(datWorking$K10.4Lv, datWorking$Education)

f <- table(datWorking$K10.4Lv, datWorking$Occupation) #independence test for occupation
prop.table(f)
chisq.test(datWorking$K10.4Lv, datWorking$Occupation)

g <- table(datWorking$K10.4Lv, datWorking$Roster) #independence test for roster
prop.table(g)
chisq.test(datWorking$K10.4Lv, datWorking$Roster)

h <- table(datWorking$K10.4Lv, datWorking$Sector) #independence test for sector
prop.table(h)
chisq.test(datWorking$K10.4Lv, datWorking$Sector)

#Fit logistics regression models (+ AICc, Deviance and pseudo R^2 for all)

#Null model
NullModel <- glm(K10.2Lv~1,family="binomial",data=datWorking)
summary(NullModel)
AICc(NullModel) 
d0 <- deviance(NullModel)


#Full model
FullModel <- glm(K10.2Lv ~ Age.Grp + Gender + Relationship + Depedants + Education + Occupation + Roster + Sector, data = datWorking, family = "binomial")
summary(FullModel)
AICc(FullModel)  
d1 <- deviance(FullModel)
FullRsquared <-1 - d1/d0

#Backwards selection model --- most parcimonious
Bmodel <- step(FullModel,direction="backward",k=2)
summary(Bmodel)
AICc(Bmodel) 
d2 <- deviance(Bmodel)
BRsquared <-1 - d2/d0

#Forward selection
Fmodel <- step(NullModel,scope=list(lower=NullModel,upper=FullModel),direction="forward",k=2)
summary(Fmodel)
AICc(Fmodel)
d3 <- deviance(Fmodel)
FRsquared <-1 - d3/d0

#Stepwise model
Smodel <- step(FullModel,direction="both",k=2) 
summary(Smodel)
AICc(Smodel)
d4 <- deviance(Smodel)
SRsquared <-1 - d4/d0

#Calculate Delta AICc
DeltaAIC0 <- AICc(NullModel) - AICc(Bmodel) #Null model
DeltaAIC1 <- AICc(FullModel) - AICc(Bmodel) #full model
DeltaAIC2 <- AICc(Bmodel) - AICc(Bmodel) #backward model
DeltaAIC3 <- AICc(Fmodel) - AICc(Bmodel) #forward model
DeltaAIC4 <- AICc(Smodel) - AICc(Bmodel) #stepwise model

#Roc curve for data
roc(datWorking$K10.2Lv, Bmodel$fitted.values, plot=TRUE, legacy.axes = TRUE, xlab="False Positive Percentages", ylab="True positive percentage", title="ROC Curve For Backwards Model")

#Determine sensitivity and specificity (0.45 threshold)
threshold=0.45
predicted_values<-ifelse(predict(Bmodel,type="response")>threshold,1,0)
actual_values<-Bmodel$y
conf_matrix<-table(predicted_values,actual_values)
specificity(conf_matrix)
sensitivity(conf_matrix)


#Generate OR and 95% CI in one line
exp(cbind(OR=coef(Bmodel),confint(Bmodel, level = 0.95)))

############################################# TASK 3 ##############################################################################

#Generate summary plots (To help me understand and describe data in summary form)
p9 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Events, fill = datWorking$Events), show.legend = FALSE) + xlab("Event Stress") + ggtitle("Event Stress Frequency (n=680)")
p10 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Partner, fill = datWorking$Partner), show.legend = FALSE) + xlab("Partner Stress") + ggtitle("Partner Stress Frequency (n=680)")
p11 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Children, fill = datWorking$Children), show.legend = FALSE) + xlab("Children Stress") + ggtitle("Child Stress Frequency (n=680)")
p12 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Parents, fill = datWorking$Parents), show.legend = FALSE) + xlab("Parents Stress") + ggtitle("Parents Stress Frequency (n=680)")
p13 <- ggplot(datWorking) + geom_bar(aes(x = datWorking$Finance, fill = datWorking$Finance), show.legend = FALSE) + xlab("Finance Stress") + ggtitle("Finance Stress Frequency (n=680)")

multiplot(p9,p10,p11,p12,p13, cols=2)

#Summarise demographic percentages and counts and frequencies

tableEvents <- table(datWorking$Events) #summary for Events
prop.table(tableEvents)

tablePartner <- table(datWorking$Partner) #summary for Partner
prop.table(tablePartner)

tableChildren <- table(datWorking$Children) #summary for Children
prop.table(tableChildren)

tableParents <- table(datWorking$Parents) #summary for Parents
prop.table(tableParents)

tableFinance <- table(datWorking$Finance) #summary for Finance
prop.table(tableFinance)

#Preform chi squared test for k10 against each categorical variable

i <- table(datWorking$K10.4Lv, datWorking$Events) #independence test for Events
prop.table(i)
chisq.test(datWorking$K10.4Lv, datWorking$Events)

j <- table(datWorking$K10.4Lv, datWorking$Partner) #independence test for Partner
prop.table(j)
chisq.test(datWorking$K10.4Lv, datWorking$Partner)

K <- table(datWorking$K10.4Lv, datWorking$Children) #independence test for Children
prop.table(K)
chisq.test(datWorking$K10.4Lv, datWorking$Children)

L <- table(datWorking$K10.4Lv, datWorking$Parents) #independence test for Parents
prop.table(L)
chisq.test(datWorking$K10.4Lv, datWorking$Parents)

M <- table(datWorking$K10.4Lv, datWorking$Finance) #independence test for Finance
prop.table(M)
chisq.test(datWorking$K10.4Lv, datWorking$Finance)

#Fit logistics regression models (+ AICc, Deviance and pseudo R^2 for all)

#Null model
NullModel2 <- glm(K10.2Lv~1,family="binomial",data=datWorking)
summary(NullModel2)
AICc(NullModel2) 
d00 <- deviance(NullModel2)

#Full model -> *********old model + 5 new predictors************
FullModel2 <- glm(K10.2Lv ~ Depedants + Occupation + Roster + Events + Children + Partner + Parents + Finance, data = datWorking, family = "binomial")
summary(FullModel2)
AICc(FullModel2)  
d11 <- deviance(FullModel)
FullRsquared2 <-1 - d11/d00

#Backwards selection model --- most parcimonious
Bmodel2 <- step(FullModel2,direction="backward",k=2)
summary(Bmodel2)
AICc(Bmodel2) 
d22 <- deviance(Bmodel2)
BRsquared1 <-1 - d22/d00

#Forward selection
Fmodel2 <- step(NullModel2,scope=list(lower=NullModel2,upper=FullModel2),direction="forward",k=2)
summary(Fmodel2)
AICc(Fmodel2)
d33 <- deviance(Fmodel)
FRsquared1 <-1 - d33/d00

#Stepwise model
Smodel2 <- step(FullModel2,direction="both",k=2) 
summary(Smodel2)
AICc(Smodel2)
d44 <- deviance(Smodel2)
SRsquared1 <-1 - d44/d00

#Calculate Delta AICc
DeltaAIC00 <- AICc(NullModel2) - AICc(Bmodel2) #Null model
DeltaAIC11 <- AICc(FullModel2) - AICc(Bmodel2) #full model
DeltaAIC22 <- AICc(Bmodel2) - AICc(Bmodel2) #backward model
DeltaAIC33 <- AICc(Fmodel2) - AICc(Bmodel2) #Forward model
DeltaAIC44 <- AICc(Smodel2) - AICc(Bmodel2) #Stepwise model delta AIC

#Roc curve for new model
roc(datWorking$K10.2Lv, Bmodel2$fitted.values, plot=TRUE, legacy.axes = TRUE, xlab="False Positive Percentages", ylab="True positive percentage", title="ROC Curve For Backwards Model")

#Determine sensitivity and specificity (0.5 threshold)
threshold2=0.5
predicted_values2<-ifelse(predict(Bmodel2,type="response")>threshold2,1,0)
actual_values2<-Bmodel2$y
conf_matrix2<-table(predicted_values2,actual_values2)
specificity(conf_matrix2)
sensitivity(conf_matrix2)

#Generate OR and 95% CI in one line
exp(cbind(OR=coef(Bmodel2),confint(Bmodel2, level = 0.95)))

#Plot increase in stress probability against each categorical variable
plot(predictorEffect("Occupation", Bmodel2))
plot(predictorEffect("Roster", Bmodel2))
plot(predictorEffect("Events", Bmodel2))
plot(predictorEffect("Partner", Bmodel2))
plot(predictorEffect("Finance", Bmodel2))


#Accuracy of models LOOCV (RMSE, PRESS etc calculated in other file)

# define training control
train_control <- trainControl(method = "LOOCV")

# train the model on training set
m1 <- train(K10.2Lv ~ Depedants + Occupation + Roster ,
                 data = datWorking,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())

# print cv scores
print(m1)


# train the model on training set
m2 <- train(K10.2Lv ~ Occupation + Roster + Events + Partner + Finance ,
                 data = datWorking,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())

# print cv scores
print(m2)
















