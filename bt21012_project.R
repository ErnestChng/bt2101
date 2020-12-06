library(readr)  # package for reading
library(dplyr) # package for data transformation

# Original data
data <- read.table("/Users/ernestchng/Desktop/ErnestChng/School/NUS/BusinessAnalytics/AY2019:20_SEM1/BT2101/Project/card.csv", sep=',', skip=2, header=FALSE)
header1 <- scan("/Users/ernestchng/Desktop/ErnestChng/School/NUS/BusinessAnalytics/AY2019:20_SEM1/BT2101/Project/card.csv", sep=',', nlines=1, what=character())
header2 <- scan("/Users/ernestchng/Desktop/ErnestChng/School/NUS/BusinessAnalytics/AY2019:20_SEM1/BT2101/Project/card.csv", sep=',', skip=1, nlines=1, what=character())
colnames(data) <- header2
data$`default payment next month` <- as.factor(data$`default payment next month`)
data$`default payment next month` <- as.numeric(data$`default payment next month`)

# Pre-processed data (filtered and scaled)
data <- read.table("/Users/ernestchng/Desktop/ErnestChng/School/NUS/BusinessAnalytics/AY2019:20_SEM1/BT2101/Project/processed_data.csv", sep=',', skip=1, header=FALSE)
header1 <- scan("/Users/ernestchng/Desktop/ErnestChng/School/NUS/BusinessAnalytics/AY2019:20_SEM1/BT2101/Project/processed_data.csv", sep=',', nlines=1, what=character())
header2 <- scan("/Users/ernestchng/Desktop/ErnestChng/School/NUS/BusinessAnalytics/AY2019:20_SEM1/BT2101/Project/processed_data.csv", sep=',', skip=0, nlines=1, what=character())
colnames(data) <- header2
data$`default payment next month` <- as.factor(data$`default payment next month`)
data$`default payment next month` <- as.numeric(data$`default payment next month`)

data <- data[,2:25] # removing ID column


#### 02 EXPLORATORY DATA ANALYSIS ####
levels(factor(data$SEX)) #"1" "2"
levels(factor(data$EDUCATION)) #"0" "1" "2" "3" "4" "5" "6"
levels(factor(data$MARRIAGE)) #"0" "1" "2" "3"

levels(factor(data$PAY_0)) #"-2" "-1" "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8" 
levels(factor(data$PAY_2)) #"-2" "-1" "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8" 
levels(factor(data$PAY_3)) #"-2" "-1" "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8" 
levels(factor(data$PAY_4)) #"-2" "-1" "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8" 
levels(factor(data$PAY_5)) #"-2" "-1" "0"  "2"  "3"  "4"  "5"  "6"  "7"  "8"
levels(factor(data$PAY_6)) #"-2" "-1" "0"  "2"  "3"  "4"  "5"  "6"  "7"  "8"

colSums(sapply(train, is.na)) # no missing values

nrow(data) - nrow(unique(data)) # 0, no duplicated rows

#install.packages("tidyverse")
#install.packages("funModeling")
#install.packages("Hmisc")
library(funModeling) 
library(tidyverse) 
library(Hmisc)

basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  #freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}
basic_eda(card)

sapply(data, class)

apply(data, 2, function(x) any(is.null(x) | is.na(x) | is.nan(x)))

summary(data)
unique(data$EDUCATION) # 2 1 3 5 4 6 0  
unique(data$MARRIAGE)  # 1 2 3 0      
unique(data$PAY_0)     # 2 -1  0 -2  1  3  4  8  7  5  6

par(mfrow=c(4, 6))
for (i in 2:25) {
  hist(data[,i],col="gray", border = "white", freq=FALSE)
  d = density(data[,i])
  lines(d,col="red")
}

data$`default payment next month` <- 
  as.factor(data$`default payment next month`)
summary(data$`default payment next month`)




#### 03 DATA PRE-PROCESSING ####
MD <- mahalanobis(data[,c(1,5,12:23)],
                  colMeans(data[,c(1,5,12:23)]),
                  cov(data[,c(1,5,12:23)]) )
data <- data[(which(MD <= 10)),]




#### SPLITTING THE DATA INTO TRAIN AND TEST ####
set.seed(123)
n = length(data$LIMIT_BAL)
index <- 1:nrow(data)
testindex <- sample(index, trunc(2*n)/3)
test.data <- data[testindex,]
train.data <- data[-testindex,]


#### 04 FEATURE SELECTION ####
## Corelation ##
# Generate correlation matrix
cor <- cor(train.data[,1:24])
round(cor, 2)

# Build correplot to visualise the correlation matrix
library(corrplot)
#par(mfrow=c(1, 1))
corrplot(cor, title="Correlation Matrix", method="circle", is.corr=FALSE, type="lower", tl.col="black", tl.srt=45)

#install.packages("GGally")
library(GGally)
set.seed(123)
# Plotting the correlation matrix
ggcorr(train.data, nbreaks=6, low="steelblue4", mid="white", 
       high="firebrick3", label=TRUE, label_size=4, 
       color="grey50", name="Correlation")

# Checking VIF score
library(mctest)
imcdiag(train.data[,2:24], train.data$`default payment next month`, method="VIF")

## Hypothesis Testing ##
set.seed(123)

# Running independent t-tests for continuous variables
cont = c(1,5,12,13,14,15,16,17,18,19,20,21,22,23)
for (i in cont) {
  print(t.test(train.data[,i], train.data$`default payment next month`)$p.value)
}

# Running Chi-square tests for categorical variables
library(MASS)
cat = c(2,3,4,6,7,8,9,10,11)
for (i in cat) {
  tbl = table(train.data$`default payment next month`, train.data[,i])
  print(colnames(data[i]))
  print(chisq.test(tbl)$p.value)
}

## Information Gain ##
#install.packages("FSelector")
library(FSelector)
nformation.gain(`default payment next month`~., data=train.data)

## Stepwise Forward and Backward Selection ##
# Step 1: Define base intercept only model
base.mod <- lm(`default payment next month`~1 , data=train.data)

# Step 2: Full model with all predictors
all.mod <- lm(`default payment next month`~. , data=train.data)

# Step 3: Perform stepwise algorithm. Direction=forwards/backwards/both determines the method
stepMod <- step(base.mod, scope=list(lower=base.mod, upper=all.mod), direction="both", trace=0, steps=1000)  
forwardMod <- step(base.mod, scope=list(lower=base.mod, upper=all.mod), direction="forward", trace=0, steps=1000)
backwardMod <- step(all.mod, scope=list(lower=base.mod, upper=all.mod), direction="backward", trace=0, steps=1000)

# Step 4: Get the selected variables.
vars_step <- names(unlist(stepMod[[1]])) 
vars_step <- vars_step[!vars_step %in% "(Intercept)"] # remove intercept
vars_forward <- names(unlist(forwardMod[[1]])) 
vars_forward <- vars_forward[!vars_forward %in% "(Intercept)"] # remove intercept
vars_backward <- names(unlist(backwardMod[[1]])) 
vars_backward <- vars_backward[!vars_backward %in% "(Intercept)"] # remove intercept

# Step 5: Print the selected variables.
print(vars_step)
print(vars_forward)
print(vars_backward)

## Recursive Feature Elimination (RFE) ##
set.seed(123)
control <- rfeControl(functions = rfFuncs, method = 'cv', number = 10, allowParallel = TRUE)
results <- rfe(train.data[,1:23], train.data$`default payment next month`, sizes = c(1:23), rfeControl = control)

set.seed(123)
options(warn=-1)

subsets <- c(1:5, 10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=data[, c(1:3, 5:13)], y=data$`default payment next month`,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

## Lasso ##
# Loading glmnet package
install.packages("glmnet")
library(glmnet)

# Using LASSO for variable selection
set.seed(123)
feat_mod_select <- cv.glmnet(as.matrix(train.data[,1:23]) , train.data[, 24], standardize = TRUE, alpha =1)

# Checking coefficients with the minimum cross-validation error
as.matrix(round(coef(feat_mod_select, feat_mod_select$lambda.min),5))

# Results
plot(feat_mod_select)

## Boruta ##
# install.packages('Boruta')
library(Boruta)

# Perform Boruta search
boruta_output <- Boruta(`default payment next month` ~ ., data=train.data, doTrace=0) 

names(boruta_output)

# Get significant variables without tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = FALSE)
print(boruta_signif)

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

## RandomForest ##
# Loading library
library(randomForest)

# Using random forest for variable selection
rfModel <-randomForest(`default payment next month` ~ ., data = train.data)

# Getting the list of important variables
varImp(rfModel)

# Train an RRF model and compute variable importance.
library(caret)
set.seed(123)
#rrfMod <- train(`default payment next month` ~ ., data=train.data, method="RRF", do.trace=TRUE)
rrfMod <- train(y=data[,24], x=data[,6:7], data=train.data, method="RRF", do.trace=TRUE)
rrfImp <- varImp(rrfMod, scale=F)
rrfImp
plot(rrfImp)

rrfMod2 <- train(y=data[,24], x=data[,6:7], data=train.data, method="RRF", do.trace=TRUE, importance=T, ntrees=10)
rrfImp2 <- varImp(rrfMod2, scale=F)
rrfImp2
plot(rrfImp2)

control <- trainControl(method="repeatedcv", number=10, repeats=3, allowParallel=TRUE)
mtry <- sqrt(ncol(train.data))
tunegrid <- expand.grid(.mtry=mtry)
rrfMod3 <- train(y=data[,24], x=data[,1:23], data=train.data, method="RRF", do.trace=TRUE, importance=T, ntrees=500, , trControl=control)

## RANDOM FOREST ##
library(caret)

control <- trainControl(method="cv", number=10, 
                        verboseIter = TRUE, allowParallel = TRUE)
rfMod <- train(y=data[,24], x=data[,1:23], data=train.data,
               method="rf", preProcess = "scale", do.trace=TRUE,
               importance=T, ntrees=500, trControl=control)

# Checking Accuracy with test data
postResample(predict(rfMod, newdata = test.data[,1:23]), 
             test.data[,24])

## Rpart ##
# Train an rpart model and compute variable importance.
library(caret)
set.seed(123)
rPartMod <- train(`default payment next month` ~ ., data=train.data, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

## Relative Importance from Logistic Regression ##
# install.packages('relaimpo')
library(relaimpo)
set.seed(123)



#### 05 MODELS ####

## Support Vector Machine ##
library(e1071)
library(Metrics)
set.seed(123)

# Linear SVM
model_svm_linear <- svm(`default payment next month` ~. , data = train.data, type = "C-classification", kernel = "linear")
pred_linear <- predict(model_svm_linear, test.data)
table(pred=pred_linear,actual=test.data$`default payment next month`)
auc(pred_linear, test.data$`default payment next month`)
mean(test.data$`default payment next month` == pred_linear)

# Polynomial SVM
model_svm_polynomial <- svm(`default payment next month` ~. , data = train.data, type = "C-classification", kernel = "polynomial")
pred_polynomial <- predict(model_svm_polynomial, test.data)
table(pred=pred_polynomial,actual=test.data$`default payment next month`)
auc(pred_polynomial, test.data$`default payment next month`)
mean(test.data$`default payment next month` == pred_polynomial)

# Radial SVM
model_svm_radial <- svm(`default payment next month` ~. , data = train.data, type = "C-classification", kernel = "radial")
pred_radial <- predict(model_svm_radial, test.data)
table(pred=pred_radial,actual=test.data$`default payment next month`)
auc(pred_radial, test.data$`default payment next month`)
mean(test.data$`default payment next month` == pred_radial)

# Sigmoid SVM
model_svm_sigmoid <- svm(`default payment next month` ~. , data = train.data, type = "C-classification", kernel = "sigmoid")
pred_sigmoid <- predict(model_svm_sigmoid, test.data)
table(pred=pred_sigmoid,actual=test.data$`default payment next month`)
auc(pred_sigmoid, test.data$`default payment next month`)
mean(test.data$`default payment next month` == pred_sigmoid)

#Feature selection
#Boruta
model_svm_radial <- svm(`default payment next month` ~ `PAY_0`+`PAY_2`+`PAY_3`+`PAY_4`+`PAY_5`+`BILL_AMT4` , data = train.data, type = "C-classification", kernel = "radial")
pred_radial <- predict(model_svm_radial, test.data)
table(pred=pred_radial,actual=test.data$`default payment next month`)
auc(pred_radial, test.data$`default payment next month`)
mean(test.data$`default payment next month` == pred_radial)

#RFE
model_svm_radial <- svm(`default payment next month` ~ `PAY_0`+`PAY_2` , data = train.data, type = "C-classification", kernel = "radial")
pred_radial <- predict(model_svm_radial, test.data)
table(pred=pred_radial,actual=test.data$`default payment next month`)
auc(pred_radial, test.data$`default payment next month`)
mean(test.data$`default payment next month` == pred_radial)

#Rpart
model_svm_radial <- svm(`default payment next month` ~ `PAY_0`+`PAY_2`+`PAY_3`+`PAY_4`+`PAY_5` , data = train.data, type = "C-classification", kernel = "radial")
pred_radial <- predict(model_svm_radial, test.data)
table(pred=pred_radial,actual=test.data$`default payment next month`)
auc(pred_radial, test.data$`default payment next month`)
mean(test.data$`default payment next month` == pred_radial)

# vars_step
model_svm_radial <- svm(`default payment next month` ~ `PAY_0`+`BILL_AMT1`+`PAY_3`+`PAY_AMT1`+`BILL_AMT6`+`PAY_AMT6`+`MARRIAGE`+`PAY_2`+`PAY_AMT5`+`BILL_AMT2`+`PAY_5`, data = train.data, type = "C-classification", kernel = "radial")
pred_radial <- predict(model_svm_radial, test.data)
table(pred=pred_radial,actual=test.data$`default payment next month`)
auc(pred_radial, test.data$`default payment next month`)
mean(test.data$`default payment next month` == pred_radial)

## Decision Tree (Random Forest) ##
## Random Forest ##
library(randomForest)
set.seed(123)

# Feature Selection from Boruta Algorithm
rf_boruta <- randomForest(y = train.data[,24], x = train.data[,c(6:10,15)], ytest = test.data[,24], xtest = test.data[,c(6:10,15)], importance = TRUE)

# Feature Selection from RFE and RF (SAME)
rfMod_rfe <- train(y=data[,24], x=data[,c(6,7)], data=test.data, method="rf", preProcess = "scale", do.trace=TRUE, importance=T, ntrees=500, trControl=control)

rf_rfe <- randomForest(y = train.data[,24], x = train.data[,6:7], ytest = test.data[,24], xtest = test.data[,6:7], importance = TRUE)

# Feature Selection from RPart
rf_rpart <- randomForest(y = train.data[,24], x = train.data[,6:10], ytest = test.data[,24], xtest = test.data[,6:10], importance = TRUE)

# Feature Selection from STEPWISE
rf_step <- randomForest(y = train.data[,24], x = train.data[,c(4,6,7,8,10,12,13,17,18,22,23)], ytest = test.data[,24], xtest = test.data[,c(4,6,7,8,10,12,13,17,18,22,23)], importance = TRUE)

## Neural Network ##
set.seed(123)
library(RSNNS)

##BORUTA
nnBoruta = mlp(train.data[,c(6:10,15)],train.data[,24],
               size = c(500), maxit = 800)
predictions <- predict(nnBoruta, train.in[,c(6:10,15)])
pred.class = ifelse(predictions >= 0.5, 1 ,0)
mean(pred.class == train.target) # 0.8128589
predictions <- predict(nnBoruta, test.in[,c(6:10,15)])
pred.class = ifelse(predictions >= 0.5, 1 ,0)
mean(pred.class == test.target) # 0.8071665

##RFE
nnRFE = mlp(train.data[,c(6:7)],train.data[,24],
            size = c(500), maxit = 800)
predictions <- predict(nnRFE, train.in[,c(6:7)])
pred.class = ifelse(predictions >= 0.5, 1 ,0)
mean(pred.class == train.target) # 0.8052434
predictions <- predict(nnRFE, test.in[,c(6:7)])
pred.class = ifelse(predictions >= 0.5, 1 ,0)
mean(pred.class == test.target) # 0.8091641

##RPART
nnRPART = mlp(train.data[,c(6:10)],train.data[,24],
              size = c(500), maxit = 800)
predictions <- predict(nnRPART, train.in[,c(6:10)])
pred.class = ifelse(predictions >= 0.5, 1 ,0)
mean(pred.class == train.target) # 0.8104869
predictions <- predict(nnRPART, test.in[,c(6:10)])
pred.class = ifelse(predictions >= 0.5, 1 ,0)
mean(pred.class == test.target) # 0.8067295

##STEPWISE
nnSTEP = mlp(train.data[,c(4,6:8,12,13,17,18,22,23)],
             train.data[,24],size = c(500), maxit = 800)
predictions <- predict(nnSTEP, 
                       train.in[,c(4,6:8,12,13,17,18,22,23)])
pred.class = ifelse(predictions >= 0.5, 1 ,0)
mean(pred.class == train.target) # 0.8174782
predictions <- predict(nnSTEP, 
                       test.in[,c(4,6:8,12,13,17,18,22,23)])
pred.class = ifelse(predictions >= 0.5, 1 ,0)
mean(pred.class == test.target) # 0.8040452

## Logistic Regression ##

library(readr)
library(InformationValue)
library(dplyr)

set.seed(14)
n = nrow(data[,1])
index <- 1:nrow(data)
testindex <- sample(index, trunc(2*n)/3)
test.data <- data[testindex,]
train.data <- data[-testindex,]

# RFE
# PAY_0, PAY_2
fit_glm.RFE<- glm(default.payment.next.month~ PAY_0+ PAY_2,
                  data=train.data, family = "binomial")
summary(fit_glm.RFE)

train.pred <- predict(fit_glm.RFE, data=(train.data %>% select(PAY_0, PAY_2)),
                      type="response")
plotROC(actuals=train.data[,24], predictedScores=train.pred)
optcut <- optimalCutoff(train.data[,24], train.pred, optimiseFor="misclasserror")
train.binpred<-ifelse(train.pred < optcut,0,1)
train.binpred
table(train.data$default.payment.next.month, train.binpred)
mean(train.data[,24] == train.binpred) # training accuracy of 81.02%
test.pred<-predict(fit_glm.RFE, (test.data %>% select(PAY_0, PAY_2)),
                   type="response")
plotROC(actuals=test.data[24], predictedScores=test.pred)
test.binpred <-ifelse(test.pred < optcut,0,1)
table(test.data$default.payment.next.month,test.binpred)
test.binpred
mean(test.data[24] == test.binpred) # test accuracy of 80.75%


# Boruta
# PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, BILL_AMT4
set.seed(4)
n = nrow(data[,1])
index <- 1:nrow(data)
testindex <- sample(index, trunc(2*n)/3)
test.data <- data[testindex,]
train.data <- data[-testindex,]

fit_glm.boruta <- glm(default.payment.next.month~ PAY_0+ PAY_2+ PAY_3+ PAY_4+ PAY_5+ BILL_AMT4,
                      data=train.data, family = "binomial")
summary(fit_glm.boruta)

train.pred <- predict(fit_glm.boruta, data=(train.data %>% select(PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, BILL_AMT4)),
                      type="response")
plotROC(actuals=train.data[,24], predictedScores=train.pred)
optcut <- optimalCutoff(train.data[,24], train.pred, optimiseFor="misclasserror")
train.binpred<-ifelse(train.pred < optcut,0,1)
table(train.data$default.payment.next.month, train.binpred)
train.binpred
mean(train.data[,24] == train.binpred) # training accuracy 81.87%
test.pred<-predict(fit_glm.boruta, (test.data %>% select(PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, BILL_AMT4)),
                   type="response")
plotROC(actuals=(test.data$default.payment.next.month), predictedScores=test.pred)
test.binpred <-ifelse(test.pred < optcut,0,1)
table((test.data)$default.payment.next.month,test.binpred)
test.binpred
mean((test.data)[24] == test.binpred) # test accuracy 80.44%

# RPART
# PAY_0, PAY_2, PAY_3, PAY_4, PAY_5
fit_glm.RPART<- glm(default.payment.next.month~ PAY_0+ PAY_2+PAY_3+ PAY_4+ PAY_5,
                    data=train.data, family = "binomial")
summary(fit_glm.RPART)

train.pred <- predict(fit_glm.RPART, data=(train.data %>% select(PAY_0, PAY_2, PAY_3, PAY_4, PAY_5)),
                      type="response")
plotROC(actuals=train.data[,24], predictedScores=train.pred)
optcut <- optimalCutoff(train.data[,24], train.pred, optimiseFor="misclasserror")
train.binpred<-ifelse(train.pred < optcut,0,1)
table(train.data$default.payment.next.month, train.binpred)
train.binpred
mean(train.data[,24] == train.binpred)
# training accuracy of 81.90%
test.pred<-predict(fit_glm.RPART, (test.data %>% select(PAY_0, PAY_2, PAY_3, PAY_4, PAY_5)),
                   type="response")
plotROC(actuals=test.data[24], predictedScores=test.pred)
test.binpred <-ifelse(test.pred < optcut,0,1)
table(test.data$default.payment.next.month, test.binpred)
test.binpred
mean(test.data[24] == test.binpred) # test accuracy of 80.47%

# STEPWISE
# PAY_0, BILL_AMT1, PAY_3, PAY_AMT1, BILL_AMT6, PAY_AMT6, MARRIAGE, PAY_2, PAY_AMT5, BILL_AMT2, PAY_AMT5
fit_glm.STEPWISE<- glm(default.payment.next.month~ PAY_0+ BILL_AMT1+ PAY_3+ PAY_AMT1+ BILL_AMT6+ PAY_AMT6
                       + MARRIAGE+ PAY_2+ PAY_AMT5+ BILL_AMT2+ PAY_AMT5, data=train.data, family = "binomial")
summary(fit_glm.STEPWISE)

train.pred <- predict(fit_glm.STEPWISE, data=(train.data %>% select(PAY_0, BILL_AMT1, PAY_3, PAY_AMT1,
                                                                    BILL_AMT6, PAY_AMT6, MARRIAGE, PAY_2,
                                                                    PAY_AMT5, BILL_AMT2, PAY_AMT5)),
                      type="response")
plotROC(actuals=train.data[,24], predictedScores=train.pred)
optcut <- optimalCutoff(train.data[,24], train.pred, optimiseFor="misclasserror")
train.binpred<-ifelse(train.pred < optcut,0,1)
table(train.data$default.payment.next.month, train.binpred)
train.binpred
mean(train.data[,24] == train.binpred) # accuracy of 81.76%

test.pred<-predict(fit_glm.STEPWISE, (test.data %>% select(PAY_0, BILL_AMT1, PAY_3, PAY_AMT1, BILL_AMT6,
                                                           PAY_AMT6, MARRIAGE, PAY_2, PAY_AMT5, BILL_AMT2,
                                                           PAY_AMT5)),
                   type="response")
plotROC(actuals=test.data[24], predictedScores = test.pred)
test.binpred <-ifelse(test.pred < optcut,0,1)
table(test.data$default.payment.next.month,test.binpred)
test.binpred
mean(test.data[24] == test.binpred) # truncated test accuracy of 80.45%

## Naiv Bayes ##
library(e1071)

NBclassfier=naiveBayes(as.factor(default.payment.next.month)~., data=train.data)
print(NBclassfier)

printALL=function(model){
  trainPred=predict(model, newdata = train.data, type = "class")
  trainTable=table(train.data$default.payment.next.month, trainPred)
  testPred=predict(NBclassfier, newdata=test.data, type="class")
  testTable=table(test.data$default.payment.next.month, testPred)
  trainAcc=(trainTable[1,1]+trainTable[2,2])/sum(trainTable)
  testAcc=(testTable[1,1]+testTable[2,2])/sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}
printALL(NBclassfier)

# Contingency Table for Training Data
trainPred
# Contingency Table for Test Data
testPred
Accuracy




#### 06 MODEL EVALUATION ####
library(Metrics)
library(caret)
control <- trainControl(method="cv", number = k, verboseIter = TRUE, 
                        allowParallel = TRUE, classProbs = TRUE)
set.seed(123)

## SUPPORT VECTOR MACHINE ##
levels(train.data[,24]) <- make.names(levels(factor(train.data[,24])))
levels(test.data[,24]) <- make.names(levels(factor(test.data[,24])))
set.seed(123)

# LINEAR #
rf_rfe <- train(y=train.data[,24], x=train.data[,6:7], 
                method='svmLinear', preProcess = "scale",importance=T,
                trControl=control)
testPred <- predict(rf_rfe, newdata = test.data[,6:7])
accuracy(testPred, test.data[,24])

# POLYNOMIAL #
rf_rfe <- train(y=train.data[,24], x=train.data[,6:7], 
                method="svmPoly", preProcess = "scale",importance=T,
                trControl=control)
testPred <- predict(rf_rfe, newdata = test.data[,6:7])
accuracy(testPred, test.data[,24])


# RADIAL #
rf_rfe <- train(y=train.data[,24], x=train.data[,6:7], 
                method='svmRadial', preProcess = "scale",importance=T,
                trControl=control)
testPred <- predict(rf_rfe, newdata = test.data[,6:7])
accuracy(testPred, test.data[,24])

## RANDOM FOREST ##
rf_rfe <- train(y=train.data[,24], x=train.data[,c(6,7)], method="rf", 
                preProcess = "scale", do.trace=TRUE, importance=T,
                ntrees=500, trControl=control)
postResample(predict(rf_rfe, newdata = test.data[,1:23]), test.data[,24])

## NEURAL NETWORK ##
rf_rfe <- train(y=train.data[,24], x=train.data[,c(6,7)], method="mlp", 
                preProcess = "scale", do.trace=TRUE, importance=T,
                ntrees=500, trControl=control)
postResample(predict(rf_rfe, newdata = test.data[,1:23]), test.data[,24])

## LOGISTIC REGRESSION ##
rf_rfe <- train(y=train.data[,24], x=train.data[,6:7], method='glm', 
                             family = "binomial", trControl=control)
testPred <- predict(rf_rfe, newdata = test.data[,6:7])
accuracy(testPred, test.data[,24])

## NAIVE BAYES ##
rf_rfe <- train(y=train.data[,24], x=train.data[,6:7], 
                                 method='naive_bayes', trControl=control)
testPred <- predict(rf_rfe, newdata = test.data[,6:7])
accuracy(testPred, test.data[,24])




#### CORRELATION ####
library(caret)
set.seed(123)
corrMat <- cor(data[1:23])
highlyCorrelated <- findCorrelation(corrMat, cutoff = 0.5) #anything with correlation above 0.5
corrplot(corrMat, method="circle", is.corr=FALSE, type="lower", tl.col="black", tl.srt=45)
print(highlyCorrelated)
colnames(data[highlyCorrelated])

res <- cor(data[1:23])
round(res, 2)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#### RANK FEATURES BY IMPORTANCE ####
set.seed(123)
#install.packages("mlbench")
#install.packages("caret")
library(mlbench)
library(caret)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(data$`default payment next month`~., data=data, method="mlp", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


#### RANDOM FOREST ####
library(randomForest)
library(Metrics)
# Applying Random Forest for all features 
set.seed(123)
model <- randomForest(`default payment next month` ~ ., data = data)
model
# Prediction using test data
pred <- predict(model, test.data[,-24])
table(pred)
# Checking accuracy: AUROC = 0.9940198
auc(pred, test.data$`default payment next month`)
importance(model)
varImpPlot(model)
# Applying Random Forest for top 3 features
set.seed(123)
model1 <- randomForest(`default payment next month` ~ 
                         `PAY_0` + `BILL_AMT1` + `AGE` + `BILL_AMT2` + `BILL_AMT3` + `PAY_AMT1` + `LIMIT_BAL` +
                         `BILL_AMT4` + `BILL_AMT6` + `BILL_AMT5` + `PAY_AMT2`, data = data)
model1
pred1 <- predict(model1, test.data[,-24])
table(pred1)
auc(pred1, test.data$`default payment next month`)


