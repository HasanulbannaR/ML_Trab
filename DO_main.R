library(readxl)
library(caret)
library(tidyverse)
library(tableone)
library(PerformanceAnalytics)
library(kableExtra)
library(ROCR)
library(randomForest)
library(neuralnet)
library(e1071)
library(cutpointr)
library(measures)
library(mlbench)
library(Hmisc)
library(CalibrationCurves)
library("writexl")

data <- read_excel("........../DO_data.xlsx")
set.seed(1)

op_col=24
clean_data<-data
clean_data<-as.data.frame(clean_data)
clean_data[,op_col]<-as.factor(clean_data[,op_col])

colnames(clean_data)<-c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24")
clean_data<-upSample(x=clean_data[,-ncol(clean_data)],y=clean_data$a24)
colnames(clean_data)[24]<-c("a24")

tone_vars = colnames(clean_data)
tone <- tableone::CreateTableOne(vars = tone_vars, factorVars=c("a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a19","a20","a21","a22"), strata = c("a24") , data =  clean_data)

tone_df <- print(tone, printToggle = FALSE, noSpaces = TRUE)
kable(tone_df) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
#############################################################################
#ODDs ratio

bi.final <- data.frame(matrix(nrow = 0, ncol = 3))
## loop through the columns of the data
for(i in 1:ncol(clean_data))
{
  # gets the variable name for the column
  iv <- colnames(clean_data)[i]  
  # we need to check for an error because the column data may not be regressable (ie only one value for the variable for all participants)
  possibleError <- tryCatch( 
    #run the model
    bi.model <- glm (a24 ~ get(iv), data = clean_data, family = 
                       binomial(link="logit")), error = function(e) e)
  ## if there was no error we will run the following code to add the results to our 
  ## bi.final table
  if(!inherits(possibleError, "error")){
    ## another error check, this step also produced an error at times
    possibleError2 <- tryCatch(
      ## convert the coefficient into an OR and compute the confidence interval
      bi.results <- exp(cbind(OR = coef(bi.model), confint(bi.model))),
      error = function(e) e)
    bi.results <- cbind(bi.results, rbind(summary(bi.model)$coefficients[7],summary(bi.model)$coefficients[8]))
    ## if there was no error above we will add the results to the table of results
    if(!inherits(possibleError2, "error")){
      
      ## add to the final bivariate results
      rownames(bi.results)[2] <- colnames(clean_data)[i]
      bi.final <- rbind(bi.final, bi.results)}}}

kable(bi.final) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
bi.signif <- bi.final[which(bi.final$`2.5 %` > 1 | bi.final$`97.5 %`<1),]

kable(bi.signif)  %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
##################################################################
runfold <- function(train, test)
{
  # stepwise regression requires setting the null model
  model.null = glm(a24 ~ 1, 
                   data=train,
                   family = binomial(link="logit"))
  # stepwise regression requires setting the full model with all 48 variables
  model.full= glm(a24 ~ a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23, data=train,
                  family = binomial(link="logit"))
  # this runs the stepwise regression and saves the regression equation
  step.results <- step(model.null,
                       scope = list(upper=model.full),
                       direction="both",
                       test="Chisq",
                       data=train, trace = 0)
  # now run the final chosen model
  final.model <- eval(step.results$call)
  # this applies the stepwise model and attempts to predict the results on the training set
  lr.pr <- predict(final.model, test, type="response") 
  # return the predicted values and the actual values
  return(list(lr.pr,test$a24))}
fivefold <- function(data){
  values <- rep(1:5, length.out=nrow(data))
  #randomly sort this vector
  random <- sample(values)
  #add this vector to the data, essentially assigning each row a random group
  data$cv_group <- random
  
  train.one <- data[which(data$cv_group!= 1),]
  test.one <- data[which(data$cv_group== 1),]
  train.two <- data[which(data$cv_group!= 2),]
  test.two <- data[which(data$cv_group== 2),]
  train.three <- data[which(data$cv_group!= 3),]
  test.three <- data[which(data$cv_group== 3),]
  train.four <- data[which(data$cv_group!= 4),]
  test.four <- data[which(data$cv_group== 4),]
  train.five <- data[which(data$cv_group!= 5),]
  test.five <- data[which(data$cv_group== 5),]
  
  res1 <- runfold(train.one, test.one)
  res2 <- runfold(train.two, test.two)
  res3 <- runfold(train.three, test.three)
  res4 <- runfold(train.four, test.four)
  res5 <- runfold(train.five, test.five)
  
  predictions <- list(res1[1][[1]],res2[1][[1]],res3[1][[1]],res4[1][[1]],res5[1][[1]])
  actuals <- list(res1[2][[1]],res2[2][[1]],res3[2][[1]],res4[2][[1]],res5[2][[1]])
  return(list(predictions, actuals))}

predictions <- list()
actuals <- list()

for(i in 1:2){
  kfold_result <- fivefold(clean_data)
  predictions <- append(predictions, kfold_result[[1]])
  actuals <- append(actuals, kfold_result[[2]])
  message(i)}

pred <- ROCR::prediction(predictions, actuals) ####### originally it was "pred <- ROCR::prediction(predictions, actuals)"
avg_auc_curve_log_reg <- ROCR::performance(pred,"tpr","fpr")

for (i in 1:length(avg_auc_curve_log_reg@x.values)){
  ind<-which(avg_auc_curve_log_reg@x.values[[i]]==0)
  if (length(ind)>1){
    avg_auc_curve_log_reg@x.values[[i]]<-avg_auc_curve_log_reg@x.values[[i]][-ind[-1]]
    avg_auc_curve_log_reg@y.values[[i]]<-avg_auc_curve_log_reg@y.values[[i]][-ind[-1]]}
  else{
    avg_auc_curve_log_reg@x.values[[i]]<-avg_auc_curve_log_reg@x.values[[i]]
    avg_auc_curve_log_reg@y.values[[i]]<-avg_auc_curve_log_reg@y.values[[i]]}}

plot(avg_auc_curve_log_reg, avg="vertical",main='Average ROC Curve for Logistic Regression',col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

### And all the curves
plot(avg_auc_curve_log_reg,main='All the ROC Curves for Logistic Regression',col=2,lwd=1)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

### And lets compute the average ROC curve
auc_list <- ROCR::performance(pred,"auc")@y.values

aucs <- c()
for(num in auc_list) { aucs <- c(aucs, num)}
auc_mean_lr <- mean(aucs)
auc_min_lr <- min(aucs)
auc_max_lr <- max(aucs)
auc_sd_lr <-sd(aucs)

lr_predictions <- predictions
lr_actuals <- actuals
#######################################
runfold_rf <- function(train, test)
{
  ## this code runs the random forests on our 48 variables
  model.rf = randomForest(a24 ~ a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23, 
                          data=train)
  pr <- predict(model.rf, test, type="prob")[,2]
  return(list(pr,test$a24))}

fivefold_rf <- function(data){
  #create a vector with the appropriate number of 1,2,3,4,5
  values <- rep(1:5, length.out=nrow(data))
  #randomly sort this vector
  random <- sample(values)
  #add this vector to the data, essentially assigning each row a random group
  data$cv_group <- random
  
  ## next we want to assign the five test and training pairings
  train.one <- data[which(data$cv_group!= 1),]
  test.one <- data[which(data$cv_group== 1),]
  train.two <- data[which(data$cv_group!= 2),]
  test.two <- data[which(data$cv_group== 2),]
  train.three <- data[which(data$cv_group!= 3),]
  test.three <- data[which(data$cv_group== 3),]
  train.four <- data[which(data$cv_group!= 4),]
  test.four <- data[which(data$cv_group== 4),]
  train.five <- data[which(data$cv_group!= 5),]
  test.five <- data[which(data$cv_group== 5),]
  
  res1 <- runfold_rf(train.one, test.one)
  res2 <- runfold_rf(train.two, test.two)
  res3 <- runfold_rf(train.three, test.three)
  res4 <- runfold_rf(train.four, test.four)
  res5 <- runfold_rf(train.five, test.five)
  
  # make a list of predictions and actuals
  predictions <- list(res1[1][[1]],res2[1][[1]],res3[1][[1]],res4[1][[1]],res5[1][[1]])
  actuals <- list(res1[2][[1]],res2[2][[1]],res3[2][[1]],res4[2][[1]],res5[2][[1]])
  return(list(predictions, actuals))}

predictions <- list()
actuals <- list()

for(i in 1:2){
  kfold_result <- fivefold_rf(clean_data)
  predictions <- append(predictions, kfold_result[[1]])
  actuals <- append(actuals, kfold_result[[2]])
  message(i)}

pred <- ROCR::prediction(predictions, actuals) ####### originally it was "pred <- ROCR::prediction(predictions, actuals)"
avg_auc_curve_rf <- ROCR::performance(pred,"tpr","fpr")

for (i in 1:length(avg_auc_curve_rf@x.values)){
  ind<-which(avg_auc_curve_rf@x.values[[i]]==0)
  if (length(ind)>1){
    avg_auc_curve_rf@x.values[[i]]<-avg_auc_curve_rf@x.values[[i]][-ind[-1]]
    avg_auc_curve_rf@y.values[[i]]<-avg_auc_curve_rf@y.values[[i]][-ind[-1]]}
  else{
    avg_auc_curve_rf@x.values[[i]]<-avg_auc_curve_rf@x.values[[i]]
    avg_auc_curve_rf@y.values[[i]]<-avg_auc_curve_rf@y.values[[i]]}}

plot(avg_auc_curve_rf, avg="vertical",main='Average ROC Curve for Random Forests',col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

### And all the curvers
plot(avg_auc_curve_rf,main='All the ROC Curves for Random Forests',col=2,lwd=1)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

auc_list <- ROCR::performance(pred,"auc")@y.values
aucs <- c()
for(num in auc_list) { aucs <- c(aucs, num)}
auc_mean_rf <- mean(aucs)
auc_min_rf <- min(aucs)
auc_max_rf <- max(aucs)
auc_sd_rf <-sd(aucs)

rf_predictions <- predictions
rf_actuals <- actuals
#######################################
numeric_data <- clean_data

for(i in 1:ncol(numeric_data)) 
{
  if(!is.numeric(numeric_data[,i]))
  {
    numeric_data[,i] <- as.numeric(numeric_data[,i])}}

max <- apply(numeric_data,2,max)
min <- apply(numeric_data,2,min)
numeric_data <- as.data.frame(scale(numeric_data, center = min, scale = max - min))

runfold_ann <- function(train, test)
{
  model.ann <- neuralnet(a24 ~ a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23, 
                         data=train )
  prob <- neuralnet::compute(model.ann, test)
  prob.results <- prob$net.result
  return(list(prob.results,test$a24))}

fivefold_ann <- function(data){
  values <- rep(1:5, length.out=nrow(data))
  #randomly sort this vector
  random <- sample(values)
  #add this vector to the data, essentially assigning each row a random group
  data$cv_group <- random
  
  train.one <- data[which(data$cv_group!= 1),]
  test.one <- data[which(data$cv_group== 1),]
  train.two <- data[which(data$cv_group!= 2),]
  test.two <- data[which(data$cv_group== 2),]
  train.three <- data[which(data$cv_group!= 3),]
  test.three <- data[which(data$cv_group== 3),]
  train.four <- data[which(data$cv_group!= 4),]
  test.four <- data[which(data$cv_group== 4),]
  train.five <- data[which(data$cv_group!= 5),]
  test.five <- data[which(data$cv_group== 5),]
  
  res1 <- runfold_ann(train.one, test.one)
  res2 <- runfold_ann(train.two, test.two)
  res3 <- runfold_ann(train.three, test.three)
  res4 <- runfold_ann(train.four, test.four)
  res5 <- runfold_ann(train.five, test.five)
  predictions <- list(res1[1][[1]],res2[1][[1]],res3[1][[1]],res4[1][[1]],res5[1][[1]])
  actuals <- list(res1[2][[1]],res2[2][[1]],res3[2][[1]],res4[2][[1]],res5[2][[1]])
  
  return(list(predictions, actuals))}

predictions <- list()
actuals <- list()

for(i in 1:2){
  kfold_result <- fivefold_ann(numeric_data)
  predictions <- append(predictions, kfold_result[[1]])
  actuals <- append(actuals, kfold_result[[2]])
  message(i)}

pred <- ROCR::prediction(predictions, actuals) ####### originally it was "pred <- ROCR::prediction(predictions, actuals)"
avg_auc_curve_ann <- ROCR::performance(pred,"tpr","fpr")

for (i in 1:length(avg_auc_curve_ann@x.values)){
  ind<-which(avg_auc_curve_ann@x.values[[i]]==0)
  if (length(ind)>1){
    avg_auc_curve_ann@x.values[[i]]<-avg_auc_curve_ann@x.values[[i]][-ind[-1]]
    avg_auc_curve_ann@y.values[[i]]<-avg_auc_curve_ann@y.values[[i]][-ind[-1]]}
  else{
    avg_auc_curve_ann@x.values[[i]]<-avg_auc_curve_ann@x.values[[i]]
    avg_auc_curve_ann@y.values[[i]]<-avg_auc_curve_ann@y.values[[i]]}}

plot(avg_auc_curve_ann, avg="vertical",main='Average ROC Curve for ANN',col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

### And all the curvers
plot(avg_auc_curve_ann,main='All the ROC Curves for Artifical Neural Network',col=2,lwd=1)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

### And lets compute the average ROC curve
auc_list <- ROCR::performance(pred,"auc")@y.values
aucs <- c()
for(num in auc_list) { aucs <- c(aucs, num)}
auc_mean_ann <- mean(aucs)
auc_min_ann <- min(aucs)
auc_max_ann <- max(aucs)
auc_sd_ann <-sd(aucs)

ann_predictions <- predictions
ann_actuals <- actuals
########################################
clean_data<-data
clean_data<-as.data.frame(clean_data)
clean_data[,24]<-as.factor(clean_data[,24])

colnames(clean_data)<-c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24")
clean_data<-upSample(x=clean_data[,-ncol(clean_data)],y=clean_data$a24)
colnames(clean_data)[24]<-c("a24")
clean_data$a24<-as.numeric(clean_data$a24)

runfold_svm <- function(train, test)
{
  model.svm = svm(a24 ~ a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23, 
                data=train)
  pr <- predict(model.svm, test)
  return(list(pr,test$a24))}

fivefold_svm <- function(data){
  values <- rep(1:5, length.out=nrow(data))
  random <- sample(values)
  data$cv_group <- random
  
  train.one <- data[which(data$cv_group!= 1),]
  test.one <- data[which(data$cv_group== 1),]
  train.two <- data[which(data$cv_group!= 2),]
  test.two <- data[which(data$cv_group== 2),]
  train.three <- data[which(data$cv_group!= 3),]
  test.three <- data[which(data$cv_group== 3),]
  train.four <- data[which(data$cv_group!= 4),]
  test.four <- data[which(data$cv_group== 4),]
  train.five <- data[which(data$cv_group!= 5),]
  test.five <- data[which(data$cv_group== 5),]
  
  res1 <- runfold_svm(train.one, test.one)
  res2 <- runfold_svm(train.two, test.two)
  res3 <- runfold_svm(train.three, test.three)
  res4 <- runfold_svm(train.four, test.four)
  res5 <- runfold_svm(train.five, test.five)
  
  predictions <- list(res1[1][[1]],res2[1][[1]],res3[1][[1]],res4[1][[1]],res5[1][[1]])
  actuals <- list(res1[2][[1]],res2[2][[1]],res3[2][[1]],res4[2][[1]],res5[2][[1]])
  return(list(predictions, actuals))}

predictions <- list()
actuals <- list()

for(i in 1:2){
  
  kfold_result <- fivefold_svm(clean_data)
  predictions <- append(predictions, kfold_result[[1]])
  actuals <- append(actuals, kfold_result[[2]])
  message(i)}

pred <- ROCR::prediction(predictions, actuals)
avg_auc_curve_svm <- ROCR::performance(pred,"tpr","fpr")

for (i in 1:length(avg_auc_curve_svm@x.values)){
  ind<-which(avg_auc_curve_svm@x.values[[i]]==0)
  if (length(ind)>1){
    avg_auc_curve_svm@x.values[[i]]<-avg_auc_curve_svm@x.values[[i]][-ind[-1]]
    avg_auc_curve_svm@y.values[[i]]<-avg_auc_curve_svm@y.values[[i]][-ind[-1]]}
  else{
    avg_auc_curve_svm@x.values[[i]]<-avg_auc_curve_svm@x.values[[i]]
    avg_auc_curve_svm@y.values[[i]]<-avg_auc_curve_svm@y.values[[i]]}}

plot(avg_auc_curve_svm, avg="vertical",main='Average ROC Curve for Support Vector Machine',col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col='gray')
plot(avg_auc_curve_svm,main='All the ROC Curves for Support Vector Machine',col=2,lwd=1)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

auc_list <- ROCR::performance(pred,"auc")@y.values
aucs <- c()
for(num in auc_list) { aucs <- c(aucs, num)}
auc_mean_svm <- mean(aucs)
auc_min_svm <- min(aucs)
auc_max_svm <- max(aucs)
auc_sd_svm <-sd(aucs)

svm_predictions <- predictions
svm_actuals <- actuals
####################################
### Print Average AUC Curves Together
par(cex.axis=1.5)
plot(avg_auc_curve_ann, avg="vertical",main='', xlab="False Positive Rate", ylab="True Positive Rate", lwd = 3, pch = 0, cex.lab=1.5)
plot(avg_auc_curve_rf, avg="vertical",add = TRUE,lwd=2, pch = 8, lty = "dotted")
plot(avg_auc_curve_log_reg, avg="vertical",add = TRUE,lwd=2, pch = 6, lty = "twodash")
plot(avg_auc_curve_svm, avg="vertical",add = TRUE,lwd=2, pch = 6, lty = "dotdash")
abline(a=0,b=1,lwd=2,lty=2,col='gray')
legend(0,1, legend=c("Artificial Neural Network", "Random Forest", "Logistic Regression", "Support Vector Machine"),  lty=c('solid','dotted','twodash','dotdash'), lwd =3, cex=1)

models <- c("Logistic Regression", "Random Forest", "Artificial Neural Network", "Support Vector Machine")
means <- c(auc_mean_lr, auc_mean_rf, auc_mean_ann,auc_mean_svm)
sds <- c(auc_sd_lr,auc_sd_rf, auc_sd_ann,auc_sd_svm)
mins <- c(auc_min_lr,auc_min_rf,auc_min_ann,auc_min_svm)
maxs <- c(auc_max_lr, auc_max_rf, auc_max_ann,auc_max_svm)

auc_table <- data.frame(matrix(nrow = 4, ncol = 0))
auc_table$model <- models
auc_table$auc_mean <- round(means, digits = 2)
auc_table$auc_sd <- round(sds, digits = 2)
auc_table$auc_min <- round(mins, digits = 2)
auc_table$auc_max <- round(maxs, digits = 2)
# print it out
kable(auc_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

## Logisitic Regression
lr_sensitivity_sum <- 0
lr_specificity_sum <- 0 
lr_cutpoint_sum <- 0
lr_accuracy_sum <- 0

for(i in 1:length(lr_actuals)) {
  cp <- cutpointr(x = lr_predictions[[i]], class = lr_actuals[[i]], method = oc_youden_kernel)
  sum_cp <-summary(cp)$cutpointr[[1]]
  lr_sensitivity_sum <- lr_sensitivity_sum + sum_cp$sensitivity
  lr_specificity_sum <- lr_specificity_sum + sum_cp$specificity
  lr_cutpoint_sum <- lr_cutpoint_sum + sum_cp$optimal_cutpoint
  lr_accuracy_sum <- lr_accuracy_sum + sum_cp$acc}

lr_mean_sensitivity <- lr_sensitivity_sum/length(lr_actuals)
lr_mean_specificity <- lr_specificity_sum/length(lr_actuals)
lr_mean_cutpoint <- lr_cutpoint_sum/length(lr_actuals)
lr_mean_accuracy <- lr_accuracy_sum/length(lr_actuals)

tp<-summary(cp)[[7]][[1]]$tp
tn<-summary(cp)[[7]][[1]]$tn
fp<-summary(cp)[[7]][[1]]$fp
fn<-summary(cp)[[7]][[1]]$fn

lr_ppv<-tp/(tp+fp)
lr_npv<-tn/(tn+fn)

## Random Forests
rf_sensitivity_sum <- 0
rf_specificity_sum <- 0 
rf_cutpoint_sum <- 0
rf_accuracy_sum <- 0

for(i in 1:length(rf_actuals)) {
  cp <- cutpointr(x = rf_predictions[[i]], class = rf_actuals[[i]], method = oc_youden_kernel)
  sum_cp <-summary(cp)$cutpointr[[1]]
  rf_sensitivity_sum <- rf_sensitivity_sum + sum_cp$sensitivity
  rf_specificity_sum <- rf_specificity_sum + sum_cp$specificity
  rf_cutpoint_sum <- rf_cutpoint_sum + sum_cp$optimal_cutpoint
  rf_accuracy_sum <- rf_accuracy_sum + sum_cp$acc}

rf_mean_sensitivity <- rf_sensitivity_sum/length(rf_actuals)
rf_mean_specificity <- rf_specificity_sum/length(rf_actuals)
rf_mean_cutpoint <- rf_cutpoint_sum/length(rf_actuals)
rf_mean_accuracy <- rf_accuracy_sum/length(rf_actuals)
tp<-summary(cp)[[7]][[1]]$tp
tn<-summary(cp)[[7]][[1]]$tn
fp<-summary(cp)[[7]][[1]]$fp
fn<-summary(cp)[[7]][[1]]$fn
rf_ppv<-tp/(tp+fp)
rf_npv<-tn/(tn+fn)

## Support Vector Machine
svm_sensitivity_sum <- 0
svm_specificity_sum <- 0 
svm_cutpoint_sum <- 0
svm_accuracy_sum <- 0

for(i in 1:length(svm_actuals)) {
  cp <- cutpointr(x = svm_predictions[[i]], class = svm_actuals[[i]], method = oc_youden_kernel)
  sum_cp <-summary(cp)$cutpointr[[1]]
  svm_sensitivity_sum <- svm_sensitivity_sum + sum_cp$sensitivity
  svm_specificity_sum <- svm_specificity_sum + sum_cp$specificity
  svm_cutpoint_sum <- svm_cutpoint_sum + sum_cp$optimal_cutpoint
  svm_accuracy_sum <- svm_accuracy_sum + sum_cp$acc}

svm_mean_sensitivity <- svm_sensitivity_sum/length(svm_actuals)
svm_mean_specificity <- svm_specificity_sum/length(svm_actuals)
svm_mean_cutpoint <- svm_cutpoint_sum/length(svm_actuals)
svm_mean_accuracy <- svm_accuracy_sum/length(svm_actuals)
tp<-summary(cp)[[7]][[1]]$tp
tn<-summary(cp)[[7]][[1]]$tn
fp<-summary(cp)[[7]][[1]]$fp
fn<-summary(cp)[[7]][[1]]$fn
svm_ppv<-tp/(tp+fp)
svm_npv<-tn/(tn+fn)

### ANN
ann_sensitivity_sum <- 0
ann_specificity_sum <- 0 
ann_cutpoint_sum <- 0
ann_accuracy_sum <- 0

for(i in 1:length(ann_predictions)) {
  ann_predictions[[i]] <- ann_predictions[[i]][,1] }

for(i in 1:length(ann_actuals)) {
  cp <- cutpointr(x = ann_predictions[[i]], class = ann_actuals[[i]], method = oc_youden_kernel)
  sum_cp <-summary(cp)$cutpointr[[1]]
  ann_sensitivity_sum <- ann_sensitivity_sum + sum_cp$sensitivity
  ann_specificity_sum <- ann_specificity_sum + sum_cp$specificity
  ann_cutpoint_sum <- ann_cutpoint_sum + sum_cp$optimal_cutpoint
  ann_accuracy_sum <- ann_accuracy_sum + sum_cp$acc}

ann_mean_sensitivity <- ann_sensitivity_sum/length(ann_actuals)
ann_mean_specificity <- ann_specificity_sum/length(ann_actuals)
ann_mean_cutpoint <- ann_cutpoint_sum/length(ann_actuals)
ann_mean_accuracy <- ann_accuracy_sum/length(ann_actuals)

tp<-summary(cp)[[7]][[1]]$tp
tn<-summary(cp)[[7]][[1]]$tn
fp<-summary(cp)[[7]][[1]]$fp
fn<-summary(cp)[[7]][[1]]$fn
ann_ppv<-tp/(tp+fp)
ann_npv<-tn/(tn+fn)

## create the final table
model <- c("Logistic Regression", "Random Forest", "Artificial Neural Network","Support Vector Machine")
sensitivitys <- c(lr_mean_sensitivity,rf_mean_sensitivity, ann_mean_sensitivity, svm_mean_sensitivity)
specificitys <- c(lr_mean_specificity,rf_mean_specificity, ann_mean_specificity, svm_mean_specificity)
accuracys <- c(lr_mean_accuracy,rf_mean_accuracy, ann_mean_accuracy, svm_mean_accuracy)
aucc <- c(auc_mean_lr, auc_mean_rf, auc_mean_ann,auc_mean_svm)
ppv <- c(lr_ppv, rf_ppv, ann_ppv,svm_ppv)
npv <- c(lr_npv, rf_npv, ann_npv,svm_npv)

table <- data.frame(matrix(nrow = 4, ncol = 0))
table$Model <- models
table$Accuracy <- round(accuracys, digits = 2)
table$Sensitivity <- round(sensitivitys, digits = 2)
table$Specificity <- round(specificitys, digits = 2)
table$AUC <- round(aucc, digits = 2)
table$ppv <- round(ppv, digits = 2)
table$npv <- round(npv, digits = 2)

kable(table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
############################################
#feature elimination
clean_data<-data
clean_data<-as.data.frame(clean_data)
clean_data[,24]<-as.factor(clean_data[,24])

colnames(clean_data)<-c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24")
set.seed(1)
clean_data<-upSample(x=clean_data[,-ncol(clean_data)],y=clean_data$a24)
colnames(clean_data)[24]<-c("a24")

set.seed(1)
control <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5,repeats=2)
subsets<-c(1:23)
rfProfile<-rfe(clean_data[,-ncol(clean_data)],clean_data[,ncol(clean_data)],sizes = subsets, rfeControl = control)
print(rfProfile)
predictors(rfProfile)
rfProfile$fit
head(rfProfile$resample)
plot(rfProfile,type=c("g","o"))
sf<-predictors(rfProfile)
sf_clean_data<-clean_data[,sf]
sf_clean_data<-cbind(sf_clean_data,clean_data[,"a24"])
colnames(sf_clean_data)[length(sf)+1]<-c("a24")
######################################################
# tuning the rf using plot and grid search
set.seed(1)
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=5, repeats=2)
tunegrid <- expand.grid(mtry=c(1:length(sf)), ntree=c(200, 500, 1000))
set.seed(1)
custom <- train(a24~., data=sf_clean_data, method=customRF, metric="Accuracy", tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)
plot(custom, cex=1.2)
#####################################################
set.seed(1)
runfold_rf <- function(train, test)
{
  model.rf = randomForest(a24 ~ a1+a2+a18+a23+a3+a14+a13+a20+a21+a16+a22+a19+a11+a15+a10+a6+a8+a17+a9, 
                          data=train)
  pr <- predict(model.rf, test, type="prob")[,2] #predict(model.rf, test, type="prob")[,2]
  return(list(pr,test$a24))}

fivefold_rf <- function(data){
  
  values <- rep(1:5, length.out=nrow(data))
  random <- sample(values)
  data$cv_group <- random
  
  train.one <- data[which(data$cv_group!= 1),]
  test.one <- data[which(data$cv_group== 1),]
  train.two <- data[which(data$cv_group!= 2),]
  test.two <- data[which(data$cv_group== 2),]
  train.three <- data[which(data$cv_group!= 3),]
  test.three <- data[which(data$cv_group== 3),]
  train.four <- data[which(data$cv_group!= 4),]
  test.four <- data[which(data$cv_group== 4),]
  train.five <- data[which(data$cv_group!= 5),]
  test.five <- data[which(data$cv_group== 5),]
  
  res1 <- runfold_rf(train.one, test.one)
  res2 <- runfold_rf(train.two, test.two)
  res3 <- runfold_rf(train.three, test.three)
  res4 <- runfold_rf(train.four, test.four)
  res5 <- runfold_rf(train.five, test.five)
  
  predictions <- list(res1[1][[1]],res2[1][[1]],res3[1][[1]],res4[1][[1]],res5[1][[1]])
  actuals <- list(res1[2][[1]],res2[2][[1]],res3[2][[1]],res4[2][[1]],res5[2][[1]])
  return(list(predictions, actuals))}

predictions <- list()
actuals <- list()
library(cutpointr)

for(i in 1:1){
  kfold_result <- fivefold_rf(sf_clean_data)
  predictions <- append(predictions, kfold_result[[1]])
  actuals <- append(actuals, kfold_result[[2]])
  message(i)}

pred <- ROCR::prediction(predictions, actuals)
avg_auc_curve_rf <- ROCR::performance(pred,"tpr","fpr")

for (i in 1:length(avg_auc_curve_rf@x.values)){
  ind<-which(avg_auc_curve_rf@x.values[[i]]==0)
  if (length(ind)>1){
    avg_auc_curve_rf@x.values[[i]]<-avg_auc_curve_rf@x.values[[i]][-ind[-1]]
    avg_auc_curve_rf@y.values[[i]]<-avg_auc_curve_rf@y.values[[i]][-ind[-1]]}
  else{
    avg_auc_curve_rf@x.values[[i]]<-avg_auc_curve_rf@x.values[[i]]
    avg_auc_curve_rf@y.values[[i]]<-avg_auc_curve_rf@y.values[[i]]}}

plot(avg_auc_curve_rf, avg="vertical",main='Average ROC Curve for Random Forests',col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

plot(avg_auc_curve_rf,main='All the ROC Curves for Random Forests',col=2,lwd=1)
abline(a=0,b=1,lwd=2,lty=2,col='gray')

auc_list <- ROCR::performance(pred,"auc")@y.values
aucs <- c()
for(num in auc_list) { aucs <- c(aucs, num)}
auc_mean_rf <- mean(aucs)
auc_min_rf <- min(aucs)
auc_max_rf <- max(aucs)
auc_sd_rf <-sd(aucs)

rf_predictions <- predictions
rf_actuals <- actuals

## Random Forests
rf_sensitivity_sum <- 0
rf_specificity_sum <- 0 
rf_cutpoint_sum <- 0
rf_accuracy_sum <- 0

for(i in 1:length(rf_actuals)) {
  cp <- cutpointr(x = rf_predictions[[i]], class = rf_actuals[[i]], method = oc_youden_kernel)
  
  sum_cp <-summary(cp)$cutpointr[[1]]
  rf_sensitivity_sum <- rf_sensitivity_sum + sum_cp$sensitivity
  rf_specificity_sum <- rf_specificity_sum + sum_cp$specificity
  rf_cutpoint_sum <- rf_cutpoint_sum + sum_cp$optimal_cutpoint
  rf_accuracy_sum <- rf_accuracy_sum + sum_cp$acc}

rf_mean_sensitivity <- rf_sensitivity_sum/length(rf_actuals)
rf_mean_specificity <- rf_specificity_sum/length(rf_actuals)
rf_mean_cutpoint <- rf_cutpoint_sum/length(rf_actuals)
rf_mean_accuracy <- rf_accuracy_sum/length(rf_actuals)

## create the final table
model <- "Eye Features" 
sensitivitys <- rf_mean_sensitivity
specificitys <- rf_mean_specificity 
accuracys <- rf_mean_accuracy
aucc <- auc_mean_rf

tp<-summary(cp)[[7]][[1]]$tp
tn<-summary(cp)[[7]][[1]]$tn
fp<-summary(cp)[[7]][[1]]$fp
fn<-summary(cp)[[7]][[1]]$fn
ppv<-tp/(tp+fp)
npv<-tn/(tn+fn)

table <- data.frame(matrix(nrow = 1, ncol = 0))
table$Model <- model
table$Accuracy <- round(accuracys, digits = 2)
table$Sensitivity <- round(sensitivitys, digits = 2)
table$Specificity <- round(specificitys, digits = 2)
table$AUC <- round(aucc, digits = 2)
table$PPV <- round(ppv, digits = 2)
table$NPV <- round(npv, digits = 2)

kable(table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
######################################
y_act<-c(actuals[[1]],actuals[[2]],actuals[[3]],actuals[[4]],actuals[[5]])-1
p_pred<-c(predictions[[1]],predictions[[2]],predictions[[3]],predictions[[4]],predictions[[5]])

#Calibration Curve
val.prob.ci.2(pp,yy,dostats = c("Intercept","Slope"))

f_y<-y_act-1
f_p<-p_pred-1
fm<-abs(f_y)
fm<-as.data.frame(fm)
fm[2]<-abs(f_p)
colnames(fm)[1]<-c("actuals")
colnames(fm)[2]<-c("DO")

setwd("C:\\Decision Curve Analysis")
source("dca.R") 
OP_DO=dca(data=fm, outcome="actuals", predictors="DO", smooth=TRUE)