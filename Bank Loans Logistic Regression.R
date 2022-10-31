
setwd('C:/Users/Nithin/Downloads/Logistic Reg')

#**************************************************************************************************************#

#########################
#-->Required Packages<--#
#########################
require(dplyr)
require(stringr)
require(fastDummies)
require(ggplot2)
require(caret)
require(car)
require(Metrics)
require(InformationValue)
require(pROC)
require(lubridate)

#**************************************************************************************************************#

################
#-->Datasets<--#
################

Loans <- read.csv('BankLoans.csv')

#**************************************************************************************************************#

#################
#-->Data Prep<--#
#################

str(Loans)

#**************************************************************************************************************#

########################
#--> Data Splitting <--#
########################

samp <- sample(1:nrow(Loans), floor(nrow(Loans)*0.7))

dev <-Loans[samp,]
val <-Loans[-samp,]

#*#**************************************************************************************************************#

########################
#--> Model Building <--#
########################

M0 <- glm(default~employ+
           address+
           debtinc+
           creddebt,data = dev,
           family = binomial(logit))

summary(M0)

#--> Columns Removed <--#
#income
#age
#ed
#othdebt

#*#*#**************************************************************************************************************#

#*#####################
#--> Model Scoring <--#
#######################

dev1 <- cbind(dev, prob = predict(M0, type = "response"))

prob <- predict(M0, val, type = 'response')
test_Y <- val$default
val1 <- cbind(test_Y,prob)
colnames(val1) = c("default","prob")
val1 <- data.frame(val1)

#--> Concordance <--#
Concordance(dev1$default,dev1$prob)
Concordance(val1$default,val1$prob)

#--> AUC <--#
roc_obj <- roc(dev1$default,dev1$prob)
auc(roc_obj)

roc_obj <- roc(val1$default,val1$prob)
auc(roc_obj)

#--> Predicted Class <--#
dev1$pred = ifelse(dev1$prob > 0.5 , 1, 0)
val1$pred = ifelse(val1$prob > 0.5 , 1, 0)

#--> Confusion Matrix <--#
confusionMatrix(dev1$pred,dev1$default)
confusionMatrix(val1$pred,val1$default)

#--> Accuracy <--#
accuracy(dev1$default,dev1$pred)
accuracy(val1$default,val1$pred)

#--> Sensitivity <--#
sensitivity(dev1$default,dev1$pred)
sensitivity(val1$default,val1$pred)

#--> Specificity <--#
specificity(dev1$default,dev1$pred)
specificity(val1$default,val1$pred)

#*#*#**************************************************************************************************************#

#*######################
#--> Best Threshold <--#
########################

roc_obj <-  roc(dev1$default,dev1$prob)
plot(roc_obj)
roc_values <- coords(roc_obj, "best", "threshold")
roc_values

#Best Value is 0.24

#*#*#**************************************************************************************************************#

#*#####################
#--> KS Statistics <--#
#######################

y_prob <- as.data.frame(cbind(dev1$default,dev1$prob))
colnames(y_prob) <- c('default','prob')

decLocations <- quantile(y_prob$prob, probs = seq(0.1,0.9,by=0.1))
y_prob$decile <- findInterval(y_prob$prob,c(-Inf,decLocations, Inf))

decile_grp <- group_by(y_prob,decile)

decile_summ <- summarize(decile_grp, total_cnt=n(), 
                         min_prob=min(p=prob), 
                         max_prob=max(prob), 
                         default_cnt=sum(default),
                         non_default_cnt=total_cnt-default_cnt, 
                         default_rate=(default_cnt/total_cnt)*100)

decile_summ<-arrange(decile_summ, desc(decile))

sum1 <- sum(decile_summ$default_cnt)
sum2 <- sum(decile_summ$non_default_cnt)

decile_summ$default_pct <- ((decile_summ$default_cnt)/sum1)*100
decile_summ$non_default_pct <- ((decile_summ$non_default_cnt)/sum2)*100
decile_summ$cum_default_pct <- cumsum(decile_summ$default_pct)
decile_summ$cum_non_default_pct <- cumsum(decile_summ$non_default_pct)
decile_summ$ks_stats <- abs(decile_summ$cum_default_pct-decile_summ$cum_non_default_pct)

View(decile_summ)

#*#*#**************************************************************************************************************#

#*################################
#--> Using the best Threshold <--#
##################################

dev1$pred <- ifelse(dev1$prob > 0.24,1,0)
val1$pred <- ifelse(val1$prob > 0.24,1,0)

#--> Concordance <--#
Concordance(dev1$default,dev1$prob)
Concordance(val1$default,val1$prob)

#--> AUC <--#
roc_obj <- roc(dev1$default,dev1$prob)
auc(roc_obj)

roc_obj <- roc(val1$default,val1$prob)
auc(roc_obj)

#--> Predicted Class <--#
dev1$pred = ifelse(dev1$prob > 0.5 , 1, 0)
val1$pred = ifelse(val1$prob > 0.5 , 1, 0)

#--> Confusion Matrix <--#
confusionMatrix(dev1$pred,dev1$default)
confusionMatrix(val1$pred,val1$default)

#--> Accuracy <--#
accuracy(dev1$default,dev1$pred)
accuracy(val1$default,val1$pred)

#--> Sensitivity <--#
sensitivity(dev1$default,dev1$pred)
sensitivity(val1$default,val1$pred)

#--> Specificity <--#
specificity(dev1$default,dev1$pred)
specificity(val1$default,val1$pred)

#*#*#**************************************************************************************************************#

#*###################################
#--> KS Statistics for test data <--#
#####################################

y_prob <- as.data.frame(cbind(val1$default,val1$prob))
colnames(y_prob) <- c('default','prob')

decLocations <- quantile(y_prob$prob, probs = seq(0.1,0.9,by=0.1))
y_prob$decile <- findInterval(y_prob$prob,c(-Inf,decLocations, Inf))

decile_grp <- group_by(y_prob,decile)

decile_summ <- summarize(decile_grp, total_cnt=n(), 
                         min_prob=min(p=prob), 
                         max_prob=max(prob), 
                         default_cnt=sum(default),
                         non_default_cnt=total_cnt-default_cnt, 
                         default_rate=(default_cnt/total_cnt)*100)

decile_summ<-arrange(decile_summ, desc(decile))

sum1 <- sum(decile_summ$default_cnt)
sum2 <- sum(decile_summ$non_default_cnt)

decile_summ$default_pct <- ((decile_summ$default_cnt)/sum1)*100
decile_summ$non_default_pct <- ((decile_summ$non_default_cnt)/sum2)*100
decile_summ$cum_default_pct <- cumsum(decile_summ$default_pct)
decile_summ$cum_non_default_pct <- cumsum(decile_summ$non_default_pct)
decile_summ$ks_stats <- abs(decile_summ$cum_default_pct-decile_summ$cum_non_default_pct)

View(decile_summ)

#*#*#**************************************************************************************************************#