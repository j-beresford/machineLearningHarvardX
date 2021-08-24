# Data Science: Machine Learning
# HarvardX Question Solutions 
# Justin Beresford

# Section 2: Machine Learning Basics
# 2.1: Basics of Evaluating Machine Learning Algorithms

###############################################################
# Caret package, training and test sets, and overall accuracy #
###############################################################

# Spliting and assigning a dataframe into test and train using caret
library(caret)
library(dslabs)
library(tidyverse)

data(heights)

y <- heights$sex
x <- heights$height

set.seed(2)

test_index <- createDataPartition(y,times = 1,p = 0.5,list = FALSE)

train_set<-heights[-test_index,]
test_set<-heights[test_index,]

# We know develop an agolrithm that predicts sex. The easiest way to test
# accuracy is to check the proportion of correctly predicted outcomes. 
# This is called 'overall accuracy'

# Randomly assign male and female, 
# but define outcome as facet with same levels as test set
y_hat_guess<-sample(c("Male","Female"),nrow(test_index),replace = TRUE)%>%
                factor(levels = levels(test_set$sex))

# y_hat_guess is the same as test_set about 50% of the time
mean(y_hat_guess==test_set$sex)*100

# EDA tells us that males are generally a bit taller, 
# so we should be able to do better than guess
heights%>%ggplot(aes(height,fill=sex))+geom_density(alpha=0.5)

# Alternative, predict male if height is within 2SDs from mean male height
heights%>%
  group_by(sex)%>%
  summarise(mean=mean(height),sd=sd(height))%>%
  mutate(two_sd_belwo=mean-sd)

# Try guessing male if height is over 62
y_hat_sd<-if_else(x>62,"Male","Female")%>%
  factor(levels = levels(test_set$sex))
mean(y==y_hat_sd)*100 # Accuracy rises to 80%

# Now define experiment that cycles through various cutoffs
cutoff<-seq(61,70,0.25)
accuracy<-map_dbl(cutoff,function(x){
  y_hat<-if_else(train_set$height>x,"Male","Female")%>%
    factor(levels = levels(test_set$sex))
  mean(y_hat==train_set$sex)  
})
# Accuracy maximised with a cut off of about 80
cutoff_results<-data.frame(cuts=cutoff,accs=accuracy)
cutoff_results%>%
  ggplot(aes(cuts,accs*100))+geom_point()

best_cutoff<-cutoff_results%>%
  filter(accuracy==max(accuracy))%>%
  select(cuts)%>%
  as.double()

# Test best cutoff against test set
y_hat_bestcut<-if_else(test_set$height>best_cutoff,"Male","Female")%>%
  factor(levels = levels(test_set$sex))
mean(test_set$sex==y_hat_bestcut)*100 # Accuracy rises to 83%


###############################################################
################# The Confusion Matrix#########################
###############################################################

# The heights dataset is mostly men. You could predict 100% men, and
# get an overall sensitivity mark of about 100%. Import to check results
# are accurate for both genders. 

###################### Sensitivity #############################
# Ability to predict positive when actual outcome is positive
# If you call everything 1 (pos), you'll get perfect sensitivity!

###################### Specificity #############################
# Ability to not predict positive (yhat=0) when actual outcome is not positive

# If you predict 1, can get false or true positive; 
# Vice versa, predict 0 gives false neg or false pos

# For example, here mainly men and mainly guessed men. Sens low, spec high...
confusionMatrix(data = y_hat_bestcut,reference = test_set$sex)
# NB Balanced Accuracy is the average of sensitivity and specificity
# This code maximises F1 score (bal avg) instead of accuracy

cutoffs<-seq(61,70)
f_scores<-map_dbl(cutoffs, function(x){
  y_hat_f<-if_else(train_set$height>x,"Male","Female")%>%
    factor(levels=levels(test_set$sex))
  F_meas(data = y_hat_f,reference = (factor(train_set$sex)))
})
data.frame(cut=cutoffs,acc=f_scores)%>%
  ggplot(aes(cut,acc))+geom_point() # Maximised at 66

y_hat_f<-if_else(train_set$height>66,"Male","Female")%>%
  factor(levels=levels(test_set$sex))

confusionMatrix(data = y_hat_f,reference = test_set$sex)
