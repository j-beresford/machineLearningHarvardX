######### Part 2 ##########
library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times = 1,p = 0.5,list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Using only the train iris dataset, for each feature, perform a simple search 
# to find the cutoff that produces the highest accuracy, predicting virginica
# if greater than the cutoff and versicolor otherwise. Use the seq function 
# over the range of each feature by intervals of 0.1 for this search.

# Which feature produces the highest accuracy?

cutoffs<-seq(0,10,0.1)

sep_length<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(train$Sepal.Length>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==train$Species)
})
sep_width<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(train$Sepal.Width>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==train$Species)
})
pet_length<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(train$Petal.Length>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==train$Species)
})
pet_width<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(train$Petal.Width>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==train$Species)
})

data.frame(c=cutoffs,sl=sep_length,sw=sep_width,pl=pet_length,pw=pet_width)%>%
  gather(key=var,val=accuracy,-c)%>%
  ggplot(aes(c,accuracy,colour=var))+
  geom_line()

data.frame(c=cutoffs,sl=sep_length,sw=sep_width,pl=pet_length,pw=pet_width)%>%
  gather(key=var,val=accuracy,-c)%>%
  filter(accuracy==max(accuracy))


y_hat_best<-if_else(train$Petal.Length>5,"virginica","versicolor")%>%
  factor(levels=levels(test$Species))
mean(y_hat_best==train$Species)




plot(iris,pch=21,bg=iris$Species)
# Which feature best optimizes our overall accuracy when using the test set?
cutoffs<-seq(0,10,0.1)

sep_length<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(test$Sepal.Length>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==test$Species)
})
sep_width<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(test$Sepal.Width>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==test$Species)
})
pet_length<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(test$Petal.Length>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==test$Species)
})
pet_width<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(test$Petal.Width>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==test$Species)
})
data.frame(c=cutoffs,sl=sep_length,sw=sep_width,pl=pet_length,pw=pet_width)%>%
  gather(key=var,val=accuracy,-c)%>%
  ggplot(aes(c,accuracy,colour=var))+
  geom_line()



#Notice that Petal.Length and Petal.Width in combination could potentially 
#be more information than either feature alone.
plot(iris,pch=21,bg=iris$Species)

#Optimize the the cutoffs for Petal.Length and Petal.Width separately in the 
#train dataset by using the seq function with increments of 0.1. 
pet_length<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(train$Petal.Length>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==train$Species)
})
pet_width<-map_dbl(cutoffs, function(x){
  y_hat<-if_else(train$Petal.Width>x,"virginica","versicolor")%>%
    factor(levels=levels(test$Species))
  mean(y_hat==train$Species)
})
# More than one max values for each, len is best at 4.7-.8, width is 1.5-7
data.frame(c=cutoffs,len=pet_length,wid=pet_width)%>%
  filter(len==max(len)|wid==max(wid))
#Then, report the overall accuracy when applied to the test dataset by 
#creating a rule that predicts virginica if Petal.Length is greater than the
#length cutoff OR Petal.Width is greater than the width cutoff, and
#versicolor otherwise.
y_hat_double<-if_else(test$Petal.Length>4.7|test$Petal.Width>1.5,
                     "virginica","versicolor")%>%
                      factor(levels=levels(test$Species))

mean(y_hat_double==test$Species)
#What is the overall accuracy for the test data now?


