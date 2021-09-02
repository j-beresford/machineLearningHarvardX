library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later


n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# We will build 100 linear models using the data above 

# First, set the seed to 1 again 
set.seed(1, sample.kind="Rounding")

# Then, within a replicate() loop, 
  # (1) partition the dataset into test and training sets with p = 0.5 and 
  # using dat$y to generate your indices, 
  # (2) train a linear model predicting y from x, 
  # (3) generate predictions on the test set, and 
  # (4) calculate the RMSE of that model. Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.

set.seed(1, sample.kind="Rounding")
mean(replicate(n = 100,expr = 
          {split_index<-createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
          train_set<-dat[-split_index,]
          test_set<-dat[split_index,]
          ols<-lm(y~x,data=train_set)
          fitted<-ols$coefficients[1]+ols$coefficients[2]*test_set$x
          mean((fitted-test_set$y)^2)^0.5}))

set.seed(1, sample.kind="Rounding")
sd(replicate(n = 100,expr = 
         {split_index<-createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
         train_set<-dat[-split_index,]
         test_set<-dat[split_index,]
         ols<-lm(y~x,data=train_set)
         fitted<-ols$coefficients[1]+ols$coefficients[2]*test_set$x
         mean((fitted-test_set$y)^2)^0.5}))

# Now we will repeat the exercise above but using larger datasets.
# Write a function that takes a size n, then 
  # (1) builds a dataset using the code provided at the top of Q1 but with n
  # observations instead of 100 and without the set.seed(1)
rm(list=ls())
n <- c(100, 500, 1000, 5000, 10000)

part_two<-function(n){
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat_maker<-function(n){MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))}
dat<-dat_maker(n)

  # (2) runs the replicate() loop that you wrote to answer Q1, which builds 100 linear models and returns a vector of RMSEs
rmses<-replicate(n = 100,expr = 
               {split_index<-createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
               train_set<-dat[-split_index,]
               test_set<-dat[split_index,]
               ols<-lm(y~x,data=train_set)
               fitted<-ols$coefficients[1]+ols$coefficients[2]*test_set$x
               mean((fitted-test_set$y)^2)^0.5})
  # (3) calculates the mean and standard deviation of the 100 RMSEs.
sd(rmses)
}

# Set the seed to 1 and then use sapply() or map() to apply your new 
# function to n <- c(100, 500, 1000, 5000, 10000)
set.seed(1, sample.kind="Rounding")
# First mean is 2.488661
map(n,part_two)

# q4: Now repeat the exercise from Q1, this time making the correlation 
# between x and y larger, as in the following code:

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
mean(replicate(n = 100,expr = 
               {split_index<-createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
               train_set<-dat[-split_index,]
               test_set<-dat[split_index,]
               ols<-lm(y~x,data=train_set)
               fitted<-ols$coefficients[1]+ols$coefficients[2]*test_set$x
               mean((fitted-test_set$y)^2)^0.5}))


set.seed(1, sample.kind="Rounding")
sd(replicate(n = 100,expr = 
                 {split_index<-createDataPartition(dat$y,times = 1,p = 0.5,list = FALSE)
                 train_set<-dat[-split_index,]
                 test_set<-dat[split_index,]
                 ols<-lm(y~x,data=train_set)
                 fitted<-ols$coefficients[1]+ols$coefficients[2]*test_set$x
                 mean((fitted-test_set$y)^2)^0.5}))

#Q6
set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# Note that y is correlated with both x_1 and x_2 but the two predictors are
# independent of each other, as seen by cor(dat).
cor(dat)
# Use caret to partition into test and training sets with p = 0.5
set.seed(1, sample.kind="Rounding")
split_index<-createDataPartition(y = dat$y,times = 1,p = 0.5,list = FALSE)
train_set<-dat[-split_index,]
test_set<-dat[split_index,]

# Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. 
# Train a single linear model for each 
ols_x1<-lm(y~x_1,data=train_set)
ols_x2<-lm(y~x_2,data=train_set)
ols_x12<-lm(y~x_1+x_2,data=train_set)

fitted_x1<-ols_x1$coefficients[1]+ols_x1$coefficients[2]*test_set$x_1
fitted_x2<-ols_x2$coefficients[1]+ols_x2$coefficients[2]*test_set$x_2
fitted_x12<-ols_x12$coefficients[1]+ols_x12$coefficients[2]*test_set$x_1 +
  ols_x12$coefficients[3]*test_set$x_2

mean((fitted_x1-test_set$y)^2)^0.5
mean((fitted_x2-test_set$y)^2)^0.5
mean((fitted_x12-test_set$y)^2)^0.5

#Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated.
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

set.seed(1, sample.kind="Rounding")
split_index<-createDataPartition(y = dat$y,times = 1,p = 0.5,list = FALSE)
train_set<-dat[-split_index,]
test_set<-dat[split_index,]

# Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. 
# Train a single linear model for each 
ols_x1<-lm(y~x_1,data=train_set)
ols_x2<-lm(y~x_2,data=train_set)
ols_x12<-lm(y~x_1+x_2,data=train_set)

fitted_x1<-ols_x1$coefficients[1]+ols_x1$coefficients[2]*test_set$x_1
fitted_x2<-ols_x2$coefficients[1]+ols_x2$coefficients[2]*test_set$x_2
fitted_x12<-ols_x12$coefficients[1]+ols_x12$coefficients[2]*test_set$x_1 +
  ols_x12$coefficients[3]*test_set$x_2

mean((fitted_x1-test_set$y)^2)^0.5
mean((fitted_x2-test_set$y)^2)^0.5
mean((fitted_x12-test_set$y)^2)^0.5
