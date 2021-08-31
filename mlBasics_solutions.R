library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)

data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#The type column of dat indicates whether students took classes in person ("inclass") or online ("online"). 
#What proportion of the inclass group is female? 
# What proportion of the online group is female?

dat%>%
  group_by(type)%>%
  summarise(mean(sex=="Female"))

# In the course videos, height cutoffs were used to predict sex. 
# Instead of height, use the type variable to predict sex. 
# Assume that for each class type the students are either all male or 
# all female, based on the most prevalent sex in each class type you 
# calculated in Q1. Report the accuracy of your prediction of sex based on type.
# You do not need to split the data into training and test sets.

y_hat <- if_else(x=="inclass","Female","Male")
mean(y==y_hat)

#Write a line of code using the table() function to show the confusion matrix between y_hat and y
table(y, y_hat)

# What is the sensitivity of this prediction?
y_hat<-y_hat%>%factor(levels=levels(y))
sensitivity(y_hat,y)
# What is the specificity of this prediction?
specificity(y_hat,y)

