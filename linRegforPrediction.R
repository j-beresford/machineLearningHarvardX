##### Section 3: Linear regression for prediction #######

library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)%>%
  as_tibble()

library(caret)

y<-galton_heights$son
x<-galton_heights$father

split_index<-createDataPartition(y,times=1,p=0.5,list=FALSE)

train_set<-galton_heights[split_index,]
test_set<-galton_heights[-split_index,]

# First thing might be to guess the average for everyone
avg<-mean(train_set$son)
mean((avg-test_set$son)^2) # Squared loss

# Or just regress father on son hights
ols<-lm(son~father,data=train_set)

fitted<-ols$coefficients[1] + test_set$father*ols$coefficients[2]
mean((fitted-test_set$son)^2)

