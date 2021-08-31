library(tidyverse)
# Test is high sensitivity and high specficity
 # - positive 85% of the time when true positive; p(Test+|sick)=0.85
 # - negative 90% of the time when true negative; p(Test-|healthy)=0.95

# 2% of the population has the disease

# Bayes theorem: p(A|B)= p(B|A)*P(A)/P(B)

# If the test is positive, how likely to be sick?
# p(sick|test+)= p(test+|sick)*p(test+)/p(sick)
p_testp_sick<-0.85
p_testn_health<-0.9
p_sick<-0.02
p_test_pos = p_testp_sick*p_sick + (1-p_testn_health)*(1-p_sick) 
p_testp_sick*p_sick/p_test_pos


# How to estimate this with R
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))

test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), 
                           replace=TRUE, prob=c(0.90,0.10))

test[disease==1] <- sample(c(0,1), size=sum(disease==1), 
                           replace=TRUE, prob=c(0.15, 0.85))
# Probability of testing positive
mean(test==1)

# Probability of having disease if test negative
data.frame(dis=disease,test=test)%>%
  mutate(falseneg=if_else(dis==1&test==0,1,0))%>%
  summarise(fn=mean(falseneg==1))

# Probability of having disease if test is positive
data.frame(dis=disease,test=test)%>%
  mutate(truepos=if_else(dis==1&test==1,1,0))%>%
  filter(test==1)%>%
  summarise(tp=mean(truepos==1))

# Compare the prevalence of disease in people who test positive to the 
# overall prevalence of disease.

# If a patient's test is positive, by how many times does that increase their 
#risk of having the disease?
# First calculate the probability of having the disease given a positive test:
pr_dis_given_posTest<- data.frame(dis=disease,test=test)%>%
  mutate(truepos=if_else(dis==1&test==1,1,0))%>%
  filter(test==1)%>%
  summarise(tp=mean(truepos==1))%>%
# Then divide by the probability of having the disease.
pr_dis <- mean(disease==1)

pr_dis_given_posTest$tp/pr_dis


library(dslabs)
data("heights")

heights%>%
  mutate(height=round(height))%>%
  group_by(height)%>%
  summarise(p=mean(sex=="Male"))%>%
  qplot(height, p, data =.)


heights%>%
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%  
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


# Generate data from a bivariate normal distrubution using the MASS package
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)

ps <- seq(0, 1, 0.1)
dat%>%
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%  
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(y, x, data =.)

