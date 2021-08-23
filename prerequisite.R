# Data Science: Machine Learning
# HarvardX Question Solutions 
# Justin Beresford


# Prerequisite knowledge required

library(dslabs)
library(tidyverse)
data(heights)
heights

# Q1: Object Classes
# Match each object to its corresponding class.

#heights dataset
class(heights)
# sex column
class(heights$sex)
# height column
class(heights$height)
# "Male"
class("Male")
# 75.00000
class(75.00000)

# Q2: Object Dimensions
# How many rows are in this dataset?
nrow(heights)

# Q3: Indexing - 1
# What is the height in row 777?
heights[777,"height"]

# Q4: Indexing - 2
# Which of these pieces of code returns the sex in row 777?
heights[777,1]

# Q5: Maximum and Minimum
# What is the maximum height in inches?
heights%>%
  summarise(max=max(height))

# Which row has the minimum height?
heights%>%
  mutate(row=1:nrow(heights))%>%
  filter(height==min(height))

## Q6: Summary Statistics
# What is the mean height in inches?
heights%>%
  summarise(mean=mean(height))
# What is the median height in inches?
heights%>%
  summarise(median=median(height))

# Q7: Conditional Statements- 1
# What proportion of individuals in the dataset are male?
heights%>%
  group_by(sex)%>%
  tally()%>%
  spread(key=sex,value=n)%>%
  mutate(proportion=Male/(Female+Male))

# Q8: Conditional Statements - 2
# How many individuals are taller than 78 inches (roughly 2 meters)?  
heights%>%
  filter(height>78)%>%
  tally()

# Q9: Conditional Statements - 3
# How many females in the dataset are taller than 78 inches?
heights%>%
  filter(height>78 & sex=="Female")%>%
  tally()
