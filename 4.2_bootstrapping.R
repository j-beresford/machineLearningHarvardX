library(dslabs)
library(caret)
library(purrr)

data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

mean(mnist_27$index_train[indexes$Resample01]==3)


mnist_27$train

bind_rows(indexes)%>%
  gather(key=k,value=v)%>%
  summarise(sum(v==3))


# Q3

quants<-replicate(10000,{
  y <- rnorm(100, 0, 1)
  quantile(y,0.75)
})

sd(quants)

 # Q4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later


indexes<-createResample(y,10000)

quantile(y[indexes$Resample01],0.75)
quantile(y[indexes$Resample02],0.75)
quantile(y[indexes$Resample03],0.75)
quantile(y[indexes$Resample04],0.75)

x=map_df(indexes, function(ind){
  quantile(y[ind],0.75)
})

sd(x$`75%`)
