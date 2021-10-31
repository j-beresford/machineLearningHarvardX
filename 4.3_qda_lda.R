library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]


df<-data.frame(x,y)


tr<-train(y~.,method="lda",data=df,preProcess="center")

tr$finalModel$means%>%
  as_tibble(rownames = "thing")%>%
  gather(key=key,value = val,-thing)%>%
  arrange(val)



# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]


df<-data.frame(x,y)
tr<-train(y~.,method="lda",data=df,preProcess="center")
tr
