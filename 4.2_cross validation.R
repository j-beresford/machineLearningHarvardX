library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

train(x=x_subset,y = y,method = "glm")


install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals<-tt$p.value

x_newsubset<-x[,ifelse(pvals<=0.01,1,0)==1]

train(x=x_newsubset,y = y,method = "glm")

fit<-train(x = x_newsubset,y=y,method = "knn",tuneGrid = data.frame(k = seq(101, 301, 25)))
plot(fit)


tissue<-dslabs::tissue_gene_expression

fit<-train(x = tissue$x,y=tissue$y,method = "knn",tuneGrid = data.frame(k = seq(1, 7, 2)))
fit

tissue$y



