library(dslabs)

pols<-dslabs::polls_2008

ggplot(pols,aes(day,margin))+geom_point()

# bin smoothers
span <- 7 


fit<-ksmooth(x=pols$day,y = pols$margin,kernel = "box",bandwidth = 7)
pols %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


# kernel
span <- 7
fit <- with(pols, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# Degree = 1 means polynomial of degree 1, i.e. a straight line
## So red line here is fitting lots of small but straight lines
## Blue lines are fitted lots of small curved lines
# Rafa prefers this to set to one. Increase wigglyness using span rather than degree. 
# Also note, degree is 2 by default, so should change this
pols %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))+
  geom_smooth(color="blue", span = 0.15, method = "loess", method.args = list(degree=2))+
  geom_smooth(color="red", linetype="dashed",span = 0.05, method = "loess", method.args = list(degree=1),se=F)+
  geom_smooth(color="blue", linetype="dashed",span = 0.5, method = "loess", method.args = list(degree=1),se=F)



# Question answers for smoothing
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

library(broom)
loess_est<-loess(formula = deaths~as.numeric(date),data = dat,span = 60/nrow(dat),degree = 1)

augment(loess_est,newdata = dat)%>%
  ggplot(aes(x=date))+
  geom_point(aes(y=deaths),colour="blue",alpha=0.5)+
  geom_line(aes(y=.fitted),colour="red",lwd=0.8)

dat%>%
  filter(!is.na(deaths))%>%
  mutate(smooth=fitted(loess_est),day=yday(date))%>%
  ggplot(aes(day,smooth,colour=year))+
  geom_line(lwd=2)


# Q3 
mnist_27<-dslabs::mnist_27
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

train<-mnist_27$train%>%
  as_tibble()%>%
  mutate(y=if_else(y==7,1,0))

est<-loess(formula = y ~ x_2, data = train, degree = 1, span=0.1)

data.frame(fit=(fitted(est)), y=train$y)%>%
  ggplot(aes(fit,y))+
  geom_point()
