library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(data.table)
library(scales)
require(forecast)
library(coefplot)
library(effects)
library(Hmisc)
library(glmnet)
library(boot)

data.orig <- read.csv('~/projects/udacity_DataR/P00000001-OH.csv', sep=',',row.names=NULL)
head(data.orig)
colnames(data.orig)[1:18] <- colnames(data.orig)[2:19]
data.orig <- data.orig[1:18]

data <- read.csv('~/projects/udacity_DataR/ohio_contb.csv', sep=',')
election_day <- as.Date("2012-11-06")
data$electdelta <- sapply(data$contb_receipt_dt,function(x) {return(election_day - as.Date(x))})


gdata <-group_by(na.omit(data),cand_party)
gsdata<-summarise(gdata, 
                  mean_contb = mean(contb_receipt_amt),
                  median_contb = median(contb_receipt_amt),
                  mean_pop = mean(population),
                  median_pop = median(population),
                  count = n()) 
ggplot(aes(x=cand_party,y=mean_contb), data=gsdata) + geom_bar(stat='identity')
ggplot(aes(x=cand_party,y=median_contb), data=gsdata) + geom_bar(stat='identity')
ggplot(aes(x=contb_receipt_amt), data=data) + geom_density(aes(fill=cand_party)) + xlim(0,5000)
# DEMS GIVE SMALLER AMOUNTS AND LIBERTARIANS GIVE LARGE AMOUNTS
ggplot(aes(x=cand_party,y=count), data=gsdata) + geom_bar(stat='identity')
# DEMS HAVE MORE DONORS THAT GIVE SMALLER AMOUNTS
ggplot(aes(x=cand_party,y=mean_pop), data=gsdata) + geom_bar(stat='identity')
ggplot(aes(x=cand_party,y=median_pop), data=gsdata) + geom_bar(stat='identity')
# GREEN PARTY DONORS ARE 'MORE LIKELY' TO LIVE IN CITIES W/ LARGER POPS
ggplot(aes(x=population), data=data) + geom_density(aes(fill=cand_party, alpha=0.3))
# USE MEDIAN AS POPULATION IS NOT NORMALLY DISTRIBUTED
ggplot(aes(y=contb_receipt_amt,x=1), data=data) + geom_boxplot(aes(fill=cand_party)) + ylim(0,5000)
ggplot(aes(y=contb_receipt_amt,x=cand_party), data=data) + geom_violin(aes(fill=cand_party)) + ylim(0,5000)
ggplot(aes(y=as.numeric(population),x=1), data=data) + geom_boxplot(aes(fill=cand_party)) 

# helper function to make frequent grouping of data with diffrent vars easier
groupData <- function(initial_data,...){
gp_data <-group_by(na.omit(initial_data),...)
gps_data<-summarise(gp_data, 
                  mean_contb = mean(contb_receipt_amt),
                  median_contb = median(contb_receipt_amt),
                  sum_contb = sum(contb_receipt_amt),
                  mean_pop = mean(population),
                  median_pop = median(population),
                  median_elect_delta = median(elect_delta),
                  mean_elect_delta = mean(elect_delta)
                  count = n()) 
gps_data
}
by_date_party <- groupData(data,contb_receipt_dt, cand_party)
ggplot(aes(x=as.Date(contb_receipt_dt), y=median_contb,color=cand_party), data=by_date_party) + geom_line() 

by_receipt <- groupData(data, receipt, cand_party)
ggplot(aes(x=cand_party, y=count, fill=as.character(receipt)),data=by_receipt) + geom_bar(stat='identity',position=position_dodge())
qplot(x=cand_party, y=median_contb, fill=as.character(receipt), data=by_receipt) + geom_bar(stat='identity',position="dodge")
# R are the only ones with receipts
# People with receipts give large donations

memo <- groupData(data, memo_cd_,cand_party)
ggplot(aes(x=cand_party, y=count, fill=as.character(memo_cd_)),data=memo) + geom_bar(stat='identity',position=position_dodge())
# Doesn't tell us too much

memo_text <- groupData(data, memo_text_,cand_party)
ggplot(aes(x=cand_party, y=count, fill=as.character(memo_text_)),data=memo_text) + geom_bar(stat='identity',position=position_dodge())
# Doesn't tell us too much




tmp <- data
tmp$contb_receipt_dt<-floor_date(as.Date(tmp$contb_receipt_dt), "month")
cand_dt <- groupData(tmp,cand_nm,contb_receipt_dt)
ggplot(aes(x=as.Date(contb_receipt_dt,"%"), y=sum_contb,color=cand_nm), data=cand_dt) + geom_line()
  
data.t <- data.table(na.omit(data))   
cand <- groupData(data,cand_nm,contb_receipt_dt)
ggplot(aes(x=cand_nm,y=median_contb,fill=cand_nm), data=cand) + geom_bar(stat='identity')

g <-group_by(na.omit(data),cand_party, contb_receipt_dt)
gs<-summarise(g, 
                  roll_sum = cumsum(contb_receipt_amt)) 

dems <- data.frame(sample_n(filter(data, cand_party=='D'),1000))
reps <-  data.frame(sample_n(filter(data, cand_party=='R'),100))
green <-  data.frame(sample_n(filter(data, cand_party=='G'),100))
lib <- data.frame(sample_n(filter(data, cand_party=='L'),100))

d <- data.frame(filter(data, cand_party=='D'))
d.s <- d$contb_receipt_amt
# TIME SERIES
data.s <- ts(d.s, start = min(as.Date(d$contb_receipt_dt)), end = max(as.Date(d$contb_receipt_dt)))

# ML
set.seed(1)
data$gp <- runif(dim(data)[1])
data$party <- data$gp
subset(data$party=='R') 

# I know for-loops are frowned upon in R, but I was sitting in an airport with no wi-fi and didn't feel like 
i = 1
for (p in data$cand_party){
  if (p == 'R'){
    data$party[i] <-0
  }
  if (p=='G'){
    data$party[i] <-100
  }
  if(p=='L'){
    data$party[i] <-100
  }
  if (p=='D'){
    data$party[i] <-1
  }
  i = i + 1
}
logit.data <- subset(na.omit(data), party!=100)
logit.data.train <- subset(logit.data, gp < .9)
logit.data.test <- subset(logit.data, gp >= .9)
regularized.fit <- glmnet(x=as.matrix(logit.data.train[,c(8,18,19)]),y=as.matrix(logit.data.train[,c(21)]), family='binomial')
regularized.results <- ifelse(predict(regularized.fit, newx=as.matrix(logit.data.test[,c(8,18,19)]), s= 0.001) > 0,1,0)
regularized.probs <- inv.logit(predict(regularized.fit, newx=as.matrix(logit.data[,c(8,18,19)]), s= 0.001))
error.rate <- mean(regularized.results != logit.data$party)
error.count <- regularized.results == logit.data.test$party
logit.data.test$res <- regularized.results

test <- data.frame(logit.data.test)

plot(x=test$contb_receipt_amt, y=test$res) 
+ geom_point()
+ stat_smooth(method='glm', family='binomial', se=FALSE,y=test$res)
