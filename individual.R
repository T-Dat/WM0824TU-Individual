library(survival)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(survminer)
library(tidyverse)

#set directory to data path
#setwd("...")

#read global cybersecurity index data
gci <- read.table("GCI.csv", sep=",", fill = TRUE, header = TRUE)

#read phishing data
data1 <- read.table("all_phishing_from_1.csv", sep=",", fill = TRUE, header = TRUE)[ ,c('firsttime', 'lasttime', 'country')]
data2 <- read.table("all_phishing_from_4736848.csv", sep=",", fill = TRUE, header = TRUE)[ ,c('firsttime', 'lasttime', 'country')]
data3 <- read.table("all_phishing_from_6391840.csv", sep=",", fill = TRUE, header = TRUE)[ ,c('firsttime', 'lasttime', 'country')]
data4 <- read.table("all_phishing_from_7843772.csv", sep=",", fill = TRUE, header = TRUE)[ ,c('firsttime', 'lasttime', 'country')]
data5 <- read.table("all_phishing_from_9403968.csv", sep=",", fill = TRUE, header = TRUE)[ ,c('firsttime', 'lasttime', 'country')]
data<-data.frame(data1)
data$firsttime <- as.Date(data$firsttime, "%Y-%m-%d %H")
data$lasttime <- as.Date(data$lasttime, "%Y-%m-%d %H")

#set time interval
maxdate <- as.Date("2017-01-01 01:00:00", "%Y-%m-%d %H")
mindate <- as.Date("2015-01-01 01:00:00", "%Y-%m-%d %H")
defaultdate <- as.Date("1970-01-01 01:00:00", "%Y-%m-%d %H")

#correct dates
data$lasttime[(data$lasttime > maxdate |  data$lasttime==defaultdate)]<-maxdate
data$event <- ifelse(data$lasttime == maxdate,0,1)
data$firsttime[data$firsttime < mindate] <- mindate
data<-data[data$lasttime > data$firsttime,]

#merge data with gci
data <- merge(data, gci, by="country")
data <- data.frame(
  uptime = as.numeric(difftime(data$lasttime, data$firsttime), units="hours"),
  event = data$event,
  GCI = data$GCI
)

#get cox model
res.cox <- coxph(Surv(uptime,event) ~ GCI, data = data)
summary(res.cox)

#check proportionality hazard assumptions
test <- cox.zph(res.cox)
test

#plot survival plot
ggsurvplot(survfit(res.cox), palette = "#2E9FDF", ggtheme = theme_minimal(), data = data)




