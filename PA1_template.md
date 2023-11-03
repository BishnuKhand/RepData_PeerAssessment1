---
title: "Reproducible Research: Peer Assessment 1"
Author: Bishnu Khand
Date: 2nd Nov 2023
output: 
  html_document:
    keep_md: true
---
##library(knitr)

##library(markdown)

##("PA1_template.Rmd", "mPA1_template.md)

##markdownToHTML("PA1_template.md", PA1_template.html") PNG FLE IS MISSING"



## Loading and preprocessing the data
setwd("C:/Users/khand/Documents/GitHub2/RepData_PeerAssessment1/activity")
activity<-read.csv("activity.csv")
df <- read.csv("activity.csv", header=T, sep=",")


## What is mean total number of steps taken per day?
df.stepsDay <- aggregate(df$steps, by=list(df$date), FUN=sum, na.rm=F)
df.stepsDayMean <- mean(df.stepsDay$x, na.rm=T)
df.stepsDayMedian <- median(df.stepsDay$x, na.rm=T)

hist(df.stepsDay$x, 
     breaks=20,
     main="Total number of steps take per day", 
     col="red", 
     xlab="Steps taken")


## What is the average daily activity pattern?

df.stepsInterval <- aggregate(df$steps, by=list(df$interval), FUN=mean, na.rm=T)

plot( x=df.stepsInterval[,1], 
      y=df.stepsInterval[,2], 
      type="l",
      col="red",
      main="Average steps taken per Interval",
      ylab="Steps taken", 
      xlab="Interval")


## Imputing missing values
nas   <- sum(is.na(df))
compl <- sum(complete.cases(df))

df.clean <- cbind(df, df.stepsInterval[,2])
names(df.clean)[4] <- c("mean")

df.clean$steps <- ifelse( is.na(df.clean$steps), df.clean$mean, df.clean$steps)


hist(df.clean.stepsDayx, 
     breaks=20,
     main="Total number of steps take per day", 
     col="red", 
     xlab="Steps taken")



## Are there differences in activity patterns between weekdays and weekends?
df.clean$date <- strptime(df.clean$date, "%Y-%m-%d")
df.clean$weekend <- (weekdays(df.clean$date) %in% c("Sunday", "Saturday"))

df.weekend <- df.clean[df.clean$weekend == TRUE,]
df.weekday <- df.clean[df.clean$weekend == FALSE,]

df.weekend.steps <- aggregate(df.weekend$steps, by=list(df.weekend$interval), FUN=mean)
df.weekday.steps <- aggregate(df.weekday$steps, by=list(df.weekday$interval), FUN=mean)

par(mfrow=c(2,1))

plot( x=df.weekend.steps[,1], 
      y=df.weekend.steps[,2], 
      type="l",
      col=124,
      main="Weekend average steps per interval",
      ylab="Average steps", 
      xlab="Interval ID")

plot( x=df.weekday.steps[,1], 
      y=df.weekday.steps[,2], 
      type="l",
      col=554,
      main="Weekday average steps per interval",
      ylab="Average steps", 
      xlab="Interval ID")


