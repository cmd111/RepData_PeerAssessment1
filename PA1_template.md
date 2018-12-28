---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)

activity<-read.csv(unz("activity.zip", "activity.csv"))

#get the total number of steps per day from the 5 minute intervals
dailyActivity<-aggregate(steps ~ date,activity[!is.na(activity$steps),], sum)

#Convert the factor dates to dates
dailyActivity$date<-as.Date(dailyActivity$date)

#Create the daily average dataset
avgActivity<-aggregate(steps ~ interval,activity[!is.na(activity$steps),], mean)
avgActivity$time<-as.POSIXct(strptime(sprintf("%04d", avgActivity$interval), format="%H%M"))
```

## What is mean total number of steps taken per day?

```r
print(paste("The mean number of steps taken per day is ",format(mean(dailyActivity$steps),digits=1,big.mark=","), "." ))
```

```
## [1] "The mean number of steps taken per day is  10,766 ."
```

```r
summary(dailyActivity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
hist(dailyActivity$steps, main="Steps per Day",xlab="Steps per Day",breaks=50,col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
ggplot(data=dailyActivity, aes(x=date, y=steps,col="red")) + 
    geom_line(aes(), size=1) +
    expand_limits(y=0) +                       
    xlab("Activity Day") + ylab("Total Steps") +
    ggtitle("Steps per Day") + theme(legend.position = "none")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->


## What is the average daily activity pattern?

```r
library(ggplot2)
ggplot(data=avgActivity, aes(x=interval, y=steps,col="red")) + 
    geom_line(aes(), size=1) +
    xlab("Time of Day") + ylab("Average Steps") +
    ggtitle("Average Daily Steps") + theme(legend.position = "none")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
highestStep<-avgActivity$time[avgActivity$steps==max(avgActivity$steps)]

print(paste("The time of day with the most average steps is ",format(highestStep,"%H:%M")))
```

```
## [1] "The time of day with the most average steps is  08:35"
```


## Imputing missing values

```r
#mean fill the missing values by time of day and day of week
activity$day<-weekdays(as.Date(activity$date))
activity$weekend<-ifelse(activity$day == "Sunday" | activity$day=="Saturday" ,"Weekend","Weekday")

avgActivity2<-aggregate(steps ~ day+interval,activity[!is.na(activity$steps),], mean)

activity.imp<-merge(activity,avgActivity2,by.x=c("day","interval"),by.y=c("day","interval") )

activity.imp$stepsImp<-ifelse(is.na(activity.imp$steps.x),activity.imp$steps.y,activity.imp$steps.x)

#get the total number of steps per day from the 5 minute intervals
dailyActivity.imp<-aggregate(stepsImp ~ date,activity.imp, sum)

#Convert the factor dates to dates
dailyActivity.imp$date<-as.Date(dailyActivity.imp$date)

#Create the daily average dataset
avgActivity.imp<-aggregate(stepsImp ~ interval,activity.imp, mean)
avgActivity.imp$time<-as.POSIXct(strptime(sprintf("%04d", avgActivity.imp$interval), format="%H%M"))
```

## What is mean total number of steps taken per day with  imputations

```r
print(paste("The mean number of steps taken per day is ",format(mean(dailyActivity.imp$stepsImp),digits=1,big.mark=","), "." ))
```

```
## [1] "The mean number of steps taken per day is  10,821 ."
```

```r
summary(dailyActivity.imp$stepsImp)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8918   11015   10821   12811   21194
```

```r
hist(dailyActivity.imp$stepsImp, main="Steps per Day Imputed",xlab="Steps per Day",breaks=50,col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
ggplot(data=dailyActivity.imp, aes(x=date, y=stepsImp,col="red")) + 
    geom_line(aes(), size=1) +
    expand_limits(y=0) +                       
    xlab("Activity Day") + ylab("Total Steps") +
    ggtitle("Steps per Day with Imputation") + theme(legend.position = "none")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)<!-- -->




## Are there differences in activity patterns between weekdays and weekends?


```r
avgActivity.impwe<-aggregate(stepsImp ~ interval+weekend,activity.imp, mean)

ggplot(data=avgActivity.impwe, aes(x=interval, y=stepsImp,col="red")) + 
    geom_line(aes(), size=1) + facet_wrap(~ weekend) +
    xlab("Time of Day") + ylab("Average Steps") +
    ggtitle("Average Daily Steps") + theme(legend.position = "none") 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
