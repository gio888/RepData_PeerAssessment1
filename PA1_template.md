# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First we set the folder location and read the file into R for analysis:


```r
setwd("/Users/gio/Git Repositories/RepData_PeerAssessment1")
actdata<-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

Analysing the data, we will make a histogram of the total number of steps taken each day, calculate and report the mean and median total number of steps taken per day.


```r
aggdata <- aggregate(steps ~ date,data=actdata,FUN=sum)
hist(aggdata$steps,main="Histogram of Total Steps Per Day",xlab="Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(aggdata$steps)
```

```
## [1] 10766.19
```

```r
median(aggdata$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

W make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:


```r
by_interval<-aggregate(steps ~ interval, data=actdata, FUN=mean)
plot(by_interval$interval,by_interval$steps,type="l",main="Average Steps per 5 minute interval",xlab="Intervals",ylab="Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is:

```r
by_interval[which.max( by_interval[,2] ),1]
```

```
## [1] 835
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
