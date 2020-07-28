---
title: "PA1_template.Rmd"
author: "Syed Shaheryar Tirmizi"
date: "7/7/2020"
output: html_document
keep_md: true

---
## 1.Loading and Preprocessing of data

```r
library(knitr)
library(ggplot2)
library(plyr)
data<-read.csv("activity.csv")
activitydata<-data[complete.cases(data),]
```
## 2.Histogram of the total number of steps taken each day

```r
filterdata<-aggregate(activitydata$steps, by=list(activitydata$date), sum)
names(filterdata)<-c("date","steps")
ggplot(data=filterdata, aes(x= steps, col="purple"))+ geom_histogram(binwidth = 1000)+ labs(title = "Total steps taken per day",x="Steps", y="Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

## 3.Mean and median number of steps taken each day

```r
meansteps<-mean(filterdata$steps)
meansteps
```

```
## [1] 10766.19
```

```r
mediansteps<-median(filterdata$steps)
mediansteps
```

```
## [1] 10765
```

## 4.Time series plot of the average number of steps taken

```r
timeseries<-aggregate(activitydata$steps, by=list(activitydata$interval), mean)
names(timeseries)<-c("Interval","Steps")
ggplot(data=timeseries, aes(x= Interval, y=Steps)) +geom_line()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## 5.The 5-minute interval that, on average, contains the maximum number of steps

```r
maxStepInterval<-timeseries[which.max(timeseries$Steps),]
maxStepInterval
```

```
##     Interval    Steps
## 104      835 206.1698
```

## 6.Code to describe and show a strategy for imputing missing data

```r
totalNAs<-sum(is.na(data$steps))
totalNAs
```

```
## [1] 2304
```

```r
index<-which(is.na(data$steps))
newData<-data
for (i in index)
{
      newData$steps[i]<- timeseries$Steps[which(timeseries$Interval== newData$interval[i])]
}
```

## 7.Histogram of the total number of steps taken each day after missing values are imputed

```r
newFilterData<-aggregate(newData$steps , by=list(newData$date), sum)
names(newFilterData)<-c("date","steps")
ggplot(data=newFilterData, aes(x= steps, col="purple"))+ geom_histogram(binwidth = 1000)+ labs(title = "Total steps taken per day",x="Steps", y="Frequency")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

## 8.Mean and Median after filling missing values

```r
Newmeansteps<-mean(newFilterData$steps)
Newmeansteps
```

```
## [1] 10766.19
```

```r
Newmediansteps<-median(newFilterData$steps)
Newmediansteps
```

```
## [1] 10766.19
```

## 9.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
days<-as.factor(weekdays(as.Date(activitydata$date)))
DayType<-revalue(days, c("Monday"="Weekday","Tuesday"="Weekday","Wednesday"="Weekday","Thursday"="Weekday","Friday"="Weekday","Saturday"="Weekend","Sunday"="Weekend"))
activitydata<-cbind(activitydata, DayType)
Newtimeseries<-aggregate(activitydata$steps, by=list(activitydata$interval, activitydata$DayType), mean)
names(Newtimeseries)<-c("Interval","DayType","Steps")
ggplot(data=Newtimeseries, aes(x= Interval, y=Steps)) +geom_line(aes(colour=DayType)) +facet_grid(DayType~.)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
