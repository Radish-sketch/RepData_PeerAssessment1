---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip "
download.file(fileUrl,destfile = ".\\activity_monitoring.zip",mode='wb',cacheOK = FALSE)

unzip("activity_monitoring.zip",exdir=getwd())
data<-read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

```r
    sumByday<-tapply(data$steps, data$date, sum,na.rm=TRUE)
    hist(sumByday,
         main="Histogram of total number of steps taken per day",
         xlab="total steps per day")
```

![](PA1_template_files/figure-html/setupPerday-1.png)<!-- -->

```r
    meanSteps<-mean(sumByday)
    medianSteps<-median(sumByday)
```

The mean number of steps taken each day is **9354.2295082**, the median number of steps taken each day is **10395**. 




## What is the average daily activity pattern? 
 
    
Time series plot of the average number of steps taken per day

```r
    meanPerDay<-tapply(data$steps,data$date,mean,na.rm=TRUE)
    plot(meanPerDay,type="l",
         ylab="average number of steps",
         xlab="day")
```

![](PA1_template_files/figure-html/dailyActivity-1.png)<!-- -->

Time series plot of the average number of steps

```r
    plot(data$steps,type="l",
         ylab="number of steps in 5 min",
         xlab="interval")
```

![](PA1_template_files/figure-html/stepsVsTime-1.png)<!-- -->





```r
    maxStep<-data[which.max(data$steps),]
    maxInterval<-which.max(data$steps)
```
The 5-minute interval that, on average, contains the maximum number of steps is **806**.


The maximum steps per 5 minutes on average is on **2012-11-27**, interval **615**. The number **16492** in the dataset.


## Imputing missing values

Number of NA values can be found 

```r
a<-table(is.na(data$steps))
a
```

```
## 
## FALSE  TRUE 
## 15264  2304
```
There are **2304** NA values in total. And either a whole day is NA or there's no NA in that day,replace NA value with the mean of all the non-NA steps in 5 min interval. codes not shown here.


```r
naRow<-which(is.na(data$steps))

meanTotal<-mean(data$steps,na.rm=TRUE)
dataImputed<-data

naTotal<-sum(is.na(dataImputed$steps))
dataImputed[which(is.na(data$steps)),1]<-meanTotal


for (i in naRow){
    if (is.na(meanSteps[dataImputed[i,2]])){
        dataImputed[i,1]<-meanTotal
    } else {
        
        dataImputed[i,1]<-meanSteps[dataImputed[i,2]]
    
    }
}
```



Histogram of the total number of steps taken each day after missing values are imputed

```r
    sumBydayIm<-tapply(dataImputed$steps, dataImputed$date, sum,na.rm=TRUE)
    hist(sumBydayIm,
         main="Total number of steps each day after missing value are imputed", 
         xlab="Total number of steps")
```

![](PA1_template_files/figure-html/histImputed-1.png)<!-- -->

```r
    meanImputed<-mean(sumBydayIm)
```

The mean value of total number of steps taken each day before imputing is **9354.2295082**,  after imputing is **1.0766189\times 10^{4}**.


## Are there differences in activity patterns between weekdays and weekends?

The number of data points on weekdays is more than on weekends, but it seems on average, the number of steps on weekdays are more than on weekends.

```r
dataImputed$date<-as.Date(dataImputed$date)
weekend <- c('Saturday', 'Sunday')
dataImputed$wDay<-factor((weekdays(dataImputed$date) %in% weekend),levels<-c(TRUE,FALSE),labels=c('weekends','weekdays'))
#table(dataImputed$wDay)
library(lattice)
xyplot(dataImputed$steps~dataImputed$interval|dataImputed$wDay,type = "l",
          main="Panel plot comparing the average number of steps per 5 min",
          xlab="Interval",
          ylab="Number of steps",
          layout=c(1,2))
```

![](PA1_template_files/figure-html/wDays-1.png)<!-- -->


## Write to HTML file

with code rmarkdown::render("PA1_template.Rmd")
