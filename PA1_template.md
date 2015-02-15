---
title: "Reproducible Research Peer Assessment 1"
author: "Y. Guo"
date: "February 15, 2015"
output: html_document
---



### Loading and preprocessing the data
Show any code that is needed to

* Load the data


```r
activity<-read.csv("./activity.csv")
```

* Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date<-as.Date(activity$date,format="%Y-%m-%d")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Calculate the total number of steps taken per day

```r
StepsTotal<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
```

* Make a histogram of the total number of steps taken each day

```r
hist(StepsTotal$steps,main="Total Number of Steps Taken Each Day",xlab="", col="red")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

* Calculate and report the mean and median of the total number of steps taken per day

```r
# mean
mean(StepsTotal$steps)
```

```
## [1] 10766.19
```

```r
# median
median(StepsTotal$steps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ts_data<-with(activity,aggregate(steps,by=list(interval),mean,na.rm=TRUE))
plot(ts_data,type="l",main="Average Daily Activity Pattern",xlab="5-Minute Interval", ylab="Average Number of Steps",col="red")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
ts_data$Group.1[which.max(ts_data$x)]
```

```
## [1] 835
```

### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Here I used the mean for that 5-minutes interval to fill in all of the missing values.


```r
StepsAverage <- aggregate(steps ~ interval, data = activity, mean)
fill_na<-numeric()
for (i in 1:nrow(activity)) {
  obs <- activity[i, ]
  if (is.na(obs$steps)) {
    steps <- StepsAverage$steps[StepsAverage$interval == obs$interval]
  } else {
    steps <- obs$steps
  }
  fill_na <- c(fill_na, steps)
}
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
modified_activity<-activity
modified_activity$steps<-fill_na
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
StepsTotal2<-aggregate(steps~date,data=modified_activity,sum,na.rm=TRUE)
hist(StepsTotal2$steps,main="Total Number of Steps Taken Each Day",xlab="", col="red")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
# mean
mean(StepsTotal2$steps)
```

```
## [1] 10766.19
```

```r
# median
median(StepsTotal2$steps)
```

```
## [1] 10766.19
```

```r
mean(StepsTotal2$steps)-mean(StepsTotal$steps)
```

```
## [1] 0
```

```r
median(StepsTotal2$steps)-median(StepsTotal$steps)
```

```
## [1] 1.188679
```

From the outputs above, we see that the means are the same; however, the median increases a little bit.

### Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
day<-weekdays(modified_activity$date)
daylevel<-vector()
for (i in 1:nrow(modified_activity)){
  if (day[i]=="Saturday"|| day[i]=="Sunday"){
    add_day<-"Weekends"
  }else{
    add_day<-"Weekdays"
  }
  daylevel<-c(daylevel,add_day)
}
daylevel<-factor(daylevel)
modified_activity$daylevel<-daylevel
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
Steps_Daylevel<-aggregate(steps~interval+daylevel,data=modified_activity,mean)
library(lattice)
xyplot(steps~interval|daylevel,data=Steps_Daylevel,type="l",layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

