# Reproducible Research: Peer Assessment 1
June 06, 2016  


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header=TRUE, sep=",")
head(data)
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
## What is mean total number of steps taken per day?

1. Total the total number of steps taken perday

```r
totalsteps <- aggregate(steps~date, data=data, sum, na.rm=TRUE)
colnames(totalsteps) <- c("Date", "Steps")
```
2. Make a histogram of the total number of steps taken each daya

```r
hist(totalsteps$Steps, main="Total Number of Steps Taken Each Day", xlab = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
paste("Mean= ", round(mean(totalsteps$Steps)))
```

```
## [1] "Mean=  10766"
```

```r
paste("Median= ", round(median(totalsteps$Steps)))
```

```
## [1] "Median=  10765"
```
## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsbyinterval <- aggregate(steps ~ interval, data=data, mean, na.rm=TRUE)
plot(steps ~ interval, data=stepsbyinterval, type="l", main="Mean steps taken in 5 minute interval", xlab="Interval", ylab="Steps", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
paste(stepsbyinterval[which.max(stepsbyinterval$steps), ]$interval, "th  Interval", sep="")
```

```
## [1] "835th  Interval"
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


I will use the mean to impute the NA values

```r
# Duplicate original data
NewData <- data

number_Of_Int <- nrow(stepsbyinterval)
n <- nrow(NewData)

for ( i in 1:n) {
  
  if ( is.na(NewData[i,1]) ) { # if the data is NA
       Cor_Interval <- NewData[i,3] # get what the corresponding interval
       w <- which(stepsbyinterval$interval==Cor_Interval)
       inputvalue <- stepsbyinterval[w,2]
       NewData[i,1] <- inputvalue
  }
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# New Dataset
head(NewData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Newtotalsteps <- aggregate(steps~date, data=NewData, sum, na.rm=TRUE)
colnames(Newtotalsteps) <- c("Date", "Steps")
hist(Newtotalsteps$Steps, main="Total Number of Steps Taken Each Day", xlab = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
paste("Mean= ", round(mean(Newtotalsteps$Steps)))
```

```
## [1] "Mean=  10766"
```

```r
paste("Median= ", round(median(Newtotalsteps$Steps)))
```

```
## [1] "Median=  10766"
```

The mean value is same as before imputing. Reason being we choosed to impute the missing values with mean values.

The median values only differ slightly by point one only as the median value is much robust than mean.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
day <- c("weekday", "weekend")
NewData$day <- ifelse(as.POSIXlt(as.Date(NewData$date))$wday%%6==0,"weekday", "weekend")

NewData$day <- factor(NewData$day, levels=c("weekday", "weekend"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 




```r
library(lattice)
Newstepsbyinterval <- aggregate(steps ~ interval, data=NewData, mean, na.rm=TRUE)
xyplot(steps ~ interval | factor(day), data=Newstepsbyinterval, layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
