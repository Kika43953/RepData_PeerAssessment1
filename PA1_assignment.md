---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
setwd("C:\\Users\\kkiyo\\Desktop\\ProgrammingAssignment2\\UCI HAR Dataset\\RepData_PeerAssessment1")
dt <- read.csv("./activity.csv")
dt$date <- as.Date(dt$date)
# names(dt) # "steps"    "date"     "interval" "date1"  
```

## What is mean total number of steps taken per day?
Calculate and report the mean and median of the total number of steps taken per day.
Mean of the total number of steps taken per day is 10766.19
Median of the total number of steps taken per day is 10765


```r
## Calculate the total number of steps taken per day
numstep <- dt %>% group_by(date) %>% summarise(stepsum = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
## Make a histogram of the total number of steps taken each day
hist(numstep$stepsum,breaks = 10)
```

![](PA1_assignment_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## Calculate the mean and median of the total number of steps taken per day
sprintf("Mean is %.2f", mean(numstep$stepsum, na.rm=TRUE)) ## mean is 10766.19
```

```
## [1] "Mean is 10766.19"
```

```r
sprintf("Median is %.2f", median(numstep$stepsum, na.rm=TRUE)) ## median is 10765
```

```
## [1] "Median is 10765.00"
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## Create new column for each interval
# dt$intervals <- rep(1:288, dim(dt)[1]/(24*12))

## Group by interval
avestep <- dt %>% group_by(interval) %>% summarise(ave = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
## Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis)
plot(avestep$interval/100, avestep$ave, type="l", main="Average steps across all days", xlab="Hour", ylab="Average steps")
```

![](PA1_assignment_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
sprintf("The interval which gives max number of steps is %.0f", avestep[avestep$ave == max(avestep$ave), ][1]) ## 835
```

```
## [1] "The interval which gives max number of steps is 835"
```
## Imputing missing values
Do these values differ from the estimates from the first part of the assignment? 
--> No, they do not.

What is the impact of imputing missing data on the estimates of the total daily number of steps?
--> There is no impact.


```r
## Calculate and report the total number of missing values in the dataset
sprintf("Number of missing values are %.0f", sum(is.na(dt$steps)))
```

```
## [1] "Number of missing values are 2304"
```

```r
## Replacing missing values by mean of 5-min intervals
dt.imp.mean <- dt
dt$steps1 <- rep(avestep$ave, 61)
for (i in as.numeric(rownames(dt))) {
  if (is.na(dt$steps[i])) {
    dt.imp.mean$steps[i] = dt$steps1[i]
  } 
  else {
    dt.imp.mean$steps[i] = dt$steps[i]
  }
}

## Create a new dataset that is equal to the original dataset but with the missing data filled in.


## Calculate the total number of steps taken per day
numstep.imp.mean <- dt %>% group_by(date) %>% summarise(stepsum = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
## Make a histogram of the total number of steps taken each day
hist(numstep.imp.mean$stepsum,breaks = 10)
```

![](PA1_assignment_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
## Calculate the mean and median of the total number of steps taken per day
sprintf("Mean is %.2f", mean(numstep.imp.mean$stepsum, na.rm=TRUE)) ## mean is 10766.19
```

```
## [1] "Mean is 10766.19"
```

```r
sprintf("Median is %.2f", median(numstep.imp.mean$stepsum, na.rm=TRUE)) ## median is 10765.00
```

```
## [1] "Median is 10765.00"
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays()\color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

    Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
    Make a panel plot containing a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

#create a vector of weekdays
weekdays1 <- c('月曜日', '火曜日', '水曜日', '木曜日', '金曜日')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
dt.imp.mean$wDay <- factor((weekdays(dt.imp.mean$date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
#Or
dt.imp.mean$wDay <- c('weekend', 'weekday')[(weekdays(dt.imp.mean$date) %in% weekdays1)+1L]


## Group by interval
dt.imp.mean.wd <- dt.imp.mean[dt.imp.mean$wDay=="weekday",]
dt.imp.mean.we <- dt.imp.mean[dt.imp.mean$wDay=="weekend",]

avestep.wd <- dt.imp.mean.wd %>% group_by(interval) %>% summarise(ave = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
avestep.we <- dt.imp.mean.we %>% group_by(interval) %>% summarise(ave = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
## Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis)
par(mfrow=c(2,1))
plot(avestep.wd$interval, avestep.wd$ave, type="l", main="Average steps in weekday", xlab="Interval", ylab="Number of steps")
plot(avestep.we$interval, avestep.we$ave, type="l", main="Average steps in weekend", xlab="Interval", ylab="Number of steps")
```

![](PA1_assignment_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


