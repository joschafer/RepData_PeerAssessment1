# Reproducible Research: Peer Assessment 1

## Synopsis
This analysis looks at the weekend versus weekday activity of an individual as measured in steps over time. From the data I found that the weekend and weekday activity is different for this individual with both different daily patterns and different overall activity levels.

## Loading and preprocessing the data
This data was collected from a personal activity monitoring device worn by an anonymous individual collected at 5 minute intervals through out the day. The data consists of two months of data during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The [https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip](data) was loaded from the course data as provided by R. Peng.

### Load the data
The data was expanded from its original zip format from the operating system and then examined with operating system tools.  It was found the be in CSV format with headers provided and it was read as is with headers.


```r
# Load supporting library and read in data
library(dplyr, warn.conflicts=FALSE)
StepsData <- read.csv("Data/activity.csv")
```
Overall the 17,568 observations were read with some NA data in the steps variable.


```r
# Basic summary
summary(StepsData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
# First 10 rows
head(StepsData, 10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

```r
# Random 10 rows
sample_n(StepsData, 10)
```

```
##       steps       date interval
## 400       0 2012-10-02      915
## 7925    128 2012-10-28     1220
## 5564      0 2012-10-20      735
## 16460     0 2012-11-27      335
## 4165      0 2012-10-15     1100
## 2633      0 2012-10-10      320
## 15496    31 2012-11-23     1915
## 766      88 2012-10-03     1545
## 12967     0 2012-11-15       30
## 9723    392 2012-11-03     1810
```

## Mean total number of steps taken per day?
The mean values are examined in 2 ways, a histogram of steps per day and the mean and median values of the overall data set.

#### Histogram of the total number of steps taken each day
A histogram of data with missing values skipped.


```r
RawSumStepsByDay <- StepsData %>% group_by(date) %>% summarize(stepsSum = sum(steps, na.rm=TRUE))
hist(RawSumStepsByDay$stepsSum, xlab="Steps per day", main="Histogram of steps per day (dirty)")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

#### Mean and median total number of steps taken per day.
The original data (wih missing values skipped) has a mean of 9354 and a median of 10395.


```r
mean(RawSumStepsByDay$stepsSum)
```

```
## [1] 9354.23
```

```r
median(RawSumStepsByDay$stepsSum)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
A time series plot of the 5-minute interval (x-axis) versus the average number of steps taken, averaged across all days (y-axis).  THe most active time of the day occurs between the 0800 and 1000 time each day.  Specifically, it occurs at 0835 with a 5 minute average of 106 steps.


```r
RawSumStepsBy5Min <- StepsData %>% group_by(interval) %>% summarize(stepsSum = sum(steps, na.rm=TRUE), stepsMean = mean(steps, na.rm=TRUE))
as.integer(RawSumStepsBy5Min[which.max(RawSumStepsBy5Min$stepsMean),"interval"])
```

```
## [1] 835
```

```r
max(RawSumStepsBy5Min$stepsMean)
```

```
## [1] 206.1698
```

```r
plot(x = RawSumStepsBy5Min$interval, y = RawSumStepsBy5Min$stepsMean, type='l', xlab="Time (24 hour)", ylab="Steps for 5 minutes", main="Steps versus time over 24 hours")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA). The extent of the missing data was  examined and the missing data was populated using the average across the complete data set for the corresponding interval.

Overall, there are 2304 periods with missing data that will be being populated.

```r
# Count of missing steps data
sum(is.na(StepsData$steps))
```

```
## [1] 2304
```

I created a cleaned data set by filling the NA values with the mean for the 5 minute interval across all days so that it could be compared with the original data.

```r
# Create new data set
find.stepsMean <- function (x) RawSumStepsBy5Min$stepsMean[match (x, RawSumStepsBy5Min$interval)]
CleanedStepsData <- StepsData %>%
      mutate(steps = ifelse(is.na(steps), find.stepsMean(interval),steps))
# Verify all NAs populated
sum(is.na(CleanedStepsData$steps))
```

```
## [1] 0
```
## Post cleanup data analysis

TODO Left off here
-- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


#### A histogram of the cleaned data.


```r
CleanedSumStepsByDay <- CleanedStepsData %>% group_by(date) %>% summarize(stepsSum = sum(steps, na.rm=TRUE))
hist(CleanedSumStepsByDay$stepsSum,xlab="Steps per day", main="Histogram of steps per day (clean)")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

#### Mean and median total number of steps taken per day.
The cleaned data (wih missing values replaced) has both a mean of 10766 and a median of 10766.

```r
mean(CleanedSumStepsByDay$stepsSum)
```

```
## [1] 10766.19
```

```r
median(CleanedSumStepsByDay$stepsSum)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

-- For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

-- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
is.weekend <- function (x) as.factor(ifelse(weekdays(x) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
CleanedStepsData <- mutate (CleanedStepsData, weekend = is.weekend(as.Date(date)))
CleanedSumStepsByDay <- mutate (CleanedSumStepsByDay, weekend = is.weekend(as.Date(date)))
```


-- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
# TODO Clean up plots
CleanedSumStepsForWeekendBy5Min <- filter(CleanedStepsData, weekend == "Weekend") %>% group_by(weekend, interval) %>% summarize(stepsSum = sum(steps, na.rm=TRUE), stepsMean = mean(steps, na.rm=TRUE))
CleanedSumStepsForWeekdayBy5Min <- filter(CleanedStepsData, weekend == "Weekday") %>% group_by(weekend, interval) %>% summarize(stepsSum = sum(steps, na.rm=TRUE), stepsMean = mean(steps, na.rm=TRUE))
par(mfrow = c(2,1))
plot(x = CleanedSumStepsForWeekendBy5Min$interval, y = CleanedSumStepsForWeekendBy5Min$stepsMean, type='l')
plot(x = CleanedSumStepsForWeekdayBy5Min$interval, y = CleanedSumStepsForWeekdayBy5Min$stepsMean, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

