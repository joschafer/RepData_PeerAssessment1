# Reproducible Research: Peer Assessment 1

## Synopsis
TODO - What does this analysis represent?  


## Loading and preprocessing the data
TODO - Describe where the data comes from and give attribution

### Load the data
TODO Describe the loading and first look at the data

```r
# Load supporting library and read in data
library(dplyr, warn.conflicts=FALSE)
StepsData <- read.csv("Data/activity.csv")
```
TODO Summary and first look at data.


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
## 12484     0 2012-11-13      815
## 4485      0 2012-10-16     1340
## 10422     0 2012-11-06      425
## 12797    NA 2012-11-14     1020
## 721      97 2012-10-03     1200
## 17139     0 2012-11-29     1210
## 6483    376 2012-10-23     1210
## 10491   238 2012-11-06     1010
## 5266      0 2012-10-19      645
## 17150     0 2012-11-29     1305
```

- Process/transform the data (if necessary) into a format suitable for your analysis

## What is mean total number of steps taken per day?
- For this part of the assignment, you can ignore the missing values in the dataset.

#### Make a histogram of the total number of steps taken each day

```r
# TODO Clean up plots
RawSumStepsByDay <- StepsData %>% group_by(date) %>% summarize(stepsSum = sum(steps, na.rm=TRUE))
hist(RawSumStepsByDay$stepsSum)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

#### Calculate and report the mean and median total number of steps taken per day

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
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# TODO Clean up plots
RawSumStepsBy5Min <- StepsData %>% group_by(interval) %>% summarize(stepsSum = sum(steps, na.rm=TRUE), stepsMean = mean(steps, na.rm=TRUE))
plot(x = RawSumStepsBy5Min$interval, y = RawSumStepsBy5Min$stepsMean, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
maxInterval <- c(which.max(RawSumStepsBy5Min$stepsMean), max(RawSumStepsBy5Min$stepsMean))
```


## Imputing missing values
- Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

-- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NaCount <- sum(is.na(StepsData$steps))
```


-- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Fill the NA values with the mean for the 5 minute interval across all days

-- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
find.stepsMean <- function (x) RawSumStepsBy5Min$stepsMean[match (x, RawSumStepsBy5Min$interval)]
CleanedStepsData <- StepsData %>%
      mutate(steps = ifelse(is.na(steps), find.stepsMean(interval),steps))
CleanedNaCount <- sum(is.na(CleanedStepsData$steps))
```

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
# TODO Clean up plots
CleanedSumStepsByDay <- CleanedStepsData %>% group_by(date) %>% summarize(stepsSum = sum(steps, na.rm=TRUE))
hist(CleanedSumStepsByDay$stepsSum)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
CleanedMeanStepsByDay <- mean(CleanedSumStepsByDay$stepsSum)
CleanedMedianStepsByDay <- median(CleanedSumStepsByDay$stepsSum)
```

## Are there differences in activity patterns between weekdays and weekends?

### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
is.weekend <- function (x) as.factor(ifelse(weekdays(x) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
CleanedStepsData <- mutate (CleanedStepsData, weekend = is.weekend(as.Date(date)))
CleanedSumStepsByDay <- mutate (CleanedSumStepsByDay, weekend = is.weekend(as.Date(date)))
```


#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
# TODO Clean up plots
CleanedSumStepsForWeekendBy5Min <- filter(CleanedStepsData, weekend == "Weekend") %>% group_by(weekend, interval) %>% summarize(stepsSum = sum(steps, na.rm=TRUE), stepsMean = mean(steps, na.rm=TRUE))
CleanedSumStepsForWeekdayBy5Min <- filter(CleanedStepsData, weekend == "Weekday") %>% group_by(weekend, interval) %>% summarize(stepsSum = sum(steps, na.rm=TRUE), stepsMean = mean(steps, na.rm=TRUE))
par(mfrow = c(2,1))
plot(x = CleanedSumStepsForWeekendBy5Min$interval, y = CleanedSumStepsForWeekendBy5Min$stepsMean, type='l')
plot(x = CleanedSumStepsForWeekdayBy5Min$interval, y = CleanedSumStepsForWeekdayBy5Min$stepsMean, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

