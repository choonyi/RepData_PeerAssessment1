---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
suppressPackageStartupMessages(library(dplyr))
##Load the data
data <- read.csv("activity.csv", na.strings = "NA")

##Process/transform the data (if necessary) into a format suitable for your analysis
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

```r
##Make a histogram of the total number of steps taken each day
daily <- data %>% 
        group_by(date) %>%
        summarize(total.n.steps = sum(steps, na.rm = TRUE))

hist(x = daily$total.n.steps,
     main = "Total number of steps taken each day",
     xlab = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
##mean and median total number of steps taken per day
mean <- mean(daily$total.n.steps)
median <- median(daily$total.n.steps)
mean
```

```
## [1] 9354.23
```

```r
median
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r
##Make a time series plot (i.e. type = "l") of the 5-minute interval 
##(x-axis) and the average number of steps taken, averaged across all days (y-axis)

avg <- data %>%
        group_by(interval) %>%
        summarize(avg_steps = mean(steps, na.rm=TRUE))

plot(x = avg$interval,
     y = avg$avg_steps,
     type = "l",
     main = "Average number of steps taken on 5-minute interval",
     xlab = "5-minute interval",
     ylab = "Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
##Which 5-minute interval, on average across all the days in the dataset, 
##contains the maximum number of steps?

avg$interval[which(avg$avg_steps == max(avg$avg_steps))]
```

```
## [1] 835
```


## Imputing missing values

```r
##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
length(which(is.na(data$steps)))
```

```
## [1] 2304
```

```r
##Devise a strategy for filling in all of the missing values in the dataset. 
##The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
##or the mean for that 5-minute interval, etc.

impute_mising_values <- function(df){
        ##First 
}
```


## Are there differences in activity patterns between weekdays and weekends?
