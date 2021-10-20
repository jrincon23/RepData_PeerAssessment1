---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data <- read.csv(unz("activity.zip","activity.csv"),header=TRUE)
data$date <- as.Date(data$date)
complete_cases <- data[complete.cases(data),]
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
steps_by_day <- aggregate(steps ~ date,complete_cases,sum)
```

2. Make a histogram of the total of steps taken each day

```r
hist(steps_by_day$steps, main="Total number of steps taken each day",xlab="Steps", ylab="Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
summarized_data <- complete_cases %>%
group_by(date) %>%
summarize(Mean=mean(steps),Median=median(steps[steps>0],na.rm=TRUE)) %>%
print
```

```
## # A tibble: 53 x 3
##    date         Mean Median
##    <date>      <dbl>  <dbl>
##  1 2012-10-02  0.438   63  
##  2 2012-10-03 39.4     61  
##  3 2012-10-04 42.1     56.5
##  4 2012-10-05 46.2     66  
##  5 2012-10-06 53.5     67  
##  6 2012-10-07 38.2     52.5
##  7 2012-10-09 44.5     48  
##  8 2012-10-10 34.4     56.5
##  9 2012-10-11 35.8     35  
## 10 2012-10-12 60.4     46  
## 11 2012-10-13 43.1     45.5
## 12 2012-10-14 52.4     60.5
## 13 2012-10-15 35.2     54  
## 14 2012-10-16 52.4     64  
## 15 2012-10-17 46.7     61.5
## 16 2012-10-18 34.9     52.5
## 17 2012-10-19 41.1     74  
## 18 2012-10-20 36.1     49  
## 19 2012-10-21 30.6     48  
## 20 2012-10-22 46.7     52  
## 21 2012-10-23 31.0     56  
## 22 2012-10-24 29.0     51.5
## 23 2012-10-25  8.65    35  
## 24 2012-10-26 23.5     36.5
## 25 2012-10-27 35.1     72  
## 26 2012-10-28 39.8     61  
## 27 2012-10-29 17.4     54.5
## 28 2012-10-30 34.1     40  
## 29 2012-10-31 53.5     83.5
## 30 2012-11-02 36.8     55.5
## 31 2012-11-03 36.7     59  
## 32 2012-11-05 36.2     66  
## 33 2012-11-06 28.9     52  
## 34 2012-11-07 44.7     58  
## 35 2012-11-08 11.2     42.5
## 36 2012-11-11 43.8     55  
## 37 2012-11-12 37.4     42  
## 38 2012-11-13 25.5     57  
## 39 2012-11-15  0.142   20.5
## 40 2012-11-16 18.9     43  
## 41 2012-11-17 49.8     65.5
## 42 2012-11-18 52.5     80  
## 43 2012-11-19 30.7     34  
## 44 2012-11-20 15.5     58  
## 45 2012-11-21 44.4     55  
## 46 2012-11-22 70.9     65  
## 47 2012-11-23 73.6    113  
## 48 2012-11-24 50.3     65.5
## 49 2012-11-25 41.1     84  
## 50 2012-11-26 38.8     53  
## 51 2012-11-27 47.4     57  
## 52 2012-11-28 35.4     70  
## 53 2012-11-29 24.5     44.5
```

## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval and the average number of steps taken

```r
mean_5_minutes_interval <- aggregate(steps ~ interval,complete_cases,mean)
with(mean_5_minutes_interval,plot(mean_5_minutes_interval$interval,steps,type="l",main="Average number of steps each 5 minutes interval",xlab="Interval",ylab="Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
mean_5_minutes_interval[which.max(mean_5_minutes_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset


```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

2. Filling the missing values of the dataset with the mean for the interval and 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
complete_data <- data
for(r in 1:nrow(complete_data)){
  if(is.na(complete_data$steps[r])){
    m <- mean_5_minutes_interval[mean_5_minutes_interval$interval==complete_data$interval[r],"steps"]
    complete_data$steps[r]<- as.integer(round(m))
  }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.Do these values differ from the estimates from the first part of the assignment?


```r
complete_steps_by_day <- aggregate(steps ~ date,complete_data,sum)
hist(steps_by_day$steps, main="Total number of steps taken each day (new dataset)",xlab="Steps", ylab="Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
complete_summarized_data <- complete_data %>%
group_by(date) %>%
summarize(Mean=mean(steps),Median=median(steps[steps>0],na.rm=TRUE)) %>%
print
```

```
## # A tibble: 61 x 3
##    date         Mean Median
##    <date>      <dbl>  <dbl>
##  1 2012-10-01 37.4     40.5
##  2 2012-10-02  0.438   63  
##  3 2012-10-03 39.4     61  
##  4 2012-10-04 42.1     56.5
##  5 2012-10-05 46.2     66  
##  6 2012-10-06 53.5     67  
##  7 2012-10-07 38.2     52.5
##  8 2012-10-08 37.4     40.5
##  9 2012-10-09 44.5     48  
## 10 2012-10-10 34.4     56.5
## 11 2012-10-11 35.8     35  
## 12 2012-10-12 60.4     46  
## 13 2012-10-13 43.1     45.5
## 14 2012-10-14 52.4     60.5
## 15 2012-10-15 35.2     54  
## 16 2012-10-16 52.4     64  
## 17 2012-10-17 46.7     61.5
## 18 2012-10-18 34.9     52.5
## 19 2012-10-19 41.1     74  
## 20 2012-10-20 36.1     49  
## 21 2012-10-21 30.6     48  
## 22 2012-10-22 46.7     52  
## 23 2012-10-23 31.0     56  
## 24 2012-10-24 29.0     51.5
## 25 2012-10-25  8.65    35  
## 26 2012-10-26 23.5     36.5
## 27 2012-10-27 35.1     72  
## 28 2012-10-28 39.8     61  
## 29 2012-10-29 17.4     54.5
## 30 2012-10-30 34.1     40  
## 31 2012-10-31 53.5     83.5
## 32 2012-11-01 37.4     40.5
## 33 2012-11-02 36.8     55.5
## 34 2012-11-03 36.7     59  
## 35 2012-11-04 37.4     40.5
## 36 2012-11-05 36.2     66  
## 37 2012-11-06 28.9     52  
## 38 2012-11-07 44.7     58  
## 39 2012-11-08 11.2     42.5
## 40 2012-11-09 37.4     40.5
## 41 2012-11-10 37.4     40.5
## 42 2012-11-11 43.8     55  
## 43 2012-11-12 37.4     42  
## 44 2012-11-13 25.5     57  
## 45 2012-11-14 37.4     40.5
## 46 2012-11-15  0.142   20.5
## 47 2012-11-16 18.9     43  
## 48 2012-11-17 49.8     65.5
## 49 2012-11-18 52.5     80  
## 50 2012-11-19 30.7     34  
## 51 2012-11-20 15.5     58  
## 52 2012-11-21 44.4     55  
## 53 2012-11-22 70.9     65  
## 54 2012-11-23 73.6    113  
## 55 2012-11-24 50.3     65.5
## 56 2012-11-25 41.1     84  
## 57 2012-11-26 38.8     53  
## 58 2012-11-27 47.4     57  
## 59 2012-11-28 35.4     70  
## 60 2012-11-29 24.5     44.5
## 61 2012-11-30 37.4     40.5
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend da


```r
complete_data <- complete_data %>%
mutate(day = ifelse(weekdays(date)=="Sunday" | weekdays(date) == "Saturday","weekend","weekday"))

complete_data$day <- as.factor(complete_data$day)
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
mean_5_minutes_interval_day <- complete_data %>%
  group_by(interval,day) %>%
  summarize(Mean=mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
p <- ggplot(data=mean_5_minutes_interval_day,aes(x=interval,y=Mean,color=day)) + 
  facet_grid(rows=mean_5_minutes_interval_day$day) +
  ylab("Steps") +
  xlab("Interval") +
  
labs(title = "Total steps by interval by day type")+
geom_line() 
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

