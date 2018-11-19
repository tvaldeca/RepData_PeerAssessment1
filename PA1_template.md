---
title: "RepData_PeerAssessment1"
output:
  html_document:
    keep_md: yes
---



### Loading and preprocessing the data


```r
data <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
```

### What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day:


```r
sum_daily_steps <- subset(data, !is.na(steps), c(date, steps)) %>% group_by(date) %>% summarise(steps = sum(steps))

ggplot(sum_daily_steps, aes(steps)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/totaldailysteps-1.png)<!-- -->

```r
mean_daily_steps <- sum_daily_steps %>% summarise(steps = mean(steps))
median_daily_steps <- sum_daily_steps %>% summarise(steps = median(steps))
```

The mean number of steps taken per day is: 1.0766189\times 10^{4}  
The median number of steps taken per day is: 10765

### What is the average daily activity pattern?

Time series plot of the average number of steps taken:


```r
int_avg_steps <- subset(data, !is.na(steps), c(interval, steps)) %>% group_by(interval) %>% summarise(avg_steps = mean(steps))

ggplot(int_avg_steps, aes(interval, avg_steps)) + geom_line()
```

![](PA1_template_files/figure-html/intavgsteps-1.png)<!-- -->

```r
max_steps <- filter(int_avg_steps, avg_steps == max(avg_steps))$interval
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 835

### Imputing missing values


```r
data_missing <- subset(data, is.na(steps))
data_not_missing <- subset(data, !is.na(steps))

missing_values <- data_missing %>% summarise(missing = n())
```

Total number of missing values in the dataset: 2304

Impute missing values by using the average steps by interval...


```r
data_missing_fix <- merge(data_missing, int_avg_steps, by.x="interval", by.y="interval") %>% select(-steps) %>% rename(steps=avg_steps)
data_fix <- rbind(data_missing_fix, data_not_missing)
```

Histogram of the total number of steps taken each day using dataset with missing values filled in:


```r
sum_daily_steps_fix <- subset(data_fix, !is.na(steps), c(date, steps)) %>% group_by(date) %>% summarise(steps = sum(steps))

ggplot(sum_daily_steps_fix, aes(steps)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/histfix-1.png)<!-- -->

```r
mean_daily_steps_fix <- sum_daily_steps_fix %>% summarise(steps = mean(steps))
median_daily_steps_fix <- sum_daily_steps_fix %>% summarise(steps = median(steps))

diffmean <- mean_daily_steps_fix$steps - mean_daily_steps$steps
diffmedian <- median_daily_steps_fix$steps - median_daily_steps$steps
```

Mean total number of steps taken per day using dataset with missing values filled in: 1.0766189\times 10^{4}  
Median total number of steps taken per day using dataset with missing values filled in: 1.0766189\times 10^{4}
  
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
Average difference is 0 so there's been no change with the average but the change in the median is 1.1886792 so there is some impact to the median.
  
### Are there differences in activity patterns between weekdays and weekends?

Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):


```r
data_fix$weekday_ind <- factor(weekdays(strptime(data_fix$date, "%Y-%m-%d")))

levels(data_fix$weekday_ind) <- c("Weekday","Weekday","Weekend","Weekend","Weekday","Weekday","Weekday")

int_avg_steps_fix <- data_fix[, c(1,3,4)] %>% group_by(interval, weekday_ind) %>% summarise(avg_steps = mean(steps))

ggplot(int_avg_steps_fix, aes(interval, avg_steps)) + geom_line() + facet_wrap(~weekday_ind, ncol = 1)
```

![](PA1_template_files/figure-html/weekday-1.png)<!-- -->
  
Assignment end!

