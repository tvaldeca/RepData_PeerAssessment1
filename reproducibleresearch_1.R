library(dplyr)

data <- read.csv("activity.csv", header = TRUE, na.strings = "NA")

sum_daily_steps <- subset(data, !is.na(steps), c(date, steps)) %>% group_by(date) %>% summarise(steps = sum(steps))

with(sum_daily_steps, hist(steps))

mean_daily_steps <- sum_daily_steps %>% summarise(steps = mean(steps))
median_daily_steps <- sum_daily_steps %>% summarise(steps = median(steps))

#Mean and median number of steps taken each day

int_avg_steps <- subset(data, !is.na(steps), c(interval, steps)) %>% group_by(interval) %>% summarise(avg_steps = mean(steps))

with(int_avg_steps, plot(interval, avg_steps, type = "l"))

max_steps <- filter(int_avg_steps, avg_steps == max(avg_steps))$interval

#The 5-minute interval that, on average, contains the maximum number of steps:

data_missing <- subset(data, is.na(steps))
data_not_missing <- subset(data, !is.na(steps))

missing_values <- data_missing %>% summarise(missing = n())

#Total number of missing values in the dataset

#Impute missing step values by using the average steps by interval

data_missing_fix <- merge(data_missing, int_avg_steps, by.x="interval", by.y="interval") %>% select(-steps) %>% rename(steps=avg_steps)

data_fix <- rbind(data_missing_fix, data_not_missing)

sum_daily_steps_fix <- subset(data_fix, !is.na(steps), c(date, steps)) %>% group_by(date) %>% summarise(steps = sum(steps))

with(sum_daily_steps_fix, hist(steps))

mean_daily_steps_fix <- sum_daily_steps_fix %>% summarise(steps = mean(steps))
median_daily_steps_fix <- sum_daily_steps_fix %>% summarise(steps = median(steps))

#Do these values differ from the estimates from the first part of the assignment?
#What is the impact of imputing missing data on the estimates of the total daily number of steps?

mean_daily_steps_fix$steps - mean_daily_steps$steps
median_daily_steps_fix$steps - median_daily_steps$steps

