---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
URL = "https://github.com/zzhandxy/RepData_PeerAssessment1/blob/master/activity.zip"
file <- "activity.zip"
if (!file.exists(file)) {
        download.file(URL, file, mode = "wb")
}
unzip(file)
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```
## [1] "English_United States.1252"
```


```r
group_Q2 <- group_by(data, date)
temp_Q2 <-summarise(group_Q2, Total = sum(all(is.na(steps))))
temp_Q2 <- filter(temp_Q2, Total == 0)
group_Q2 <- filter(group_Q2, date %in% temp_Q2$date)
sum_Q2 <- summarise(group_Q2, Total = sum(steps, na.rm = TRUE))
g_Q2 <- ggplot(sum_Q2, aes(Total)) 
g_Q2 + geom_histogram(bins = 25) + 
    labs(x = "Total number of steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
Mean_Q2 = mean(sum_Q2$Total)
Median_Q2 = median(sum_Q2$Total)
```
- The **mean** total number of steps taken per day is 10766.1886792.
- The **median** total number of steps taken per day is 10765.

## What is the average daily activity pattern?


```r
group_Q3 <- group_by(data, interval)
sum_Q3 <- summarise(group_Q3, Average = mean(steps, na.rm = TRUE))

g <- ggplot(sum_Q3, aes(interval, Average)) 
g + geom_line() + 
    labs(x = "Interval", y = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
Max_inv <- sum_Q3$interval[which(sum_Q3$Average == max(sum_Q3$Average))]
```
-On average across all the days in the dataset, the interval 835 contains the **maximum** number of steps.

## Imputing missing values

```r
num_na <- sum(is.na(data$steps))
```
There are 2304 missing values in the dataset.


```r
fill_na <- merge(data, sum_Q3, by="interval")
fill_na <- transform(fill_na, steps = ifelse(is.na(steps), Average, steps))
group_Q4 <- group_by(fill_na, date)
sum_Q4 <- summarise(group_Q4, Total = sum(steps, na.rm = TRUE))
f <- ggplot(sum_Q4, aes(Total))
f + geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
Mean_Q4 = mean(sum_Q4$Total)
Median_Q4 = median(sum_Q4$Total)
```
- The **mean** total number of steps taken per day is 10766.1886792.
- The **median** total number of steps taken per day is 10766.1886792.

Filling in all of the missing values with the mean for that day has almost no effect on mean and 

## Are there differences in activity patterns between weekdays and weekends?


```r
library(lubridate)
fill_na <- fill_na[, -4]
fill_na <- fill_na[order(fill_na$date),]
fill_na <-mutate(fill_na, day = ifelse(wday(date) %in% c(6, 7), "weekends", "weekdays"))
fill_na$day <- factor(fill_na$day)
group_Q5 <- group_by(fill_na, day, interval)
sum_Q5 <- summarise(group_Q5, Average = mean(steps, na.rm = TRUE))
g_Q5 <- ggplot(sum_Q5, aes(interval, Average))
g_Q5 + geom_line() +
    facet_grid(day~.) +
    labs(x = "interval", y = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
