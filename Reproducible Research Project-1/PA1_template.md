---
title: "Reproducible Research Project 1"
author: "Ramaranjan"
date: "Monday, June 08, 2015"
output: html_document
---

# Reproducible Research Project - 1

- ## Loading and preprocessing the data


```r
library(dplyr)
library(ggplot2)
setwd("C:/Users/rruj/Downloads")
data <- read.csv("activity.csv")
```

- ## What is mean total number of steps taken per day?


```r
# Transforming the dataset to a desired format
raw <- tbl_df(data)
by_date <- group_by(raw, date)
steps_by_date <- summarize(by_date, sum(steps))
colnames(steps_by_date)[2] <- "steps"

#Plotting the graph using the ggplot2 package
g <- ggplot(steps_by_date, aes(x=steps))
g <- g + geom_histogram(binwidth=2000, color="red")
g <- g + labs(x="Sum of steps", title = "Distribution of steps")
g
```

<img src="figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

```r
mean_steps <- round(with(steps_by_date, mean(steps, na.rm=T)),2)
median_steps <- with(steps_by_date, median(steps, na.rm=T))
```

- *Mean Steps* : The mean steps taken each day is 1.076619 &times; 10<sup>4</sup>

- *Median Steps* : The median steps taken each day is 10765


```r
#Plotting the time-series graph for 5-minute interval averaged across all days
by_interval <- group_by(raw, interval)
steps_by_interval <- summarize(by_interval, mean(steps, na.rm = TRUE))
colnames(steps_by_interval)[2] <- "steps"
g2 <- ggplot(steps_by_interval, aes(x=interval, y=steps))
g2 <- g2 + geom_line()
g2 <- g2 + labs(x="5-minute interval", y="Average step count", title="Time Distribution of step count")

max_steps <- steps_by_interval[with(steps_by_interval, which.max(steps)), ]$interval

g2 <- g2 + geom_vline(xintercept=max_steps, col="red")
g2 <- g2 + geom_text(aes(x=max_steps, label="Max steps interval", y=100), angle=90, vjust=1.2, text=element_text(size=30))
g2
```

<img src="figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />

- Maximum steps are taken on the interval 835

- ## Are there differences in activity patterns between weekdays and weekends?

```r
# Adding a column for weektime
data$day <- with(data, weekdays(as.Date(date, format="%Y-%m-%d")))
data$weektime <- with(data, as.factor(ifelse(day %in% c("Sunday","Saturday"),"Weekend","Weekday")))
data <- data[,c(1:3,5)]

#Transforming the dataset
raw2 <- tbl_df(data)
by_weektime_interval <- group_by(raw2, weektime, interval)
avg_steps <- summarize(by_weektime_interval, mean(steps, na.rm = T))
colnames(avg_steps)[3] <- "Average.Steps"

# Plotting the activity patterns between weekends and weekdays
g3 <- ggplot(avg_steps, aes(x=interval, y=Average.Steps))
g3 <- g3 + geom_line()
g3 <- g3 + facet_wrap( ~ weektime)
g3
```

<img src="figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />

```r
# Mean of steps in weekdays
mean(avg_steps$Average.Steps[avg_steps$weektime == "Weekday"], na.rm = T)
```

```
## [1] 35.33796
```

```r
# Mean of steps in weekends
mean(avg_steps$Average.Steps[avg_steps$weektime == "Weekend"], na.rm = T)
```

```
## [1] 43.07837
```

### *Conslusion : It can be clearly observed from the graphs and the averages that more steps are taken during the weekends (43.07 mean steps) than the weekdays (35.33 mean steps)*
