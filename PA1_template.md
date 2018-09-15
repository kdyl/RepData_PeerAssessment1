---
title: "Rmarkdown Assignment"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



## Loading and preprocessing the data


```{r}
activity <- read.csv("activity.csv", header=TRUE)
activity$date <- as.Date(as.character(activity$date),"%Y-%m-%d")
```



## What is mean total number of steps taken per day?

Total number of steps taken per day:
```{r total}
na <- is.na(activity$steps)
act_sum <- tapply(activity$steps[!na], activity$date[!na], sum)
```


Histogram of the total number of steps taken each day:
```{r histogram}
hist(act_sum, breaks=8, xlab = "Number of steps", main = "Total number of steps each day")
```

Mean of the total number of steps taken per day is:
```{r mean}
mean(act_sum)
```

while median:
```{r median}
median(act_sum)
```




## What is the average daily activity pattern?
```{r daily activity pattern}
avg_interval <- tapply(activity$steps[!na], activity$interval[!na], mean)
plot(avg_interval, type="l", xaxt = 'n', xlab = "Interval", ylab = "Mean of steps",
     main = "Average daily activity per interval")
axis(1, at = seq(0,2355/5,by=10), labels=seq(0,2355, by = 50))
```


Interval which on average contains the maximum number of steps is:
``` {r max interval}
as.numeric(which(avg_interval==max(avg_interval))*5)
```




## Imputing missing values

Total number of missing values is:
``` {r NA}
sum(is.na(activity$steps))
```


Chosen strategy for imputing NA was to use exchange NA value with 5-minute interval mean
```{r imputing}
avg_int_full <- rep(avg_interval, times = 61)
act_not_na <- activity
for (i in 1:length(act_not_na$steps)) {
      if (is.na(act_not_na$steps[i])) {
        act_not_na$steps[i] = avg_int_full[i]
      }
}
```


Histogram of imputed dataset:
```{r calc for imputed data}
imput_sum <- tapply(act_not_na$steps, act_not_na$date, sum)
hist(imput_sum, breaks=8, xlab = "Number of steps", main = "Total number of steps each day (imputed data")
```


Mean of total number of steps from imputed dataset is:
``` {r imput mean}
mean(imput_sum)
```

While median:
``` {r imput median}
median(imput_sum)
```




## Are there differences in activity patterns between weekdays and weekends?

Grouping into weekdays/weekend:
```{r weekday}
act_week <- activity
# Sorry, but names of the week are in polish. It means monday, tuesday, wednesday, thursday, friday.
for (i in 1:length(act_week$date)) {
  if (weekdays(act_week$date[i]) %in% c("poniedzia³ek", "wtorek", "œroda", "czwartek", "pi¹tek")) {
          act_week$weekday[i] <- "weekday"
      }
  else {  act_week$weekday[i] <- "weekend" }
}
```


``` {r weekday plot}
week_mean <- aggregate(act_week$steps[!na], list(act_week$interval[!na],act_week$weekday[!na]), mean)

par(mfrow=c(2,1),mar=c(4, 4.5, 2, 0.5))

#weekday
plot(week_mean$Group.1[week_mean$Group.2=="weekday"],
     week_mean$x[week_mean$Group.2=="weekday"],
     type="l", xlab="", ylab = "Mean steps", main = "Weekdays")

#weekend
plot(week_mean$Group.1[week_mean$Group.2=="weekend"],
     week_mean$x[week_mean$Group.2=="weekend"],
     type="l", xlab = "Interval", ylab = "Mean steps", main = "Weekend")
```