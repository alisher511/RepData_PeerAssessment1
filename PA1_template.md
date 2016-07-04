---
title: "Reproducible Research Course Project 1"
author: "A. H."
date: "July 3, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Loading and preprocessing the data
```{r}
df <- read.csv("activity.csv")
head(df)
df$date <- as.Date(df$date)
```

Loading essential packages
```{r, echo=FALSE}
require(ggplot2)
require(dplyr)
```

### Total steps groupped by date
```{r}
x <- df %>% 
        group_by(date) %>% 
        summarise(total_steps = sum(steps))
head(x)
```
The third column reports the total number of steps taken by an individual in a given day

### Accompanying plot
```{r}
g <- ggplot(x, aes(x=total_steps))
g <- g + labs(title="Total number of steps taken per day", x =("total number of steps")) 
g <- g + geom_histogram(na.rm = TRUE, fill = "olivedrab", color = "black")
g
```

The mean and median of the steps taken per day
```{r}
mean(x$total_steps, na.rm = TRUE)
median(x$total_steps, na.rm = TRUE)
```
### Activity patterns
Average daily steps
```{r}
avg_steps <- tapply(df$steps, df$interval, FUN = mean, na.rm = TRUE)
head(avg_steps)
```

The formed array shows us the number of average daily steps taken in a specific interval
Create a dataframe of values that is going to help us construct the plot
```{r}
y <- data.frame(
        interval = as.numeric(names(avg_steps)), 
        steps = avg_steps, 
        row.names = NULL
        )
head(y)
```

Plot of average daily activity. 
```{r}
qplot(data = y, x=interval, y= steps, geom = "line", main = "Average Daily Activity")
```
We can clearly see a spike in the morning and very little activity during night hours
The maximum number of steps across all the days in the dataset are taken during 8:35 AM

```{r}
subset(y, steps == max(y$steps))
```

## Missing values
Total number of rows with NAs is **2304**

```{r}
table(is.na(df$steps))
```

```{r}
df2 <- replace(df$steps, list = which(is.na(df$steps) == TRUE), values = y$steps)
head(df2)
```

### Ceating a new dataset with the missing data filled in. 
Here, we use the average daily steps for each interval across all days
```{r}
df2 <- data.frame(date = df$date, interval = df$interval, steps = df2)
head(df2)
```

### A histogram of the total number of steps taken each day 
```{r}
x2 <- df2 %>% 
        group_by(date) %>% 
        summarise(total_steps = sum(steps))

g <- ggplot(x2, aes(x=total_steps))
g <- g + labs(title="Total number of steps taken per day", x =("total number of steps")) 
g <- g + geom_histogram(fill = "salmon2", color = "black")
g
```

### Mean and median values
```{r}
mean(x2$total_steps)
median(x2$total_steps)
```
It should not be surprising to see very similar values for mean & average steps because we specifically included the averaged values for each interval. We also see a higher frequency of occurence of the number of steps.

### Weekdays v. weekends
```{r}
df2$days <- weekdays(df2$date, abbreviate = TRUE)

df2$weekend <- as.factor(ifelse(df2$days == "Sat" | df2$days == "Sun", "weekend", "weekday"))
        
g <- ggplot(data = df2, aes(interval, steps, color = weekend))
g <- g + facet_wrap( ~ weekend, nrow = 2)
g <- g + stat_summary(fun.y="mean", geom="line", lwd = 1, aes(group=1))
g
```

There is a significant difference in the activity patterns based on the day of the week. Weekday activity occurs primarily during the morning hours of regular work week while much of weekend activity occures late in the morning and is more narrow ("relaxed"") with no unusual peaks.

###
##
#
