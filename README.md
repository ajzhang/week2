---
title: "Week2 Assignment of Reproducible Research"
output: html_document
date: "2023-02-28"
---

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a **single R markdown** document that can be processed by **knitr** and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use <span style="color:red">echo = TRUE</span>
 so that someone else will be able to read the code. **This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.**

* set global echo=TRUE
```{r global_setting}
knitr::opts_chunk$set(echo=TRUE)
```

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.


**Loading and preprocessing the data**  
Show any code that is needed to

1. Load the data (i.e. <span style="color:red">read.csv()</span>)
```{r load_data, echo=TRUE}
activityData <- read.csv(file="activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis
```{r transform_data, echo=TRUE}
activityData$date <-as.Date(activityData$date, format="%Y-%m-%d")
```


**What is mean total number of steps taken per day?**  
For this part of the assignment, you can ignore the missing values in the dataset.  
  
1.Calculate the total number of steps taken per day
```{r total_steps, echo=TRUE}
AggregatedData<-aggregate(activityData$steps, list(activityData$date), sum, na.rm=TRUE)
colnames(AggregatedData) <- c("date", "steps")
```
  
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
```{r histo_plot, echo=TRUE}
hist(AggregatedData$steps,breaks=10, main="Histogram of total steps per day", xlab="Total steps per day", ylab="Frequency")
```

3. Calculate and report the mean and median of the total number of steps taken per day
```{r mean_per_day}
mean_per_day<-mean(AggregatedData$steps, na.rm=TRUE)
```
Mean steps per day is mean_per_day = `r mean_per_day`.

```{r median}
median_per_day<-median(AggregatedData$steps, na.rm=TRUE)
```
Median steps per day is median_per_day = `r median_per_day`.


**What is the average daily activity pattern?**  
1. Make a time series plot (i.e. <span style="color:red">type = "l"</span>) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
```{r time_series_plot}
timeseries_steps<-aggregate(activityData$steps, list(activityData$interval), mean, na.rm=TRUE)
colnames(timeseries_steps) <- c("interval", "steps")
plot(timeseries_steps$interval,timeseries_steps$steps, type="l", xlab="5-minute Interval", ylab="Mean of steps")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
timeseries_steps$interval[which.max(timeseries_steps$steps)]
```


**Imputing missing values**  
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 
NAs)
```{r}
sum(is.na(activityData$steps) == TRUE)
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**strategy:** add a new column "fixed_steps", the values equal to the column "steps" and the missing vales replaced with the rounded average steps of the 5-min interval

```{r}
activityData$fixed_steps <- ifelse(is.na(activityData$steps), round(timeseries_steps$steps[match(activityData$interval, timeseries_steps$interval)], 0), activityData$steps)
head(activityData, n=20)
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r new_dataset}
new_dataset<-data.frame(steps=activityData$fixed_steps, date=activityData$date, interval=activityData$interval)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
fixed_AggregatedData<-aggregate(new_dataset$steps, list(new_dataset$date), sum, na.rm=TRUE)
colnames(fixed_AggregatedData) <- c("date", "fixed_steps")
hist(fixed_AggregatedData$fixed_steps,breaks=10, main="Histogram of the total steps", xlab="steps (correced the missing steps)", ylab="Frequency")
```

Calculate and report the mean and median of the total number of steps taken per day after missing steps correction
```{r fixed_mean}
fixed_mean_per_day<-mean(fixed_AggregatedData$fixed_steps, na.rm=TRUE)
```
Mean steps per day is mean_per_day = `r fixed_mean_per_day`. (the difference with before correction is about `r round(fixed_mean_per_day - mean_per_day)` steps.)

```{r fixed_median}
fixed_median_per_day<-median(fixed_AggregatedData$fixed_steps, na.rm=TRUE)
```
Median steps per day is median_per_day = `r fixed_median_per_day`.(the differnce with before correction is about `r round(fixed_median_per_day - median_per_day)` steps.)


**Are there differences in activity patterns between weekdays and weekends?**  
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
library(chron)
packageVersion("chron") 
new_dataset$daytype <- ifelse(is.weekend(new_dataset$date),"weekend", "weekday")
daytype_interval<-aggregate(steps ~ interval + daytype, new_dataset, mean, na.rm=TRUE)
```

2. Make a panel plot containing a time series plot (i.e. <span style="color:red">type = "l"</span>) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r}
library(ggplot2)
ggplot(daytype_interval, aes(interval, steps)) + 
  geom_line() + xlab("Interval") + ylab("Mean of steps") + facet_grid(daytype ~ .)
```



