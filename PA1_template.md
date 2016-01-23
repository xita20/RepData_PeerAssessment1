# Reproducible Research: Peer Assessment 1
## *Preview the output: ctrl+shift+k*

## Part 1: Loading and preprocessing the data

```r
#Input dataset (i.e. read.csv())
df <- read.csv("C:\\Users\\njiang\\Documents\\GitHub\\RepData_PeerAssessment1\\activity.csv")

#Process/transform the data (if necessary) into a format suitable for your analysis
#Transition the date into date format
df$date <- as.Date(df$date)
```


## Part 2: What is mean total number of steps taken per day?

```r
#Histogram of the total number of steps taken each day
library(ggplot2)
total.steps.per.day <- aggregate(x = df$steps , by = list(df$date), FUN = sum ,na.rm=TRUE)
names(total.steps.per.day) <- c("date","steps")
histplot <- ggplot(total.steps.per.day,aes(x = steps)) +
            ggtitle("Histogram of daily steps") +
            xlab("Steps (binwidth 2000)") +
            geom_histogram(binwidth = 2000)
histplot
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)\

```r
#Mean total number of steps taken per day
mean(total.steps.per.day$steps , na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
#Median total number of steps taken per day
median(total.steps.per.day$steps , na.rm = TRUE)
```

```
## [1] 10395
```



## Part 3: What is the average daily activity pattern?

```r
#Time series plot of 5-minute interval and the average number of steps taken, averaged across all days
average.steps.by.interval  <- aggregate(x = df$steps , by = list(df$interval), FUN = mean ,na.rm=TRUE)
names(average.steps.by.interval) <- c("interval","steps")

avg.step.line <- ggplot(average.steps.by.interval,aes(interval,steps)) +
                 ggtitle("Time Series Plot of Average Steps by Interval") +
                 geom_line()
avg.step.line  
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\

```r
#The 5-min time interval contains the maximum number of steps?
average.steps.by.interval[which.max(average.steps.by.interval$steps),c("interval")]
```

```
## [1] 835
```




## Part 4: Imputing missing values

```r
#total number of missing values in the dataset
nrow(df[is.na(df$steps),])
```

```
## [1] 2304
```

```r
#Imputing missing step values with mean step at time interval
df.imputed <- merge(x = df, y = average.steps.by.interval, by = "interval", all.x = TRUE)
df.imputed[is.na(df.imputed$steps.x),c("steps.x")] <- df.imputed[is.na(df.imputed$steps.x),c("steps.y")]

#cleaning data
df.imputed$date <- as.Date(df.imputed$date)
df.imputed$date.x <- NULL
df.imputed$Group.1 <- NULL
df.imputed$steps <- df.imputed$steps.x
df.imputed$steps.x <- NULL #delete column
df.imputed$steps.y <- NULL #delete column

#Histogram with new dataframe
total.steps.by.day <- aggregate(x = df.imputed$steps , by = list(df.imputed$date), FUN = sum ,na.rm=TRUE)
names(total.steps.by.day) <- c("date","steps")
histplot <- ggplot(total.steps.by.day,aes(x = steps)) +
            ggtitle("Histogram of daily steps after Imputation") +
            xlab("Steps (binwidth 2000)") +
            geom_histogram(binwidth = 2000)
histplot 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

```r
#Mean total number of steps taken per day after imputation
mean(total.steps.by.day$steps , na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
#Median total number of steps taken per day after imputation
median(total.steps.by.day$steps , na.rm = TRUE)
```

```
## [1] 10766.19
```



## Part 5: Are there differences in activity patterns between weekdays and weekends?

```r
#Factor variable with two levels indicating a weekday or weekend.
df.imputed$weekday <- as.factor(ifelse(weekdays(df.imputed$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")) 

average.steps.by.interval.and.weekday  <- aggregate(x = df.imputed$steps , 
                                                    by = list(df.imputed$interval,df.imputed$weekday), FUN = mean ,na.rm=TRUE)
names(average.steps.by.interval.and.weekday) <- c("interval","weekday","steps")

#Panel time series plot of the 5-minute interval and the average number of steps taken 
#Averaged across all weekday days or weekend days.
avg.step.line <- ggplot(average.steps.by.interval.and.weekday,aes(interval,steps)) +
                 ggtitle("Time Series Plot of Average Steps by Interval after Imputation") +
                 facet_grid(. ~ weekday) +
                 geom_line(size = 1)
avg.step.line  
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)\

