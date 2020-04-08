---
title: "Reproducible Reasearch Project 1"
output: html_document
---



## Reproducible Research Project 1 Submission
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken
      

#Reading Dataset

```r
setwd("F:/Career Prep/Data Science Specialization")
data<- read.csv("activity.csv") 
str(data)
```

'data.frame':	17568 obs. of  3 variables:
 $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ interval: int  0 5 10 15 20 25 30 35 40 45 ...


Let's reformat the Date column to a date format.

```r
data$date <- as.Date(data$date, "%Y-%m-%d")
str(data)
```

'data.frame':	17568 obs. of  3 variables:
 $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ date    : Date, format: "2012-10-01" "2012-10-01" ...
 $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

#What is mean total number of steps taken per day?

Now, let's calculate the total number of steps taken each day and plot the histogram.

```r
total <- tapply(data[is.na(data$steps)==FALSE,]$steps, data[is.na(data$steps)==FALSE,]$date, FUN = sum)
hist(total)
```

![](/images/hist1.png)

Now, let's calculate the mean and median values for the total number of steps taken per day.

```r
mea<- mean(total)
med<- median(total)
```
Mean is `mea`  
Median is `med` 

#What is the average daily activity pattern?

Now, we've to make a time series plotof the 5-minute interval and the average number of steps taken, averaged across all days. Let's aggregate them across the intervals, find the average and plot. I'll be disregarding intervals where the data is unavailable.



```r
t <- data.frame("Interval" = sort(unique(data$interval)),
                "Mean" = tapply( X = data[is.na(data$steps)==FALSE,]$steps,
              data[is.na(data$steps)==FALSE,]$interval,
              FUN =mean))

plot(t$Interval, t$Mean, 'l', xlab = "Interval", ylab = "Mean Steps")
```

![](/images/ts.png)

To find the interval with the maximum mean number of steps, we order them into place.

```r
head(order(t$Mean, decreasing = TRUE))
```

[1] 104 105 107 106 103 101


#Imputing missing values
Let's find the number of rows with NAs in them.


```r
sum(apply(data, 1, anyNA))
```

[1] 2304
There are 2304 rows with NA values in them.
For imputing missing values, let's replace the missing values with the mean number of steps for that specific interval.


```r
data.i <- data.frame("date" = as.character(), "interval" = as.numeric(), "steps" = as.numeric())
for(i in 1:nrow(data)){
  int <- data[i, "interval"]
  st <- data[i, "steps"]
  if(is.na(st)==TRUE){
     a.i <- data.frame("date" = data[i, "date"], "interval" = int, "steps" = t[t$Interval==int,]$Mean) 
  }
  else{
    a.i <- data.frame("date" = data[i, "date"], "interval" = int, "steps" = st) 
  }
  data.i <- rbind(data.i, a.i)
  }
```


Now let's make a histogram using the new dataset and also calculate the mean and median.


```r
total <- tapply(data.i$steps, data.i$date, FUN = sum)
hist(total)
```

![](/images/hist2.png)

```r
mea<- mean(total)
med<- median(total)
print(mea)
```

[1] 10766.19

```r
print(med)
```

[1] 10766.19
New mean is `mea`  
New median is `med`


#Are there differences in activity patterns between weekdays and weekends?

```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
data.i$wDay <- factor((weekdays(data.i$date) %in% weekdays1), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
data.i$day <- weekdays(data.i$date)

par(mfrow = c(2,1))
par(fig=c(0,1,0.34,1), new=TRUE)
plot(unique(data.i[levels(data.i$wDay) == c('weekday'),]$interval)
, tapply(data.i[levels(data.i$wDay) == c('weekday'),]$steps,
                 data.i[ levels(data.i$wDay) == c('weekday'),]$interval,
                 FUN = mean),
     'l', xlab = '', ylab = '', main = "Weekday(Top) and Weekend(Bottom) Mean Steps Per Interval", axes = FALSE)
Axis(side=1, labels=FALSE)
Axis(side=2, labels=TRUE)
par(fig=c(0,1,0,0.66), new=TRUE)

plot(unique(data.i[levels(data.i$wDay) == c('weekend'),]$interval)
, tapply(data.i[levels(data.i$wDay) == c('weekend'),]$steps,
                 data.i[ levels(data.i$wDay) == c('weekend'),]$interval,
                 FUN = mean),
     'l', xlab = 'Interval', ylab = 'Mean Number of Steps', main = "")
```

![](/images/final.png)





Now let's knit the Markdown document into an HTML document.

