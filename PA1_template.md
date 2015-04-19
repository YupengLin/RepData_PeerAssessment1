# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1 Read the csv data

```r
act <- read.csv("activity.csv",stringsAsFactors=FALSE)
```
2 Change date variable from string to date variable

```r
act$date <- as.Date(act$date,"%Y-%m-%d")
```
## What is mean total number of steps taken per day?

Please ignore the warning message from loading dplyr package and the table is displayed as following.
Mean and median step per day. I used the group_by function again.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(chron)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.3
```

```r
actDaily <- group_by(act,date)
stepDay <- summarize(actDaily,sum=sum(steps),mean=mean(steps,na.rm=TRUE),median=median(steps,na.rm=TRUE))
print(stepDay)
```

```
## Source: local data frame [61 x 4]
## 
##          date   sum     mean median
## 1  2012-10-01    NA      NaN     NA
## 2  2012-10-02   126  0.43750      0
## 3  2012-10-03 11352 39.41667      0
## 4  2012-10-04 12116 42.06944      0
## 5  2012-10-05 13294 46.15972      0
## 6  2012-10-06 15420 53.54167      0
## 7  2012-10-07 11015 38.24653      0
## 8  2012-10-08    NA      NaN     NA
## 9  2012-10-09 12811 44.48264      0
## 10 2012-10-10  9900 34.37500      0
## ..        ...   ...      ...    ...
```

Plot the histogram

```r
hist(stepDay$sum,xlab="step per day",main="histogram of step per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 





## What is the average daily activity pattern?

1 Make a time series plot

```r
interStat <- group_by(act,interval)
interMean <- summarize(interStat,mean=mean(steps,na.rm=TRUE))
plot(interMean$interval,interMean$mean,lty=1,type="l",xlab="Interval",ylab="Mean")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max <- max(interMean$mean)
for (i in 1:288) {
  if(interMean[i,2] == max){
    index <- i
  }
}
intervalMax <- interMean[index,1]
intervalMax
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```


## Imputing missing values

1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
 sum(is.na(act$steps))
```

```
## [1] 2304
```

2 Devise a strategy for filling in all of the missing values in the dataset.

I used the mean of all day to fill the missing values

3 Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
interStat <- group_by(act,interval)
interMean <- summarize(interStat,mean=mean(steps,na.rm=TRUE))

for (i in 1:dim(act)[1]){
  if(is.na(act[i,1])){
    interIndex <- act[i,3]
    act[i,1] <- interMean[[which(interMean$interval==interIndex),2]]
  }
}
```

4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day


```r
actDaily <- group_by(act,date)
```

```r
stepDay <- summarize(actDaily,sum=sum(steps),mean=mean(steps,na.rm=TRUE),median=median(steps,na.rm=TRUE))
hist(stepDay$sum,xlab="step per day",main="histogram of step per day Without NA")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

```r
print(stepDay)
```

```
## Source: local data frame [61 x 4]
## 
##          date      sum     mean   median
## 1  2012-10-01 10766.19 37.38260 34.11321
## 2  2012-10-02   126.00  0.43750  0.00000
## 3  2012-10-03 11352.00 39.41667  0.00000
## 4  2012-10-04 12116.00 42.06944  0.00000
## 5  2012-10-05 13294.00 46.15972  0.00000
## 6  2012-10-06 15420.00 53.54167  0.00000
## 7  2012-10-07 11015.00 38.24653  0.00000
## 8  2012-10-08 10766.19 37.38260 34.11321
## 9  2012-10-09 12811.00 44.48264  0.00000
## 10 2012-10-10  9900.00 34.37500  0.00000
## ..        ...      ...      ...      ...
```
From the histogram, we can see that the  histogram that the data in the middle range significantly increases due to the median-adding strategy.

## Are there differences in activity patterns between weekdays and weekends?

```r
weekend <- is.weekend(act$date)
act <- cbind(act,weekend)
act$weekend <- sub("FALSE","weekday",act$weekend)
act$weekend <- sub("TRUE","weekend",act$weekend)

st <- group_by(act,interval,weekend)
stSum <- summarize(st,sum=sum(steps))
stSum$weekend <- factor(stSum$weekend,labels=c("weekday","weekend"))
xyplot(sum~interval|weekend,data=stSum,lty=1,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Yes, there are different type of activity pattern, In the weekday, more activity is expected, especially in the morning.
