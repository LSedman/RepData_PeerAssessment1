# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# set correct working directory
#setwd("Documents/Learn/Online courses/DataScience/Reproducible research/week2/RepData_PeerAssessment1/")

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if (!file.exists("activity.zip") & !file.exists("activity.csv")) {
        download.file(url, destfile="activity.zip", method="curl")
}

if (!file.exists("activity.csv")){
        unzip("activity.zip")
}

act <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
perDay <- tapply(act$steps, act$date, sum)
hist(perDay, breaks=10, xlab="Number of steps taken per day", 
     main="Daily activity")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(perDay, na.rm=T)
```

```
## [1] 10766
```

```r
median(perDay, na.rm=T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
int <- aggregate(x = act$steps, by = list(act$interval), FUN = "mean", na.rm=T)
plot(int[,1], int[,2], type="l", main="Average daily activity pattern",
     xlab="Interval", ylab="Average number of steps taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

## Imputing missing values


```r
sum(is.na(act))
```

```
## [1] 2304
```

```r
# NA <- mean for that 5-min interval
n <- length(act[,1])
imputed <- vector()
length(imputed) <- n

for(i in 1:n) {
       if(is.na(act[i,1])) {
               imputed[i] <- subset(int[,2], int[,1]==act[i,3])
       } else {
               imputed[i] <- act[i,1]
       }
}

act[,1] <- imputed

perDay2 <- tapply(act$steps, act$date, sum)
hist(perDay2, breaks=10, xlab="Number of steps taken per day", 
     main="Daily activity")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(perDay2)
```

```
## [1] 10766
```

```r
median(perDay2)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

```r
act[,2] <- as.Date(act[,2])
for(i in 1:n) {
       if(weekdays(act[i,"date"])=="Saturday"|weekdays(act[i,"date"])=="Sunday"){
               act[i,"Weekd"] <- "weekend"
       } else {
               act[i,"Weekd"] <- "weekday"
       }
}

int2 <- aggregate(x = act$steps, by = list(act$interval, act$Weekd), FUN = "mean")
colnames(int2) <- c("interval", "Weekd", "steps")

library(lattice)
xyplot(steps ~ interval | Weekd, data = int2, layout=c(1,2), xlab="Interval", 
       ylab="Number of steps", type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
