Reproducible Research: Peer Assessment 1
========================================================
## Loading and preprocessing the data


```r
data<-read.csv("activity.csv")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?


```r
dataset<-aggregate(steps ~ date, data, sum)
hist(dataset$steps,breaks=50,xlab="steps",main=NULL)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
sprintf("Mean : %f ", mean(dataset$steps))
```

```
## [1] "Mean : 10766.188679 "
```

```r
sprintf("Meadian : %d" , median(dataset$steps))
```

```
## [1] "Meadian : 10765"
```

## What is the average daily activity pattern?


```r
agg<-aggregate(.~interval, FUN=mean, data=data)
plot(agg$interval,agg$steps,type="l",xlab="interval",ylab="steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
sprintf("Max value interval: %d" , agg$interval[which.max(agg$steps)])
```

```
## [1] "Max value interval: 835"
```

## Imputing missing values

```r
sprintf("No. of NA values : %d" ,sum(is.na(data$steps)))
```

```
## [1] "No. of NA values : 2304"
```

```r
newdata<-data
newdata[is.na(newdata)]<-0
newdataset<-aggregate(steps ~ date, newdata, sum)
hist(newdataset$steps,breaks=50,xlab="steps",main=NULL)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
sprintf("Mean : %f ", mean(newdataset$steps))
```

```
## [1] "Mean : 9354.229508 "
```

```r
sprintf("Meadian : %d" , median(newdataset$steps))
```

```
## [1] "Meadian : 10395"
```

## Are there differences in activity patterns between weekdays and weekends?


```r
agg1<-newdata
agg1[is.na(agg1)]<-0
agg1$day<-weekdays(as.Date(agg1$date))
agg1$DAY<-"Weekday"
agg1$DAY[agg1$day %in% c("Saturday","Sunday")]<-"Weekend"
agg1<-transform(agg1,DAY=factor(DAY))
weekend<-subset(agg1,DAY=="Weekend")
weekday<-subset(agg1,DAY=="Weekday")
aggwn<-aggregate(steps ~ interval, FUN=mean, data=weekend)
aggwd<-aggregate(steps ~ interval, FUN=mean, data=weekday)

par(mfrow=c(2,1))
plot(aggwn$interval,aggwd$steps,type="l",main="Weekend",xlab="interval",ylab="steps")
plot(aggwd$interval,aggwn$steps,type="l",main="Weekday",xlab="interval",ylab="steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

