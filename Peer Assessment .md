Peer Assessment 1
=====================
Loading and preprocessing the data


1 Load the data (i.e. read.csv()).

2 Process/transform the data (if necessary) into a format suitable for your analysis.



```r
setwd("~/Desktop/Coursera/Reproducible Research/Peer Assessment 1")
data<-read.csv("activity.csv")
```

# What is mean total number of steps taken per day?

All missing values in the dataset are ignored.

1 Make a histogram of the total number of steps taken each day


```r
Dailysteps<-tapply(data$steps,data$date,sum)
hist(Dailysteps)
```

![plot of chunk DailystepHistogram](figure/DailystepHistogram.png) 

2 Calculate and report the mean and median total number of steps taken per day

```r
Mean<-tapply(data$steps,data$date,mean)
Median<-tapply(data$steps,as.character(data$date),median)
Mean
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA     0.4375    39.4167    42.0694    46.1597    53.5417 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##    38.2465         NA    44.4826    34.3750    35.7778    60.3542 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##    43.1458    52.4236    35.2049    52.3750    46.7083    34.9167 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##    41.0729    36.0938    30.6285    46.7361    30.9653    29.0104 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##     8.6528    23.5347    35.1354    39.7847    17.4236    34.0938 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##    53.5208         NA    36.8056    36.7049         NA    36.2465 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    28.9375    44.7326    11.1771         NA         NA    43.7778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##    37.3785    25.4722         NA     0.1424    18.8924    49.7882 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##    52.4653    30.6979    15.5278    44.3993    70.9271    73.5903 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##    50.2708    41.0903    38.7569    47.3819    35.3576    24.4688 
## 2012-11-30 
##         NA
```

```r
Median
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA          0          0          0          0          0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##          0         NA          0          0          0          0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##          0          0          0          0          0          0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##          0          0          0          0          0          0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##          0         NA          0          0         NA          0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0         NA         NA          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##          0          0         NA          0          0          0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##          0          0          0          0          0          0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0          0 
## 2012-11-30 
##         NA
```

## What is the average daily activity pattern?

1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data1<-data[order(data$interval),]
data2<-na.omit(data1)
NoOfinterval<-numeric(0)
sum<-numeric(0)
interval<-unique(data2$interval)
for (i in interval) {
        intervaldataset<-subset(data2,data2$interval==i)
        NoOfinterval<-c(NoOfinterval,nrow(intervaldataset))
        sum<-c(sum,sum(intervaldataset$steps))
}
interval_mean<-sum/NoOfinterval
plot(interval,interval_mean,type="l")
```

![plot of chunk TimeSeriesPlot](figure/TimeSeriesPlot.png) 

2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalmeanchar<-as.character(interval_mean)
maxno<-grep(as.character(max(interval_mean,na.rm=T)),intervalmeanchar)
Max_5minuteInterval<-interval[[maxno]] ###### This is the value of the Max_5-minuteInterval
Max_5minuteInterval
```

```
## [1] 835
```

### Imputing missing values

1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalrow<-nrow(data)
completedrow<-nrow(na.omit(data))
NAsrow<-totalrow-completedrow  ##### This is the total number of rows with NAs. 
NAsrow
```

```
## [1] 2304
```

2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
table<-data.frame(rep(interval_mean,61))  ##### Replicate the interval means for 61 times, 
                                          ##### because there are 61 days in the dataset. 
colnames(table)<-c("mean")

data_new<-cbind(data,table)

data_new$label<-is.na(data_new$steps) ##### Create a factor "is.na"" label 
colnames<-colnames(data_new)
data_split<-split(data_new,data_new$label) ##### Split the dataset into two datasets 
                                           ##### based on "is.na" label 
data_split1<-data.frame(data_split[1])
colnames(data_split1)<-colnames
data_split2<-data.frame(data_split[2])
colnames(data_split2)<-colnames
data_split2[,1]<-data_split2[,4]
data_new<-rbind(data_split1,data_split2)
data_new<-data_new[,1:3]
data_new<-data_new[order(data_new$date),]

############repeat the steps in Section "What is mean total number of steps taken per day?"
Dailysteps_new<-tapply(data_new$steps,data_new$date,sum)
hist(Dailysteps_new)
```

![plot of chunk dataset with the mean for that 5-minute interval filling for NA](figure/dataset with the mean for that 5-minute interval filling for NA.png) 

3 Calculate and report the mean and median total number of steps taken per day in the new dataset

```r
Mean<-tapply(data_new$steps,data_new$date,mean)
Median<-tapply(data_new$steps,as.character(data_new$date),median)
Mean
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##    37.3826     0.4375    39.4167    42.0694    46.1597    53.5417 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##    38.2465    37.3826    44.4826    34.3750    35.7778    60.3542 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##    43.1458    52.4236    35.2049    52.3750    46.7083    34.9167 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##    41.0729    36.0938    30.6285    46.7361    30.9653    29.0104 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##     8.6528    23.5347    35.1354    39.7847    17.4236    34.0938 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##    53.5208    37.3826    36.8056    36.7049    37.3826    36.2465 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    28.9375    44.7326    11.1771    37.3826    37.3826    43.7778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##    37.3785    25.4722    37.3826     0.1424    18.8924    49.7882 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##    52.4653    30.6979    15.5278    44.3993    70.9271    73.5903 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##    50.2708    41.0903    38.7569    47.3819    35.3576    24.4688 
## 2012-11-30 
##    37.3826
```

```r
Median
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      34.11       0.00       0.00       0.00       0.00       0.00 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##       0.00      34.11       0.00       0.00       0.00       0.00 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##       0.00      34.11       0.00       0.00      34.11       0.00 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       0.00       0.00       0.00      34.11      34.11       0.00 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##       0.00       0.00      34.11       0.00       0.00       0.00 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##       0.00       0.00       0.00       0.00       0.00       0.00 
## 2012-11-30 
##      34.11
```

#### Are there differences in activity patterns between weekdays and weekends?

1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data_new$date<-strptime(as.character(data_new$date),"%Y-%m-%d")
data_new$weekday<-weekdays(data_new$date)
data_new$week<-ifelse(data_new$weekday %in% c("Satuday", "Sunday"),
       data_new$week <-"Weekend", data_new$week <-"Weekday")
```


2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
data1<-data_new[order(data_new$interval),]
NoOfinterval<-numeric(0)
sum<-numeric(0)
interval<-unique(data1$interval)
for (i in interval) {
        intervaldataset<-subset(data1,data1$interval==i)
        NoOfinterval<-c(NoOfinterval,nrow(intervaldataset))
        sum<-c(sum,sum(intervaldataset$steps))
}
data1$mean<-sum/NoOfinterval
data_weekday<-subset(data1,data1$week=="Weekday")
data_weekend<-subset(data1,data1$week=="Weekend")
par(mar=c(4,4,0.3,0.3), mfcol=c(2,1))
plot(data_weekend$interval,data_weekend$mean,type="l",ylab="Number fo steps")
plot(data_weekday$interval,data_weekday$mean,type="l",ylab="Number fo steps")
```

![plot of chunk New TimeSeriesPlot](figure/New TimeSeriesPlot.png) 
