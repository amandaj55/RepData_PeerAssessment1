# Reproducible Research Project1
AJ  

Set the working directory and read in activity.csv


```r
setwd("~/Documents/Coursera/ReproducibleResearch/Project1")
 activity <- read.csv("activity.csv")
  library(ggplot2)
```


Calculate total steps and plot histogram


```r
## Calculate total steps per day
 TotalSteps <- tapply(activity$steps,activity$date,sum)
 
 ## Histogram of total steps per day
 hist(TotalSteps, breaks = 8, xlab = "Total Steps per day", main = "Histogram of total steps per day")
```

![](PA1_template_files/figure-html/total steps mean steps-1.png)

```r
 ## Calculate mean and median of total stpes
 MeanSteps <- mean(TotalSteps, na.rm = TRUE)
 MedianSteps <- median(TotalSteps, na.rm = TRUE)
```

Mean of total steps per day: 1.0766189\times 10^{4}  
Median of total steps per day: 10765  

Calculate and plot  mean steps per interval across all days  
Find the max mean steps per interval and the associated interval


```r
## Calculate and plot  mean steps per interval across all days.
  MeanStepsInterval <- aggregate(activity$steps,by=list(activity$interval),FUN=mean, na.rm = TRUE)
  colnames(MeanStepsInterval) <- c("Interval","MeanSteps") 
  plot(MeanStepsInterval$Interval,MeanStepsInterval$MeanSteps,xlab="Interval",ylab = "Number of steps",type="l", main ="Mean steps per interval across all days" ) 
```

![](PA1_template_files/figure-html/steps per interval-1.png)

```r
## Find the max mean steps per interval and the associated interval
  MaxMeanStepsInterval <- max(MeanStepsInterval$MeanSteps)
 
  IntervalMaxMeanSteps <- MeanStepsInterval[which(MeanStepsInterval$MeanSteps==MaxMeanStepsInterval),]
  IntervalMaxMeanStepsInterval <- IntervalMaxMeanSteps$Interval 
```

Interval with max mean steps across all days : 835  

Create new dataset with NA's replaced by mean steps per interval


```r
## Count the number of rows with NA's
 IsNACols <- colSums(is.na(activity))
NumOfNACols <- IsNACols[1]

## Create a new dataset containing original data
activityNoNAs <- activity

## Replace missing steps values with  mean of interval in new dataset
activityNoNAs[is.na(activityNoNAs[,1]), 1] <- aggregate(activityNoNAs$steps,by=list(activityNoNAs$interval),FUN=mean, na.rm = TRUE)$x

TotalStepsNoNas <- tapply(activityNoNAs$steps,activityNoNAs$date,sum)
```
Total number of rows with NA's: 2304  



```r
hist(TotalStepsNoNas, breaks = 8, xlab = "Total Steps per day", main = "Histogram of total steps per day NA's replaced")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
 MeanStepsNoNA <- mean(TotalStepsNoNas, na.rm = TRUE)
 MedianStepsNoNA <- median(TotalStepsNoNas, na.rm = TRUE)
```

Mean of total steps per day (NAs removed): 1.0766189\times 10^{4}    
Median of total steps per day (NAs removed): 1.0766189\times 10^{4}  

Indicate if weekday or weekend and calculate mean steps per interval for each subset


```r
## create variable containing day of the week corresponding to date
activityNoNAs$day <- weekdays(as.Date(activityNoNAs$date), abbr = TRUE)  

## Set weekend variable = weekend or weekday depending on day
activityNoNAs$weekend  <- grepl("S(at|un)", activityNoNAs$day)
activityNoNAs$weekend  <- gsub("TRUE","weekend",activityNoNAs$weekend )
activityNoNAs$weekend  <- gsub("FALSE","weekday",activityNoNAs$weekend )

## subset dataset into weekdays and weekend
weekday <- subset(activityNoNAs,weekend == "weekday")
weekend <- subset(activityNoNAs,weekend == "weekend")

## calculate mean steps for each interval over all weekdays and weekends
MeanStepsInterval$WeekdayMean <- aggregate(weekday$steps,by=list(weekday$interval),FUN=mean, na.rm = TRUE)
MeanStepsInterval$WeekendMean <- aggregate(weekend$steps,by=list(weekend$interval),FUN=mean, na.rm = TRUE)

## plot mean steps for each interval over all weekdays and weekends
par(mfrow = c(2,1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
plot(MeanStepsInterval$WeekdayMean,xlab="Interval",ylab = "Number of steps",type="l", main ="Mean of steps per interval over all weekdays" )
plot(MeanStepsInterval$WeekendMean,xlab="Interval",ylab = "Number of steps",type="l", main ="Mean of steps per interval over all weekends" ) 
```

![](PA1_template_files/figure-html/indicate if weekday or weekend and subset dataset accordingly; plot weekday and weekend mean steps-1.png)


