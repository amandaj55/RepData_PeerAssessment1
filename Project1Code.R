setwd("~/Documents/Coursera/ReproducibleResearch/Project1")
 activity <- read.csv("activity.csv")

 
 ## MeanSteps <- tapply(activity$steps,activity$date,mean)
 ## or
## MeanSteps2 <- aggregate(activity$steps,by=list(activity$date),FUN=mean)
 ## colnames(MeanSteps2) <- c("Date","MeanSteps")
 ## MedianSteps2 <- aggregate(activity$steps,by=list(activity$date),FUN=median)
 
 
 ## Calculate total steps per day
 TotalSteps <- tapply(activity$steps,activity$date,sum)
 
 ## Histogram of total steps per day
 hist(TotalSteps, breaks = 8, xlab = "Total Steps per day", main = "Histogram of total steps per day")
 
 ## Calculate mean and median of total stpes
 MeanSteps <- mean(TotalSteps, na.rm = TRUE)
 MedianSteps <- median(TotalSteps, na.rm = TRUE)
 
 
 ## Calculate and plot  mean steps per interval across all days.
 MeanStepsInterval <- aggregate(activity$steps,by=list(activity$interval),FUN=mean, na.rm = TRUE)
 colnames(MeanStepsInterval) <- c("Interval","MeanSteps")
 plot.ts(MeanStepsInterval$MeanSteps,xlab="Interval",ylab = "Number of steps")
 ggplot(data = MeanStepsInterval,aes(Interval, MeanSteps)) + geom_line()
 plot(MeanStepsInterval$Interval,MeanStepsInterval$MeanSteps,xlab="Interval",ylab = "Number of steps",type="l", main ="Mean steps per interval across all days" ) 
 
 ## plot(MeanStepsInterval,xlab="Interval",ylab = "Number of steps")
 
 ## Find the max mean steps per interval and the associated interval
 MaxMeanStepsInterval <- max(MeanStepsInterval$MeanSteps)
 IntervalMaxMeanSteps <- MeanStepsInterval[which(MeanStepsInterval$MeanSteps==MaxMeanStepsInterval),]
 IntervalMaxMeanStepsInterval <- IntervalMaxMeanSteps$Interval
 
 ## Count the number of rows with NA's
 IsNACols <- colSums(is.na(activity))
 NumOfNACols <- IsNACols[1]
 
## Create a new dataset containing original data 
activityNoNAs <- activity
 
## Assign mean of interval to Steps variable if NA
activityNoNAs[is.na(activityNoNAs[,1]), 1] <- aggregate(activityNoNAs$steps,by=list(activityNoNAs$interval),FUN=mean, na.rm = TRUE)$x

TotalStepsNoNas <- tapply(activityNoNAs$steps,activityNoNAs$date,sum)
hist(TotalStepsNoNas, breaks = 8, xlab = "Total Steps per day", main = "Histogram of total steps per day NA's replaced")
MeanStepsNoNA <- mean(TotalStepsNoNas, na.rm = TRUE)
MedianStepsNoNA <- median(TotalStepsNoNas, na.rm = TRUE)

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