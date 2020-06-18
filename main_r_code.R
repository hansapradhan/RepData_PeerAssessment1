##1.Reading the dataset
## ----------------------------------------------------------------
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")

##2.Histogram of the total number of steps taken each day
##-----------------------------------------------------------------
library(ggplot2)
Q2<-data.frame(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
Q2$date<-rownames(Q2)
rownames(Q2)<-NULL
names(Q2)[[1]]<-"Total Steps"
png("plot1.png")
#Total Steps by date bar chart
ggplot(Q2,aes(y=Q2$`Total Steps`,x=Q2$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
dev.off()
ggplot(Q2,aes(y=Q2$`Total Steps`,x=Q2$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
#Histogram of total steps
qplot(Q2$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
png("plot1.1.png")
qplot(Q2$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
dev.off()

##3.Mean and median number of steps taken each day
##---------------------------------------------------------
library(dplyr)
Q3<-data.frame(round(tapply(activity$steps,activity$date,mean,na.rm=TRUE),2))
Q3$date<-rownames(Q3)
rownames(Q3)<-NULL
names(Q3)[[1]]<-"Mean Steps"
temp<-activity%>%select(date,steps) %>% group_by(date) %>% summarise(median(steps))
names(temp)[[2]]<-"Median Steps"
Q3$median<-temp$`Median Steps`
Q3<-Q3 %>% select(date,`Mean Steps`,median)

##4.Average number of steps time series graph
## ------------------------------------------------------------------------
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
png("plot2.png")
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
dev.off()
##5.The 5-minute interval that, on average, contains the maximum number of steps
## ------------------------------------------------------------------------
averages[which.max(averages$steps),]

##6.1How many missing
## --------------------------------------------------------
missing <- is.na(data$steps)
# How many missing
table(missing)

##6.2Imputing the data
## ------------------------------------------------------------------------
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
##7.steps each day for data after being imputed 
## ------------------------------------------------------------------------
total.steps <- tapply(filled.data$steps, filled.data$date, FUN="sum")
png("plot3.png")
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
dev.off()
mean(total.steps)
median(total.steps)

##8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## ------------------------------------------------------------------------
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)


averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
png("plot4.png")
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
dev.off()