#Checking the working directory
getwd()

#Setting desired directory
setwd("C:/Users/nirma/Documents/GitHub/RepData_PeerAssessment1")

#Invoking Required Libraries and Resources
require(lubridate)
require(dplyr)
require(ggplot2)
library(scales)
require(ggthemes)
require(RColorBrewer)

#Reading the data file. Data have been already downloaded, 
#unzipped and saved in the local directory
mydata<-read.csv('activity.csv')

#Understanding the dataset
head(mydata)
summary(mydata)
class(mydata)
names(mydata)

#Checking how the data interact (dichotomous relation)
pairs(mydata, panel = points,
      horInd = 1:nc, verInd = 1:nc,
      lower.panel = panel, upper.panel = panel,
      diag.panel = NULL, text.panel = textPanel,
      label.pos = 0.5 + has.diag/3, line.main = 3,
      cex.labels = NULL, font.labels = 1,
      row1attop = TRUE, gap = 1, log = "",
      horOdd = !row1attop, verOdd = !row1attop)

#Setting date
mydata$date<-as.Date(mydata$date)

#1. Calculating total steps taken per day and making histogram
dailysteps<-aggregate(steps~date,mydata,FUN=sum, na.rm=TRUE)
par(bg='brown')
hist(dailysteps$steps,
     main="Histogram of Total Steps Per Day",
     xlab="Total Steps Per Day",
     ylab="Frequency of the Day",
     border="blue",
     col="orange",
     las=1)

#2. Calculating and reporting Mean and the Median of total steps per day
# Mean
dailymean<-mean(dailysteps$steps)
dailymean
#Median
dailymedian<-median(dailysteps$steps)
dailymedian

#3. What is the average daily activity pattern?
#a. making a time series plot (i.e., type="1")of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
#quickly checking dataset
names(mydata)
#Calculating Interval Steps
intervalsteps<-aggregate(steps~interval,data=mydata,mean,na.rm=TRUE)
#plotting the interval steps
par(bg='grey')
plot(steps~interval,data=intervalsteps, type='l',
     main="Average Interval Frequency",
     xlab="Average Steps",
     ylab="Interval Frequency",
          cex.lab=1,
          col="blue")
#b. Which 5-minute interval, on average across all the days in the dataset 
#contains the maximum number of steps
maxstepsinterval<-intervalsteps[which.max(intervalsteps$steps),]$interval
maxstepsinterval

#Dealing with the Missing Values
#a. Calculate and report the total missing values in the dataset
totalmissing<-sum(is.na(mydata$steps))
totalmissing
#b. Devise a strategy for filling in all missing values in the data set using mean. 

#Imputing NAs
NAcolumn<-ifelse(is.na(mydata$steps),round(intervalsteps$steps[match
                  (mydata$interval,intervalsteps$interval)],0),mydata$steps)
#New dataset 'imputeddata'
imputeddata<-data.frame(steps=NAcolumn,interval=mydata$interval,date=mydata$date)

#Quick checking
head(imputeddata,n=10)

#Making a histogram of the total steps per day and calculating and reporting the 
#mean and median
dailyimputed<-aggregate(imputeddata$steps,list(imputeddata$date),FUN=sum)
colnames(dailyimputed)<-c("Date","Steps")
#Plotting a histogram
hgram<-ggplot(dailyimputed,aes(Steps))
hgram+geom_histogram(boundary=0,binwidth = 2500,col="black",fill="orange")+
  ggtitle("Histogram of Steps Per Day")+xlab("Steps")+ylab("Frequency")+
  theme(plot.title=element_text(face="bold",size=12))+scale_x_continuous(breaks=
                  seq(0,25000,2500))+scale_y_continuous(breaks = seq(0,26,2))
#Calculate and report mean and median total number of steps per day. Do these
#values differ from the estimates fromt he first part of the assignment? What 
#is the impact of imputing missing data on the estimates of the total daily 
#number of steps?

#a.Daily average steps after imputation
mean(dailyimputed$Steps)
#b. Daily median steps after imputation
median(dailyimputed$Steps)

#Are there differnces in activity patterns between weekdays and weekends?
#a. Creating a new factor variable in dataset with two levels="weekday" and "weekend"
imputeddata$exactdate<-as.Date(imputeddata$date,format="%Y-%m-%d")
#New variable with weekday
imputeddata$weekday<-weekdays(imputeddata$exactdate)
#Introducing weekend
imputeddata$daytype<-ifelse(imputeddata$weekday=='Saturday'|
                              imputeddata$weekday=='Sunday','weekend','weekday')
#checking if it works
head(imputeddata, n=5)

#Creating two time series plot of the 5-minute interval (x) and the average number of steps taken averaged across weekday days or weekend days (y)
stepsPerTime<-aggregate(steps~interval+daytype,data=imputeddata,FUN=mean,
                        na.action = na.omit)
stepsPerTime$time<-intervalsteps$interval/100
#Drawing Line Plots
Lplot<-ggplot(stepsPerTime,aes(time,steps))
Lplot+geom_line(col="red")+ggtitle("Average Steps Per Time Interval:Weekdays vs.
  Weekends")+xlab("Time")+ylab("Steps")+theme(plot.title=element_text(
    face="bold",size=12))+facet_grid(daytype~.)

