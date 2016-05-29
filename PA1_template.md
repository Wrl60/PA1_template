Fitbit Activity Monitoring Data
-------------------------------

-----------------------------------------------------------------------------------
===================================================================================

#### Loading and preprocessing the data

    setwd("~/Coursera/Course 5 Rep/Project_1") #Setting the directory to correct location on computer. 

    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
    unzip("activity.zip")
    df<-read.table("activity.csv", sep=",", na.strings = "NA", header=T)

Data is downloaded into data frame called df. For reproducibility
purposes the source zip file and the csv file are preserved in the
folder.

-----------------------------------------------------------------------------------
===================================================================================

#### What is mean total number of steps taken per day?

    totalsteps <-aggregate(steps ~ date, df, sum) #calulates the total number of steps taken per day
    hist(totalsteps$steps, main = paste("Histogram of Steps Taken per Day"))

![](PA1_template_files/figure-markdown_strict/What%20is%20mean%20total%20number%20of%20steps%20taken%20per%20day-1.png)<!-- -->

    summary(totalsteps$steps) #show mean and median of steps taken per day. 

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    8841   10760   10770   13290   21190

Note the histogram above and summary statistics contains missing data.

-----------------------------------------------------------------------------------
===================================================================================

#### What is the average daily activity pattern?

    timeinterval<-aggregate(steps ~ interval, df, mean)
    plot(timeinterval, type="l",main = paste("Avg of Steps Taken per 5-Minute Interval"), xlab="5 Minute Intervals")
    abline(h=mean(timeinterval$steps), col="blue", lty = 3)

![](PA1_template_files/figure-markdown_strict/What%20is%20the%20average%20daily%20activity%20pattern-1.png)<!-- -->

    timeinterval[which(timeinterval$steps == max(timeinterval$steps, na.rm = TRUE)), ]

    ##     interval    steps
    ## 104      835 206.1698

Above is the 5-minute interval, on average across all the days in the
dataset

-----------------------------------------------------------------------------------
===================================================================================

#### Imputing missing values, and see if histogram is different?

    totalNA <-sum(is.na(df$steps)) 
    paste("The total number of NA records in this dataset is", totalNA)

    ## [1] "The total number of NA records in this dataset is 2304"

    idf<-df #create a data frame to have imputed values

    ##the strategy for imputing missing data is to use the mean for the 5-minute interval
    idf$steps[which(is.na(idf))] <-timeinterval$steps #imputes the mean of the 5-minute interval for NA

    ## test to compare before and after data
    head(df)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    head(idf)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

    tail(df)

    ##       steps       date interval
    ## 17563    NA 2012-11-30     2330
    ## 17564    NA 2012-11-30     2335
    ## 17565    NA 2012-11-30     2340
    ## 17566    NA 2012-11-30     2345
    ## 17567    NA 2012-11-30     2350
    ## 17568    NA 2012-11-30     2355

    tail(idf)

    ##           steps       date interval
    ## 17563 2.6037736 2012-11-30     2330
    ## 17564 4.6981132 2012-11-30     2335
    ## 17565 3.3018868 2012-11-30     2340
    ## 17566 0.6415094 2012-11-30     2345
    ## 17567 0.2264151 2012-11-30     2350
    ## 17568 1.0754717 2012-11-30     2355

    itotalsteps <-aggregate(steps ~ date, idf, sum) #calulates the total number of steps taken per day
    hist(itotalsteps$steps, main = paste("Histogram of Steps Taken per Day"))

![](PA1_template_files/figure-markdown_strict/Imputing%20missing%20values-1.png)<!-- -->

    summary(itotalsteps$steps) #show mean and median of steps taken per day. 

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10770   10770   12810   21190

There is no difference with the histogram between the data with the NA's
and the data with the imputed interval.

-----------------------------------------------------------------------------------
===================================================================================

#### Are there differences in activity patterns between weekdays and weekends?

    library(chron)

    ## Warning: package 'chron' was built under R version 3.2.4

    library(lattice)

    ## Warning: package 'lattice' was built under R version 3.2.5

    idfw<-idf
    idfw$date <-as.Date(idfw$date) #Convert date from a factor to date format
    idfw$dayofwk<-weekdays(idfw$date)
    idfw$weekend<-is.weekend(idfw$date) #Creates binary result for wkday vs. wkend


    idfwf<-subset(idfw, idfw$weekend=="FALSE") ##this is the weeday data
    idwftimeinterval<-aggregate(steps ~ interval, idfwf, mean)
    idwftimeinterval$weekday<-"weekday"
    idfwt<-subset(idfw, idfw$weekend=="TRUE") ## this is the weekend data
    idwttimeinterval<-aggregate(steps ~ interval, idfwt, mean)
    idwttimeinterval$weekday<-"weekend"

    idwtticomb <-rbind(idwftimeinterval,idwttimeinterval)

    xyplot(steps ~ interval | weekday, data = idwtticomb,  main='Fitbit Weekday vs. Weekend', type = "l", xlab = "5 Second Intervals", 
        ylab = "Avg Number of steps", index.cond=list(c(2,1)),layout = c(1, 2))

![](PA1_template_files/figure-markdown_strict/Difference%20between%20weekdays%20and%20weekends-1.png)<!-- -->
