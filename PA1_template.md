# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
#Read the data and intialize packages and perform any transformations necessary
data <- read.csv(unz("activity.zip", "activity.csv"))
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 3.2.2
```

## What is mean total number of steps taken per day?


```r
#Filter NAs out and compute Sum per day
dataTotalSteps <- filter(data, steps != 'NA') %>%
        group_by(date) %>% 
        summarise(Total_Steps_Per_Day = sum(steps))

#Set binwidth by using the range function to find the range of your columns
plot <- ggplot(dataTotalSteps, aes(Total_Steps_Per_Day)) + geom_histogram(binwidth = 21194/30)
plot
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean and median are given in this summary:

```r
#You could compute it directly like this:
#mean(dataFiltered$Total_Steps_Per_Day)
#median(dataFiltered$Total_Steps_Per_Day)

#More elegantly
summary(dataTotalSteps$Total_Steps_Per_Day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?


```r
#Filter NAs out and compute avg per day
dataAverage <- filter(data, steps != 'NA') %>%
        group_by(interval) %>% 
        summarise(Average_Steps_Per_Day = mean(steps))

plot <- ggplot(dataAverage, aes(x = interval,y = Average_Steps_Per_Day)) + geom_line()
plot
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Largest average of steps per day

```r
#Largest average of steps per day
indicie.of.max <- which(dataAverage$Average_Steps_Per_Day == max(dataAverage$Average_Steps_Per_Day))
dataAverage[indicie.of.max,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval Average_Steps_Per_Day
## 1      835              206.1698
```

## Imputing missing values
The number of missing values is 

```r
#Imputing missing values
data[!complete.cases(data),] %>% nrow()
```

```
## [1] 2304
```
Fill in NA's with means of every interval from entire data set

```r
#Fill NA's with mean
Filled_NAs_Only <- left_join(x = data, y = dataAverage, by = "interval") %>%
                   filter(is.na(steps)) %>%
                   select(steps = Average_Steps_Per_Day, date, interval)
Non_NAs_Only <- filter(data, steps != 'NA')

Average_Approx_For_NAs <- rbind(Filled_NAs_Only, Non_NAs_Only) %>%
                          arrange(date)

dataTotalApprox <- group_by(Average_Approx_For_NAs,interval) %>% 
        summarise(Total_Steps_Per_DayApprox = sum(steps))

plot <- ggplot(dataTotalApprox, aes(x = interval,y = Total_Steps_Per_DayApprox)) + geom_line()
plot
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


```r
#Plot filtered NA's versus adjusted averaged NA's
#Filter NAs out and compute Sum per interval
dataTotalSteps <- filter(data, steps != 'NA') %>%
        group_by(interval) %>% 
        summarise(Total_Steps_Per_Day = sum(steps))

plot1 <- ggplot(dataTotalSteps, aes(x = interval,y = Total_Steps_Per_Day)) + geom_line()
plot2 <- ggplot(dataTotalApprox, aes(x = interval,y = Total_Steps_Per_DayApprox)) + geom_line()
grid.arrange(plot1, plot2)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

The mean and median compared are given in this summary:

```r
summary(dataTotalSteps$Total_Steps_Per_Day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   131.8  1808.0  1981.0  2800.0 10930.0
```

```r
summary(dataTotalApprox$Total_Steps_Per_Day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   151.6  2081.0  2280.0  3223.0 12580.0
```

The values clearly differ between the two data sets. The impact of using the average value for each interval for filling NAs is that the mean, median, and maximum number of steps taken increased compared to the dataset with the NAs filtered out.

## Are there differences in activity patterns between weekdays and weekends?


```r
#Find differences btw weekday and weekend step counts
Filled_NAs_Only <- left_join(x = data, y = dataAverage, by = "interval") %>%
                   filter(is.na(steps)) %>%
                   select(steps = Average_Steps_Per_Day, date, interval)
Filled_NAs_Only$date <- weekdays(as.Date(Filled_NAs_Only$date)) 

Non_NAs_Only <- filter(data, steps != 'NA')
Non_NAs_Only$date <- weekdays(as.Date(Non_NAs_Only$date)) 


Average_Approx_For_NAs <- rbind(Filled_NAs_Only, Non_NAs_Only) %>%
                          arrange(date)
Average_Approx_For_NAs$date <- sapply(Average_Approx_For_NAs$date, FUN = switch,
                               Monday = 'weekday',
                               Tuesday = 'weekday',
                               Wednesday = 'weekday',
                               Thursday = 'weekday',
                               Friday = 'weekday',
                               Saturday = 'weekend',
                               Sunday = 'weekend') 

Weekdays <- filter(Average_Approx_For_NAs, date == "weekday") %>%
            group_by(interval) %>%
            summarise(AverageSteps = mean(steps))
Weekends <- filter(Average_Approx_For_NAs, date == "weekend") %>%
            group_by(interval) %>%
            summarise(AverageSteps = mean(steps))

plot1 <- ggplot(Weekdays, aes(x = interval,y = AverageSteps)) + geom_line() + ggtitle("Weekdays")
plot2 <- ggplot(Weekends, aes(x = interval,y = AverageSteps)) + geom_line() + ggtitle("weekend")
grid.arrange(plot1, plot2)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 



