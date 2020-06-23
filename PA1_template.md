READING DATA
------------

    dat=read.csv('activity.csv')

TOTAL STEPS PER DAY
-------------------

    s1=split(dat,dat$date)
    totalsteps=as.data.frame(sapply(s1,function(x) sum(x$steps)))   #calculate sums
    names(totalsteps)='steps'
    print(totalsteps)       #show the total number of steps taken per day

    ##            steps
    ## 2012-10-01    NA
    ## 2012-10-02   126
    ## 2012-10-03 11352
    ## 2012-10-04 12116
    ## 2012-10-05 13294
    ## 2012-10-06 15420
    ## 2012-10-07 11015
    ## 2012-10-08    NA
    ## 2012-10-09 12811
    ## 2012-10-10  9900
    ## 2012-10-11 10304
    ## 2012-10-12 17382
    ## 2012-10-13 12426
    ## 2012-10-14 15098
    ## 2012-10-15 10139
    ## 2012-10-16 15084
    ## 2012-10-17 13452
    ## 2012-10-18 10056
    ## 2012-10-19 11829
    ## 2012-10-20 10395
    ## 2012-10-21  8821
    ## 2012-10-22 13460
    ## 2012-10-23  8918
    ## 2012-10-24  8355
    ## 2012-10-25  2492
    ## 2012-10-26  6778
    ## 2012-10-27 10119
    ## 2012-10-28 11458
    ## 2012-10-29  5018
    ## 2012-10-30  9819
    ## 2012-10-31 15414
    ## 2012-11-01    NA
    ## 2012-11-02 10600
    ## 2012-11-03 10571
    ## 2012-11-04    NA
    ## 2012-11-05 10439
    ## 2012-11-06  8334
    ## 2012-11-07 12883
    ## 2012-11-08  3219
    ## 2012-11-09    NA
    ## 2012-11-10    NA
    ## 2012-11-11 12608
    ## 2012-11-12 10765
    ## 2012-11-13  7336
    ## 2012-11-14    NA
    ## 2012-11-15    41
    ## 2012-11-16  5441
    ## 2012-11-17 14339
    ## 2012-11-18 15110
    ## 2012-11-19  8841
    ## 2012-11-20  4472
    ## 2012-11-21 12787
    ## 2012-11-22 20427
    ## 2012-11-23 21194
    ## 2012-11-24 14478
    ## 2012-11-25 11834
    ## 2012-11-26 11162
    ## 2012-11-27 13646
    ## 2012-11-28 10183
    ## 2012-11-29  7047
    ## 2012-11-30    NA

    hist(totalsteps$steps, main='Histogram of Daily Total Steps', xlab='steps')     #plot histogram

![](PA1_template_files/figure-markdown_strict/total_steps_per_day-1.png)

    summary(totalsteps)

    ##      steps      
    ##  Min.   :   41  
    ##  1st Qu.: 8841  
    ##  Median :10765  
    ##  Mean   :10766  
    ##  3rd Qu.:13294  
    ##  Max.   :21194  
    ##  NA's   :8

**As the data summary shows, the mean and the median of the total number
of steps taken per day are 10766 and 10765 respectively.**

AVERAGE DAILY ACTIVITY PATTERN
------------------------------

    s2=split(dat,dat$interval)
    stepsper5min=as.data.frame(sapply(s2,function(x) mean(x$steps,na.rm=TRUE)))     #drop NAs for means calculation
    names(stepsper5min)='steps'
    stepsper5min$interval=row.names(stepsper5min)
    with(stepsper5min,plot(interval,steps,type='l',main='Average Steps per 5 min')) #making the time series plot

![](PA1_template_files/figure-markdown_strict/average_daily_activity_pattern-1.png)

    stepsper5min$interval[which.max(stepsper5min$steps)]     #search for the maximum 5-minute interval

    ## [1] "835"

**So the 5-minute interval of 8:35-8:40 contains the maximum number of
steps.**

IMPUTING MISSING VALUES
-----------------------

    summary(dat)    #check NAs in the original data set

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

    #creating a new data set by imputing missing steps with the mean for that 5-minute interval
    newdat=dat
    for (i in 1:nrow(newdat)) {
         if(is.na(newdat$steps[i])==TRUE){
             index=which(stepsper5min$interval==newdat$interval[i])
             newdat$steps[i]=stepsper5min$steps[index]
         }
     }
    s3=split(newdat,newdat$date)
    newtotalsteps=as.data.frame(sapply(s3,function(x) sum(x$steps)))        #calculate sums
    names(newtotalsteps)='steps'
    hist(newtotalsteps$steps, main='New Histogram of Daily Total Steps', xlab='steps')      #plot histogram

![](PA1_template_files/figure-markdown_strict/imputing_missing_values-1.png)

    summary(newtotalsteps)

    ##      steps      
    ##  Min.   :   41  
    ##  1st Qu.: 9819  
    ##  Median :10766  
    ##  Mean   :10766  
    ##  3rd Qu.:12811  
    ##  Max.   :21194

**As the summary of the original data set shows, there are 2304 NAs only
in the ‘steps’ column, i.e. there are 2304 rows with NAs totally;**

**My imputing strategy is to use the mean steps for that 5-minute
interval;**

**As the summary of the new data set shows, the mean and the median of
the total number of steps taken per day are both 10766, of which the
median is 1 more than the original while the mean is the same;**

**As the two histograms shows, imputing missing data with the mean for
that 5\_minute interval gathers more data towards the middle of the
plot.**

DIFFERENCE BETWEEN WEEKDAYS AND WEEKENDS
----------------------------------------

    for (j in 1:nrow(newdat)) {
         if(weekdays(as.Date(newdat$date[j]))=='星期六'|weekdays(as.Date(newdat$date[j]))=='星期日'){
             newdat$day[j]='weekend'
         }
         else{newdat$day[j]='weekday'}
     }
    newdat$day=as.factor(newdat$day)        #create the new factor variable
    str(newdat)     #show the new factor variable

    ## 'data.frame':    17568 obs. of  4 variables:
    ##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ day     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...

    #calculate for weekdays and weekends respectively
    weekdaydat=subset(newdat,day=='weekday')
    s4=split(weekdaydat,weekdaydat$interval)
    weekdaysteps=as.data.frame(sapply(s4,function(x) mean(x$steps,na.rm=TRUE)))
    names(weekdaysteps)='steps'
    weekdaysteps$interval=row.names(weekdaysteps)
    weekdaysteps$day=rep('weekday',nrow(weekdaysteps))
    weekenddat=subset(newdat,day=='weekend')
    s5=split(weekenddat,weekenddat$interval)
    weekendsteps=as.data.frame(sapply(s5,function(x) mean(x$steps,na.rm=TRUE)))
    names(weekendsteps)='steps'
    weekendsteps$interval=row.names(weekendsteps)
    weekendsteps$day=rep('weekend',nrow(weekendsteps))
    steps=rbind(weekdaysteps,weekendsteps)  #combine the two dataframe together for ggplot
    library(ggplot2)
    ggplot(steps,aes(interval,steps,group=1))+geom_line()+facet_grid(day~.)+theme(axis.text.x  = element_blank())        #hide x scale for clarity

![](PA1_template_files/figure-markdown_strict/difference_between_weekdays_and_weekends-1.png)
