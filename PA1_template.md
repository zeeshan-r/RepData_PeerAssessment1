<style type="text/css">
h3 { /* Header 3 */
  font-size: 18px;
  font-family: "Times New Roman", Times, serif;
  color: DarkBlue;
}
</style>

### Loading and preprocessing the data

    library( ggplot2)
    df=read.csv("./data/repdata_data_activity/activity.csv")

    str(df)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    df$date=as.POSIXct(df$date)

### What is mean total number of steps taken per day?

    steps_daily=aggregate(df$steps~df$date,FUN = sum)
    colnames(steps_daily)=c("Date","Steps")


    ggplot(steps_daily,aes(x=Steps))+
      geom_histogram()+
      ggtitle("Steps Taken Daily")+
      xlab("Steps")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

### Calculate and report the mean and median of the total number of steps taken per day

    mean_old=mean(steps_daily$Steps,na.rm = T)
    median_old=median(steps_daily$Steps,na.rm = T)
    print(paste("Mean is:",mean_old,"&  Median is:",median_old))

    ## [1] "Mean is: 10766.1886792453 &  Median is: 10765"

### What is the average daily activity pattern?

    meansteps=aggregate(df$steps~df$interval,FUN = mean)
    colnames(meansteps)=c("Interval","Steps")

    ggplot(meansteps,aes(x=Interval,y=Steps))+
      geom_line()+
      ggtitle("Average Daily Activity")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### Total missing values

    missingvalues = nrow(df[is.na(df$steps),])
    missingvalues

    ## [1] 2304

### Devise a strategy for filling in all of the missing values in the dataset

    df_filled <- transform(df,steps = ifelse(is.na(df$steps),
                              meansteps$Steps[match(df$interval, 
                                                  meansteps$Interval)],
                                                 df$steps))

    df_filled_agg <- aggregate(df_filled$steps~df_filled$date, FUN=sum)
    colnames(df_filled_agg)=c("Date","Steps")

    hist(df_filled_agg$Steps,
         main = "Number of Steps Per Day",
         xlab = "Number of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

### Calculate and report the mean and median total number of steps taken per day.

    mean_new=mean(df_filled_agg$Steps,na.rm = T)
    median_new=median(df_filled_agg$Steps,na.rm = T)
    print(paste("Mean is:",mean_new,"&  Median is:",median_new))

    ## [1] "Mean is: 10766.1886792453 &  Median is: 10766.1886792453"

### Do these values differ from the estimates from the first part of the assignment?

    print(paste("Difference in mean:",mean_new-mean_old))

    ## [1] "Difference in mean: 0"

    print(paste("Difference in median:",median_new-median_old))

    ## [1] "Difference in median: 1.1886792452824"

### Are there differences in activity patterns between weekdays and weekends?

    # Let's create defferciate bw weekday and weekend
    Type_of_day <- function(date) {
      day_name <- weekdays(date)
      if (day_name %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
          return ("Weekday")
      else if (day_name %in% c('Saturday', 'Sunday'))
          return ("Weekend")
    }

    df_filled$day <- sapply(df_filled$date, FUN = Type_of_day)

### Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

    stepsbyDaytype <- aggregate(steps ~ interval + day, df_filled, mean)
    ggplot(data = stepsbyDaytype, aes(x = interval, y = steps)) + 
      geom_line() +
      facet_grid(day ~ .) +
      
      xlab("Interval") +
      ylab("Number of Steps") +
      theme(plot.title = element_text(hjust = 0.5))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)
