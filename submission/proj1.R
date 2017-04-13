library(ggplot2)
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
projData <- read.csv(unz(temp, "activity.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
unlink(temp)

str(projData)
projData$date <- as.Date(data$date)
unique(projData$interval)

# What is mean total number of steps taken per day?
# 1. Make a histogram of the total number of steps taken each day


a <- aggregate(projData$steps, by = list(projData$date), FUN = sum)
hist(a$x)
ggplot(a, aes(x = Group.1, y = x)) +
  geom_line()
  # geom_histogram(binwidth = 1)

# 2. Calculate and report the mean and median total number of steps taken per day
#a <- aggregate(projData$steps, by = list(projData$date), FUN = function(x){
#  c(mean = mean(x),median = median(x))
#  })
#names(a) <- c("Date","Mean","Me")
medianTotSteps <- median(a$x, na.rm = TRUE)
meanTotSteps <- mean(a$x, na.rm = TRUE)






# What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
b <- aggregate(projData$steps, by = list(projData$interval), FUN = function(x) mean(x, na.rm = TRUE))
names(b) <- c("Interval", "Steps")
qplot(Interval, Steps, data = b, geom = "line")

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
b$Interval[b$Steps == max(b$Step, na.rm = TRUE)]




# Imputing missing values

# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(projData))
sum(!complete.cases(projData))

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
c <- aggregate(projData$steps, by = list(projData$interval), FUN = function(x) median(x, na.rm = TRUE))
names(c) <- c("Interval","MedianSteps")

stepsNA <- is.na(projData$steps)

imputedProjData <- projData
for(record in length(imputedProjData$interval)){
  currentInterval <- imputedProjData$interval[record]
  if (is.na(currentInterval)){
    imputedProjData$interval[record] <- b$Steps[match(currentInterval, c$Interval)]

  }
}
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# This guy -> imputedProjData

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



# Are there differences in activity patterns between weekdays and weekends?

# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
