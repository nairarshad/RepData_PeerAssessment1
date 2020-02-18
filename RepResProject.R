## Load Libraries
list.of.packages <- c("data.table","plyr","dplyr","tibble", "lubridate", "knitr", "ggplot2", "stringr", "forcats", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Functions
sumnona <- function(x){if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)}
meanona <- function(x){if (all(is.na(x))) x[NA_integer_] else mean(x, na.rm = TRUE)}

# DATA DOWNLOAD
# Create temporary file
temp <- tempfile()

# Download zip into temporary file
download.file(url = "https://github.com/nairarshad/RepData_PeerAssessment1/blob/master/activity.zip?raw=true",
              destfile = temp, mode = 'wb')

# Unzip the zip file
data.unzip <- unzip(temp)

# Unlink the temporary file
unlink(temp)

# DATA READ

# Read the activity data
activity.data <- read.csv(data.unzip, stringsAsFactors = FALSE) %>% 
    tbl_df() %>%
    mutate(date = as.Date(date))

# Total steps by date
total.steps <- activity.data %>%
    group_by(date) %>%
    summarize(total_steps = sumnona(steps),
              total_intervals = n()) %>%
    mutate(total_intervals = NULL)

# Histogram of the total number of steps taken each day
p1 <- ggplot(data = total.steps, aes(x = total_steps)) 
p1 <- p1 + geom_histogram(bins = 25) 
p1 <- p1 + labs(x = "Total Step Count",
                y = "Count",
                title = "Histogram of the total number of steps taken each day")
p1 <- p1 + theme_bw()

# Mean and median total steps
total.steps.mean <- mean(total.steps$total_steps, na.rm = TRUE)
total.steps.medn <- median(total.steps$total_steps, na.rm = TRUE)

# Average step count for each 5-min interval
interval.5min <- activity.data %>%
    group_by(interval) %>%
    summarize(avg_steps = mean(steps, na.rm = TRUE))

# Time series plot of the average number of steps taken across all days at 5-minute intervals
p2 <- ggplot(data = interval.5min, aes(x = interval, y = avg_steps))
p2 <- p2 + geom_line()
p2 <- p2 + labs(x = "Interval (HHMM)",
                y = "Average step count",
                title = "Time series plot of the average number of steps taken across all days at 5-minute intervals")
p2 <- p2 + scale_x_continuous(labels = function(x) str_pad(x, width = 4, pad = "0"))
p2 <- p2 + theme_bw()

# Which interval corresponds to maximum average steps
maxintrvl <- interval.5min$interval[which.max(interval.5min$avg_steps)]

# How many NAs in the data
countna <- sum(is.na(activity.data$steps))

# Inititalize df for manioulation: impute for NAs
activity.data.nona <- as.data.frame(activity.data)

# Row-wise identify NA and replace with interval average
for(i in 1:nrow(activity.data.nona)){
    if(is.na(activity.data.nona[i,"steps"])){
        intrvl <- activity.data.nona[i,"interval"]
        activity.data.nona[i,"steps"] <- 
            interval.5min$avg_steps[interval.5min$interval==intrvl]
    }
}

# Total steps from imputed data by date
total.steps.nona <- activity.data.nona %>%
    group_by(date) %>%
    summarize(total_steps = sumnona(steps),
              total_intervals = n()) %>%
    mutate(total_intervals = NULL)

# Histogram of the total number of steps taken each day (Imputed Data)
p3 <- ggplot(data = total.steps.nona, aes(x = total_steps)) 
p3 <- p3 + geom_histogram(bins = 25) 
p3 <- p3 + labs(x = "Total Step Count",
                y = "Count",
                title = "Histogram of the total number of steps taken each day (Imputed Data)")
p3 <- p3 + theme_bw()

# Mean and median total steps after imputing
total.steps.nona.mean <- mean(total.steps.nona$total_steps, na.rm = TRUE)
total.steps.nona.medn <- median(total.steps.nona$total_steps, na.rm = TRUE)

# Backup the imputed data
activity.data.nona.bc <- activity.data.nona
activity.data.nona <- NULL

# Add factor column whether Weekday/Weekend
activity.data.nona <- activity.data.nona.bc %>%
    mutate(day = fct_collapse(factor(weekdays(date, abbreviate = TRUE)), 
                              Weekend = c("Sat","Sun"),
                              Weekday = c("Mon","Tue","Wed","Thu","Fri"))) %>%
    mutate(day = factor(day, levels = c("Weekend","Weekday")))

# Average by wday and interval
wday.mean <- activity.data.nona %>%
    tbl_df() %>%
    group_by(day, interval) %>%
    summarize(avg_steps = mean(steps, na.rm = TRUE))

# Time series plot of the average number of steps taken across all days at 5-minute intervals
p4 <- ggplot(data = wday.mean, aes(x = interval, y = avg_steps))
p4 <- p4 + geom_line()
p4 <- p4 + labs(x = "Interval (HHMM)",
                y = "Average step count",
                title = "Time series plot of the average number of steps taken across all days at 5-minute intervals")
p4 <- p4 + scale_x_continuous(labels = function(x) str_pad(x, width = 4, pad = "0"))
p4 <- p4 + facet_grid(day~.)
p4 <- p4 + theme_bw()


