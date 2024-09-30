#####Google Analytics Case Study####
#Bellabeat Data Analysis and Visualization  
#Data Transformation and Visualizing!
####Packages & Directory Selection####
install.packages('tidyverse')
install.packages('lubridate')
install.packages('dplyr')
install.packages('reshape2')

library(tidyverse)
library(lubridate)
library(dplyr)
library(reshape2)

#Set working directory
setwd("C:/Users/mpon/Desktop/GoogleAnalyticsCaseStudy/KaggleData_FitBitFitnessTracker/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16")

####Hourly Data Transformation####

#pull the dfs into the environment using read.csv
calories_hourly <- read.csv('hourlyCalories_merged.csv')
intensities_hourly <- read.csv('hourlyIntensities_merged.csv')
steps_hourly <- read.csv('hourlySteps_merged.csv')

#review using head()
head(calories_hourly)
head(intensities_hourly)
head(steps_hourly)
#All data includes IDs, a date, and relevant datapoint. 

#Verify No Duplication!
hourlyrows <- (
  nrow(calories_hourly) +
    nrow(intensities_hourly) +
    nrow(steps_hourly)
  )
hourlyuniquerows <- (
  nrow(unique(calories_hourly)) + 
    nrow(unique(intensities_hourly)) + 
    nrow(unique(steps_hourly))
  )

#TEST#
if (hourlyrows == hourlyuniquerows){
  print('There are no duplicate rows in the data')
  } else {
  print('Duplicates are present')
  }

#verify the same users are present in each file.
n_distinct(calories_hourly$Id)
n_distinct(intensities_hourly$Id)
n_distinct(steps_hourly$Id)
# 33 distinct user IDs in each file

#let's get a count of the IDs in the file.
hrID <- (c(unique(calories_hourly$Id), 
           unique(intensities_hourly$Id),
           unique(steps_hourly$Id)))

#transform numeric data to matrix and then dataframe
#one column IDs, fill by row!
hrIDmatrix <- matrix(data=hrID, ncol=1, byrow = TRUE)
hrIDdf <- as.data.frame(hrIDmatrix)
hrIDsummary <- hrIDdf %>% 
  group_by(V1) %>% 
  summarise(frequency = n())

#TEST#
if (unique(hrIDsummary$frequency) == 3){
  print('Each unique user is present in each dataset')
} else {
  print('Each unique user is NOT present in each dataset')
}
# great! only one unique value of 3, there are 33 unique users and 99 total observations. everyone's here.


#We need to clean the date data up, this what we'll use to join the tables eventually.
data.class(calories_hourly$ActivityHour)
data.class(intensities_hourly$ActivityHour)
data.class(steps_hourly$ActivityHour)
#they are all character data types.

#using lubridate functions - convert to date time. 
calories_hourly$dateclean <- mdy_hms(calories_hourly$ActivityHour)
intensities_hourly$dateclean <- mdy_hms(intensities_hourly$ActivityHour)
steps_hourly$dateclean <- mdy_hms(steps_hourly$ActivityHour)

#mutate date time to hour column for visualizing by hour. 
calories_hourly <- calories_hourly %>% 
  mutate(hour = hour(calories_hourly$dateclean))
intensities_hourly <- intensities_hourly %>% 
  mutate(hour = hour(intensities_hourly$dateclean))
steps_hourly <- steps_hourly %>% 
  mutate(hour = hour(steps_hourly$dateclean))

#now let's aggregate by hour for visualizing.
calories_aggregated <- aggregate(x = calories_hourly$Calories, 
                                 by=list(calories_hourly$hour),
                                 FUN = sum)
colnames(calories_aggregated) <- c('hour', 'calories')

intensities_aggregated <- aggregate(x=intensities_hourly$TotalIntensity, 
                                    by=list(intensities_hourly$hour),
                                    FUN = sum)
colnames(intensities_aggregated) <- c('hour', 'intenseminutes')

steps_aggregated <- aggregate(x = steps_hourly$StepTotal, 
                              by=list(steps_hourly$hour),
                              FUN = sum)
colnames(steps_aggregated) <- c('hour', 'steptotal')

#### Hourly Data Visualizations ####
#Do some basic visualizing to ID some trends.
# Calories -> users burn the most calories in the afternoon (noon - 6pm)
ggplot(data = calories_aggregated,
       aes(x = hour,
           y = calories,
           fill = calories)) +
  geom_bar(stat='identity', na.rm=TRUE) +
  scale_fill_gradient(low="blue", high="orange") +
  geom_smooth()

#Intensities -> users are most intense during a similar period to calories, activity falls at 3pm.
ggplot(data = intensities_aggregated,
       aes(x = hour,
           y = intenseminutes,
           fill = intenseminutes)) +
  geom_bar(stat='identity', na.rm=FALSE) +
  scale_fill_gradient(low="blue", high="orange") +
  geom_smooth()
  
#Steps -> trends align with previous charts. this makes sense!
ggplot(data = steps_aggregated,
       aes(x=hour,
           y=steptotal,
           fill=steptotal)) +
  geom_bar(stat='identity', na.rm=TRUE) +
  scale_fill_gradient(low="blue", high="orange") +
  geom_smooth()

#Let's pull all of this data together to visualize this consistent trend.
#join the data
hourlyjoin <- calories_aggregated %>% 
  left_join(steps_aggregated, by = 'hour') %>% 
  left_join(intensities_aggregated, by = 'hour')

#We want user averages so we'll divide the figures by the number of users in each set (33)
#I want to reduce the figure further to include daily average instead of aggregated sum. 

d1 = as_date('2016-04-12')
d2 = as_date('2016-05-12')
difftime(d2, d1, 'days')
#The time difference is 30 days. 

hourlyplot <- ggplot(hourlyjoin,
       aes(x = hour,
           y = intenseminutes/33/30)) +
  geom_point(aes(color = calories/33/30, 
                 size = steptotal/33/30)) +
  scale_color_gradient(low="blue", 
                       high="orange") +
  labs(title = 'Average User Hourly Minutes of Intensity, Calories Burnt and Steps Taken',
       subtitle = 'Fitbit Fitness Tracker Data by Mechanical Turk 4/12/16 - 5/12/16',
       y = 'Minutes of Intensity',
       x = 'Hour of Day',
       color = 'Calories Burnt',
       size = 'Steps Taken')
####Hourly Questions to Answer####


#Justify na.rm
  #remove any n/a values to not incorrectly skew bars/throw off calculations with available data.
  #not a huge issue, can be ran false without issue.

#what is stat = 'identity'
  #tells R that the geom_bar() y calculation will be from the data itself and not a count
  #https://stackoverflow.com/questions/59008974/why-is-stat-identity-necessary-in-geom-bar-in-ggplot

#Describe findings

####Hourly Data Findings####
hourlyplot
#What's the basic trend?
#hourlyplot visualizes the trend of users to be most active in the afternoon. We see 
#users are most active daily after noon and through the evening (before 8pm).
#This includes the most calories burnt, intense minutes, and steps taken.
#How does this apply to the customer?
#Users are using Fitbit devices through the day to record data. Minutes of intensity 
#only really tapers off to zero in the wee hours of the morning. Users are otherwise
#utilizing fitbit to record activity data during their entire day. 
#The device is more of a lifestyle tracker rather than a fitness tracker.
#How can this inform Bellabeat's Marketing strategy? 
#Bellabeat time is a wellness watch that tracks and records data similar to the Fitbit data
#presented here. In order to better compete in the space, the integrated app from Bellabeat
#should be advertised with as a general wellness piece with an emphasis on it's ability to record fitness/activity data! 

#With clear daily trends, the Bellabeat membership can also be advertised with a functionality to 
#administer/promote group workouts - targeting smart device users who see to be active during post-business hours.
#with user data available like this, building functionalities that recognize unique daily trends can be created
#these functionalities should also be advertised!

#With the data presented, it is obvious Urska had wanted us to investigate the usage of smart "fitness" devices.
#Bellabeat has positioned itself as a general wellness company, it's devices are built with similar functionalities
#to those that are built for fitness specific purposes (fitbit/garmin, etc.). They have an opportunity to
#fit both niches - similar to how Apple has built an Apple watch. Bellabeat's values in supporting women should be
#widely shared which may make them a more enticing product than Apple's offerings for women.
####Daily Data Transformations####

#pull data into environment
sleep_daily <- read.csv('sleepDay_merged.csv')
activity_daily <- read.csv('dailyActivity_merged.csv')
calories_daily <- read.csv('dailyCalories_merged.csv')
intensity_daily <- read.csv('dailyIntensities_merged.csv')
steps_daily <- read.csv('dailySteps_merged.csv')

#review using head()
head(sleep_daily)
head(activity_daily)
head(calories_daily)
head(intensity_daily)
head(steps_daily)
#looks like we're joining on id/date again

#Verify No Duplication!
dailyrows <- (
  nrow(sleep_daily) +
    nrow(activity_daily) +
    nrow(calories_daily)+
    nrow(intensity_daily)+
    nrow(steps_daily)
)
dailyuniquerows <- (
  nrow(unique(sleep_daily)) +
    nrow(unique(activity_daily)) +
    nrow(unique(calories_daily))+
    nrow(unique(intensity_daily))+
    nrow(unique(steps_daily))
)
dailyuniquerows
#TEST#
if (dailyrows == dailyuniquerows){
  print('There are no duplicate rows in the data')
} else {
  print('Duplicates are present')
}

#There seem to be three duplicate rows.
sleep_daily <- unique(sleep_daily)
activity_daily <- unique(activity_daily)
calories_daily <- unique(calories_daily)
intensity_daily <- unique(intensity_daily)
steps_daily <- unique(steps_daily)

#Run test again
dailyrows <- (
  nrow(sleep_daily) +
    nrow(activity_daily) +
    nrow(calories_daily)+
    nrow(intensity_daily)+
    nrow(steps_daily)
)
dailyuniquerows <- (
  nrow(unique(sleep_daily)) +
    nrow(unique(activity_daily)) +
    nrow(unique(calories_daily))+
    nrow(unique(intensity_daily))+
    nrow(unique(steps_daily))
)
dailyuniquerows
#TEST#
if (dailyrows == dailyuniquerows){
  print('There are no duplicate rows in the data')
} else {
  print('Duplicates are present')
}
#No more dupes!

#Verify the same users are present in each file.
n_distinct(sleep_daily$Id)
n_distinct(activity_daily$Id)
n_distinct(calories_daily$Id)
n_distinct(intensity_daily$Id)
n_distinct(steps_daily$Id)

#33 users are present except for in sleep daily (24)
#let's get a count of the IDs in the file.
dayID <- c(unique(sleep_daily$Id),
           unique(activity_daily$Id),
           unique(calories_daily$Id),
           unique(intensity_daily$Id),
           unique(steps_daily$Id))

#transform numeric data to matrix and then dataframe
#one column IDs, fill by row!
dayIDmatrix <- matrix(data=dayID, ncol=1, byrow = TRUE)
dayIDdf <- as.data.frame(dayIDmatrix)
dayIDsummary <- dayIDdf %>% 
  group_by(V1) %>% 
  summarise(frequency = n())

sum(dayIDsummary$frequency)
#sum is 156, 4x33+24 
4*33+24
#TEST#
if (mean(dayIDsummary$frequency) == 5){
  print('Each unique user is present in each dataset')
} else {
  print('Each unique user is NOT present in each dataset')
}

mean(dayIDsummary$frequency)
156/33
# great! mean is not 5, there are 33 unique users and 156 total observations. everyone's here.
# we are short because we are missing sleep data from 9 users. 

#data.class of the dates present

data.class(sleep_daily$SleepDay)
data.class(activity_daily$ActivityDate)
data.class(calories_daily$ActivityDay)
data.class(intensity_daily$ActivityDay)
data.class(steps_daily$ActivityDay)
#all characters, let's add a date column. 

#convert to date
sleep_daily$date <- mdy_hms(sleep_daily$SleepDay)
activity_daily$date <- mdy(activity_daily$ActivityDate)
calories_daily$date <- mdy(calories_daily$ActivityDay)
intensity_daily$date <- mdy(intensity_daily$ActivityDay)
steps_daily$date <- mdy(steps_daily$ActivityDay)

#check out the date transformation
head(sleep_daily)
head(activity_daily)
head(calories_daily)
head(intensity_daily)
head(steps_daily)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Don't share, do not comprehend.
#Data Join
#merge via list() base r function
#dailydata = list(sleep_daily, activity_daily, calories_daily, intensity_daily, steps_daily)
#head(dailydata)

#this worked - I don't full comprehend reduce()
#dailydatajoin <- dailydata %>% 
  #reduce(left_join, by=c('Id', 'date'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Share this instead

dailydata <- left_join(sleep_daily, activity_daily, by = c('Id', 'date'))

dailydata <-left_join(dailydata, calories_daily, by = c('Id', 'date')) %>% 
left_join(dailydata, intensity_daily, by = c('Id', 'date')) %>%
left_join(dailydata, steps_daily, by = c('Id', 'date'))

head(dailydata)
#all the columns! and dupes. let's clean. 
colnames(dailydata)
#select the columns, this is our clean dataset!
dailydataclean <- dailydata[,c(1,6,20,21,41:57)]

head(dailydataclean)

####Daily Data Visualizations####
#Activity Data

activityplot <- ggplot(data = dailydataclean)

activityplot + 
  geom_point(data=dailydataclean, 
             aes(x=date, 
                 y=SedentaryMinutes), 
             color = 'green') +
  geom_point(data=dailydataclean, 
             aes(x=date, 
                 y=LightlyActiveMinutes), 
             color = 'blue') +
  geom_point(data=dailydataclean, 
             aes(x=date, 
                 y=FairlyActiveMinutes), 
             color ='purple') +
  geom_point(data=dailydataclean, 
             aes(x=date, 
                 y=VeryActiveMinutes), 
             color ='red') +
  geom_line(data=dailydataclean, 
            aes(x=date,
                y=mean(SedentaryMinutes)),
            color='green')

activityplot + 
  geom_point(data=dailydataclean, 
             aes(x=TotalMinutesAsleep/60, 
                 y=SedentaryMinutes), 
             color = 'green') +
  geom_point(data=dailydataclean, 
             aes(x=TotalMinutesAsleep/60, 
                 y=LightlyActiveMinutes), 
             color = 'blue') +
  geom_point(data=dailydataclean, 
             aes(x=TotalMinutesAsleep/60, 
                 y=FairlyActiveMinutes), 
             color ='purple') +
  geom_point(data=dailydataclean, 
             aes(x=TotalMinutesAsleep/60, 
                 y=VeryActiveMinutes), 
             color ='red')

activityplot +
  geom_smooth(data=dailydataclean, 
              aes(x=TotalMinutesAsleep/60, 
                  y=SedentaryMinutes), se = FALSE, color = 'green') +
  geom_smooth(data=dailydataclean, 
              aes(x=TotalMinutesAsleep/60, 
                  y=LightlyActiveMinutes), se = FALSE, color = 'blue') +
  geom_smooth(data=dailydataclean, 
              aes(x=TotalMinutesAsleep/60, 
                  y=FairlyActiveMinutes),
              se = FALSE, color = 'purple') +
  geom_smooth(data=dailydataclean, 
              aes(x=TotalMinutesAsleep/60, 
                  y=VeryActiveMinutes), se = FALSE, color = 'red') =

#Interesting point with sedentary minutes declining with increased sleep. 

#Calories, Actvity, and Steps Look
#More activity minutes/steps more calories burnt. Logical!
activitycalorieplot <- ggplot(data = dailydataclean) +
  geom_point(aes(x=VeryActiveMinutes, y=Calories.x))
activitycalorieplot

stepcalorieplot <- ggplot(data = dailydataclean) +
  geom_point(aes(x=TotalSteps, y=Calories.x))
stepcalorieplot

#Let's solidify this point by joining all the tables into a visual.

#Why are we melting#
#to reshape the activity_data into long format where the ID, Date, and each activity 
#type is it's own record.
activitydailylong <- melt(activity_daily, 
                     id.vars = c('Id', 'date'), 
                     measure.vars = c('VeryActiveMinutes', 
                                      'FairlyActiveMinutes', 
                                      'LightlyActiveMinutes', 
                                      'SedentaryMinutes'))
#We then joined the long data with the rest of the table to match the hours of sleep to each record.
adljoin <- left_join(activitydailylong, dailydataclean, by = c('Id', 'date'))
head(adljoin)
colnames(adljoin)
n_distinct(adljoin$variable)
unique(adljoin$variable)

ggplot(data = adljoin, aes(TotalMinutesAsleep/60, value/60, group = variable, color = variable)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  #  stat_summary(fun = 'mean') 
  labs(title = 'Hours of Activity by Hours Slept Daily',
       subtitle = 'Mechanical Turk Data - Fitbit April - May',
       x = 'Hours of Sleep',
       y = 'Hours of Activity',
       color = 'Activity Type')
#note stat summary differences
ggplot(data = adljoin, aes(TotalMinutesAsleep/60, value/60, group = variable, color = variable)) +
  geom_smooth(se=FALSE) +
  #  stat_summary(fun = 'mean') +
  geom_vline(xintercept = 7, linetype = "dashed", color = " dark green", size = 1) +
  facet_grid(rows= vars(variable), scales = 'free') +
  labs(title = 'Hours of Activity by Hours Slept Daily',
       subtitle = 
       'Faceted by Activity Type
Mechanical Turk Data - Fitbit April - May',
       x = ('Hours of Sleep'),
       y = 'Hours of Activity',
       color = 'Activity Type')

#Warning message received about 2,120 rows containing non-finite data outside of scale range.
#We were missing a real portion of sleep data (data for 24 ppts vs 33 ppts being analyzed)
#The full datasets were joined.

sleepanalysis <- adljoin %>% 
  group_by(TotalSleepRecords) %>% 
  summarise(n=n())

sleepanalysis
#The 2,120 rows containing non-finite data are the 2,120 records with a missing (NA) input for TotalSleepRecords.

####Daily Questions to Answer####
#https://www.ncbi.nlm.nih.gov/books/NBK279322/ 
#avg person sleep about 7 rhs
#Explain findings
#Explain melt - done
#Explain the 2120 missing points in the final table.- done
#Clean up the long activity data (Select columns) - done, no rev
help(melt)

####Daily Data Findings####
#What's the basic trend?
  #I really explored the relationship between sleep and activity and found initially
  #Sedentary minutes generally decline daily with more sleep.

  #I then dove into daily calories vs daily steps, mroe steps equated to more calories burnt.
  #This was a logical relationship.

  #I then reshaped the activity data into a long format by ID/DATE/Activity Type to allow for a faceted analysis.
  #I found my first unusual trend: after 7hrs of sleep: very active minutes took a decline from it's peak,
  #fairly active minutes took a dip 
  #lightly active minutes peaked
  #sedentary minutes continually declined with more sleep
  #limitation here - i don't know how these are all defined.
  #sleep data was limited, only 24/33 of users in data set had sleep data.

#How does this apply to the customer?
  #greater amounts of sleep (up to the general average of 7hrs), can be associated with greater very active minutes.
  #sleep beyond the average associates with lower sedentary minutes and greater lightly active minutes.

#How does it impact the marketing strategy?
  # externally - boasting functionalities that support long term behavioral changes (better sleep, exercise reminders)
  #would be a strong selling point for bellabeat, especially it's timepiece and application products.

  #internally - in order to really deliver on the user behavioral changes additional data is needed. Bellabeat should work to 
  #obtain consent and begin collecting user data from it's unique customers (wellness motivated, tech-savvy women) in order
  #to begin developing the functionalities it needs to best promote healthy sleep, activity, and intensity habits. 
  #ex: reminders, social "leaderboards" (see strava), personal analytics dashboards, etc. 
