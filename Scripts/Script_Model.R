# requiere libraries ------------------------------------------------------
pacman::p_load(RMySQL,caret,tidyverse,ggplot2,chron,lubridate,RColorBrewer,
               scales, reshape,devtools,bbplot,colorspace,plotly,ggfortify,
               forecast,imputeTS,padr)
# devtools::install_github('bbc/bbplot')


# get the data ------------------------------------------------------------

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
            host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# 
# ## List the tables contained in the database 
# dbListTables(con)
# 
# 
# ## Lists attributes contained in a table
# dbListFields(con,'iris')
# 
# 
# ## Use asterisk to specify all attributes for download
# irisALL <- dbGetQuery(con, "SELECT * FROM iris")
# 
# ## Use attribute names to specify specific attributes for download
# irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
# 
# 
# ## Lists attributes contained in a year 2006
dbListFields(con,"yr_2006")

## Use attribute names to specify specific attributes for download
data2006 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
                       Sub_metering_2,Sub_metering_3 FROM yr_2006")
data2007 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1, 
                       Global_active_power,
                       Sub_metering_2,Sub_metering_3 FROM yr_2007")
data2008 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
                       Global_active_power,
                       Sub_metering_2,Sub_metering_3 FROM yr_2008")
data2009 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
                       Global_active_power,
                       Sub_metering_2,Sub_metering_3 FROM yr_2009")
data2010 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
                       Global_active_power,
                       Sub_metering_2,Sub_metering_3 FROM yr_2010")
# as_tibble(data2007)


# data<-as_tibble(bind_rows(data2007,data2008,data2009))
# data <- as_tibble(data)
# data_full <- data
data_year <- as_tibble(bind_rows(data2006,data2007,data2008,data2009,data2010))



## Combine Date and Time attribute values in a new attribute column
data_year <-cbind(data_year,paste(data_year$Date,data_year$Time),
                  stringsAsFactors=FALSE)

# data_full %>% unite(col="DateTime",Date,Time,remove = FALSE)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change 
# the column number)
colnames(data_year)[7] <-"DateTime"

## Move the DateTime attribute within the dataset
data_year <- data_year[,c(ncol(data_year), 1:(ncol(data_year)-1))]
head(data_year)


## Convert DateTime from POSIXlt to POSIXct 
data_year$DateTime <- as.POSIXct(data_year$DateTime, "%Y/%m/%d %H:%M:%S")
# data_full <- as.data.frame(data_full)
# thetimes_full <- chron(data_full[,1],
# data_full[,2],
# format =c(Date= "y-m-d",Time ="h:m:s" ))
## Add the time zone
attr(data_year$DateTime, "tzone") <- "Europe/Paris"

data_year <- data_year %>% pad(break_above = 3)



# lists -------------------------------------------------------------------
group<- c("year", "month", "week", "day")
granularity<- list()
data_year3 <- data_year2
data_year2 <-data_year %>% select(colIcare) %>% as_tibble() 
for (i in group) {

  data_year3$DateTime <- floor_date(x = data_year2$DateTime, unit = i)
  
  granularity[[i]] <- data_year3 %>% group_by(DateTime) %>% 
    summarise(sub1 = sum(Sub_metering_1),sub2 = sum(Sub_metering_2),sub3 = sum(Sub_metering_3))

}

granularity$month$ID <- 1:length(granularity$month$DateTime)
write.csv(x = granularity$month, file = "DATAMONTH.csv" )
# funcion Ibai -------------------------------------------------------------


# nonaconsumption <- pad(consumption, break_above = 3)
nonaconsumption <- data_year

(which(is.na(padconsumption))/nrow(nonaconsumption))+1 #just to identify a col with NAs

vector_na <- which(is.na(nonaconsumption[,2]))

box <- c()
box_length <- c()
count <- 1
for ( i in 1:length(vector_na) ) {
  i <- i + sum(box_length) - length(box_length)
  if(i > length(vector_na)){stop("you are at the end, my friend")}
  else{
    if(i == length(vector_na)){box <- c(box, vector_na[i])
    box_length <- c(box_length, count)
    stop("the last NA is isolated")}
  }
  box <- c(box, vector_na[i])
  while(vector_na[i+1] == (vector_na[i] + 1))
    {count <- count + 1; print(count); i=i+1}
  box_length <- c(box_length, count)
  count = 1
}



# COntinuar con el ejercicio ----------------------------------------------


data_year[,-(1:3)] <- sapply(data_year[,-(1:3)],function(x){ na_locf(x)})
write.csv(x = data_year,file = "datatable.csv")
# new columns
task2_data<- data_year %>% mutate(year = year(DateTime),
                                   consumption = Global_active_power*1000/60,
                                   cost= consumption*0.1472,
                                   other = consumption - Sub_metering_1 - 
                                   Sub_metering_2 - Sub_metering_3,
                                   month = month(DateTime),
                                   week = week(DateTime),
                                   weekDay = weekdays(DateTime),
                                   day = day(DateTime),
                                   hour  = hour(DateTime),
                                   minute = minute(DateTime),
                                   quarter_year = quarter(x = Date,
                                                          with_year = TRUE),
                                   quarter = quarter(x = Date,
                                                     with_year = FALSE))


write.csv(x = task2_data,file = "FULLDATA.csv")
# Plan of atack task 2 ----------------------------------------------------

## Plot all of sub-meter 1
# plot(data_year$Sub_metering_1)

ploting_cols<-c("Sub_metering_1","Sub_metering_2","Sub_metering_3",
                "consumption","other","cost")

# MONTH

dataforecastmonth <- task2_data %>% group_by(year,month) %>%
  summarise_at(vars(ploting_cols),list(sum=~sum(.))) %>% round(digits = 0)

write.csv(x = dataforecastmonth,file = "dataforecastmonth.csv")

ts_consumption_month <- ts(data = dataforecastmonth$consumption_sum,
                     start = c(2006,12), frequency = 12)

autoplot( ts_consumption_month  %>% stl(s.window = "periodic"))
 
stlmonth <- ts_consumption_month  %>% stl(s.window = "periodic")
apply(stlmonth$time.series, 2, var)/var(ts_consumption_month)


ts_consumption_month_train <- window(ts_consumption_month, end = c(2010,1))
ts_consumption_month_test <- window(ts_consumption_month, start = c(2010,1))

arima_auto_01 <- auto.arima(ts_consumption_month_train)
model01 <- HoltWinters(ts_consumption_month_train)
pred01<-forecast(model01,h = 11)
predarima01<-forecast(arima_auto_01,h = 11)

autoplot(ts_consumption_month_test, colour = "black") + 
  autolayer(pred01, colour = "red", PI=FALSE)
autoplot(ts_consumption_month_test, colour = "black") + 
  autolayer(predarima01, colour = "red", PI=FALSE)
accuracy(predarima01,ts_consumption_month_test)
accuracy(pred01,ts_consumption_month_test)

# WEEK

dataforecastweek <- task2_data %>% group_by(year,month,week) %>%
  summarise_at(vars(ploting_cols),list(sum=~sum(.))) %>% round(digits = 0)

write.csv(x = dataforecastweek,file = "dataforecastweek.csv")

ts_consumption_week <- ts(data = dataforecastweek$consumption_sum,
                         start = c(200,50), frequency = 52)


stlweek<- ts_consumption_week  %>% stl(s.window = "periodic")
autoplot( ts_consumption_week  %>% stl(s.window = "periodic"))
apply(stlweek$time.series, 2, var)/var(ts_consumption_week)

ts_consumption_week_train <- window(ts_consumption_week, end = c(2010,1))
ts_consumption_week_test <- window(ts_consumption_week, start = c(2010,2))

model02 <- HoltWinters(ts_consumption_week_train)
pred02<-forecast(model01,h = length(ts_consumption_week_test))

autoplot(ts_consumption_week_test, colour = "black") + 
  autolayer(pred02, colour = "red", PI=FALSE)

# DAY

dataforecastday <- task2_data %>% group_by(year,month,day) %>%
  summarise_at(vars(ploting_cols),list(sum=~sum(.))) %>% round(digits = 0)

write.csv(x = dataforecastday,file = "dataforecastday.csv")

ts_consumption_day <- ts(data = dataforecastday$consumption_sum,
                         start = c(2007), frequency = 365)

stlday <- ts_consumption_day  %>% stl(s.window = "periodic")
autoplot( ts_consumption_day  %>% stl(s.window = "periodic"))
apply(stlday$time.series, 2, var)/var(ts_consumption_day)

ts_consumption_day_train <- window(ts_consumption_day, end = c(2010,1))
ts_consumption_day_test <- window(ts_consumption_day, start = c(2010,2))

autoplot( ts_consumption_day_train  %>% stl(s.window = "periodic"))

model03 <- HoltWinters(ts_consumption_day_train)
# arima03 <- auto.arima(ts_consumption_day_train)
pred03<-forecast(model03,h = length(ts_consumption_day_test))

autoplot(ts_consumption_day_test, colour = "black") + 
  autolayer(pred03, colour = "red", PI=FALSE)



## Subset the second week of 2008 - All Observations
houseWeek <- filter(task2_data, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)


## Subset the 9th day of January 2008 - All observations
houseDay <- filter(task2_data, year == 2008 & month == 1 & day == 9)

justaday<- task2_data %>% filter(year == 2008,month == 1, day == 9,
                                 Sub_metering_1 > 1)

## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,
        type = 'scatter', mode = 'lines')

ggplot(justaday, aes(x=DateTime, y = Sub_metering_1,color = Sub_metering_1) ) + 
  geom_line(size  = 1.3) + 
  bbc_style()+
  labs(y = "I'm an axis")
  

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room',
            mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))




## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(task2_data,
                     year == 2008 & month == 1 & day == 9 &
                       (minute == 0 | minute == 10 | minute == 20 |
                          minute == 30 | minute == 40 | minute == 50))

houseDay10 <- task2_data %>% filter(year == 2008, month == 1, day == 9,
                                    minute == 0 | minute == 10 | minute == 20 |
                                      minute == 30 | minute == 40 |
                                      minute == 50)

gathered <- houseDay10 %>% select(DateTime,Sub_metering_1,Sub_metering_2,
                                  Sub_metering_3) %>% 
  gather(key = "Sub_metering", value = "measurement",-DateTime)


ggplot(data = gathered, aes(x = DateTime) ) +
  geom_line(aes(y= measurement, color = Sub_metering)) + 
  labs(title = "2008-01-09 every 10 min", x="Time", y = "consumption") + 
  scale_fill_continuous(name = "Title",labels = c('Kitchen',
                                                'Laundry Room',
                                                'Water Heater & AC'))



## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute 
# frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime,
        y = ~houseDay10$Sub_metering_1, name = 'Kitchen',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room',
            mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset to one observation per week on Mondays at 8:00pm for 2007,
# 2008 and 2009
house070809weekly <- filter(task2_data, weekDay == "martes" &
                              hour == 20 & minute == 1)

house070809weekly <- task2_data %>% filter(weekDay == "martes",
                                             hour == 20, minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52,
                         start=c(2007,1))


## Plot sub-meter 3 with autoplot (you may need to install these packages)


autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 3")

plot.ts(tsSM3_070809weekly)


## Apply time series linear regression to the sub-meter 3 ts object 
# and use summary to obtain R2 and RMSE from the model you built
# library(forecast)
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 

summary(fitSM3)


## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)
## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - 
  components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very,
# very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))


## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))


## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours",
     xlab="Time - Sub-meter 3")


