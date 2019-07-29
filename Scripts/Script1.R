pacman::p_load(RMySQL,caret,tidyverse,ggplot2,chron,lubridate,RColorBrewer,
               scales, reshape,devtools,bbplot,colorspace,plotly)
# devtools::install_github('bbc/bbplot')

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
# data2006 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#                        Sub_metering_2,Sub_metering_3 FROM yr_2006")
# data2007 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1, 
#                        Global_active_power,
#                        Sub_metering_2,Sub_metering_3 FROM yr_2007")
# data2008 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#                        Global_active_power,
#                        Sub_metering_2,Sub_metering_3 FROM yr_2008")
# data2009 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#                        Global_active_power,
#                        Sub_metering_2,Sub_metering_3 FROM yr_2009")
# data2010 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#                        Global_active_power,
#                        Sub_metering_2,Sub_metering_3 FROM yr_2010")
# as_tibble(data2007)

data_year <- as_tibble(bind_rows(
  dbGetQuery(con, "SELECT Date,Time,Sub_metering_1, 
                       Global_active_power,
                       Sub_metering_2,Sub_metering_3 FROM yr_2007"),
  dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
                       Global_active_power,
                       Sub_metering_2,Sub_metering_3 FROM yr_2008"),
  dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
                       Global_active_power,
                       Sub_metering_2,Sub_metering_3 FROM yr_2009"),
  dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
                       Global_active_power,
                       Sub_metering_2,Sub_metering_3 FROM yr_2010")))

# data<-as_tibble(bind_rows(data2007,data2008,data2009))
# data <- as_tibble(data)
# data_full <- data
data_year <- as_tibble(bind_rows(data2007,data2008,data2009,data2010))


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

## Inspect the data types
str(data_year)

data_year2 <- data_year %>% mutate(year = year(DateTime),
                                   consumption = Global_active_power*1000/60,
                                   cost= consumption*0.1472,
                                   other = consumption - Sub_metering_1 - 
                                     Sub_metering_2 - Sub_metering_3,
                                   month = month(DateTime),
                                   week = week(DateTime),
                                   # weekday = weekday(DateTime),
                                   day = day(DateTime),
                                   hour  = hour(DateTime),
                                   minute = minute(DateTime),
                                   quarter_year = quarter(x = Date,
                                                          with_year = TRUE),
                                   quarter = quarter(x = Date,
                                                     with_year = FALSE))
# yourdataframe$year <- year(yourdataframe$DateTime)


# data_year2 <- as_tibble(data_year2 %>% filter(year != 2010))



ploting_cols<-c("Sub_metering_1","Sub_metering_2","Sub_metering_3",
                "consumption","other","cost")




data_year2 %>% group_by(year) %>% 
  summarise_at(vars(ploting_cols),
               list(sum=~sum(.))) -> results_year

names(results_year) %in% "sum"

data_year2 %>% group_by(year,quarter) %>% 
  summarise_at(vars(ploting_cols),
               list(~min(.),~max(.),
                    ~mean(.), ~sd(.),sum=~sum(.)/1000)) -> 
  results_quarter

data_year2 %>% group_by(year,quarter,month) %>% 
  summarise_at(vars(ploting_cols),
               list(~min(.),~max(.),
                    ~mean(.), ~sd(.),sum=~sum(.)/1000)) -> results_month




data_year2 %>% group_by(year,quarter,month,day) %>% 
  summarise_at(vars(ploting_cols),
               list(sum=~sum(.)/1000)) -> results_day


# consumption by timeframe ------------------------------------------------


# consumption year

ggplot(data=results_year,aes(x = year)) + 
  geom_line(aes(y = Sub_metering_1_sum,col = "Sub_metering_1_sum",),size = 1) +
  geom_point(aes(y = Sub_metering_1_sum,col = "Sub_metering_1_sum"),size = 2) +  
  geom_line(aes(y = Sub_metering_2_sum,col = "Sub_metering_2_sum"),size = 1) + 
  geom_point(aes(y = Sub_metering_2_sum,col = "Sub_metering_2_sum"),size = 2) + 
  geom_line(aes(y = Sub_metering_3_sum,col = "Sub_metering_3_sum"),size = 1) + 
  geom_point(aes(y = Sub_metering_3_sum,col = "Sub_metering_3_sum"),size = 2) + 
  # geom_line(aes(y = consumption_sum,col = "consumption_sum"),size = 1) + 
  # geom_point(aes(y = consumption_sum,col = "consumption_sum"),size = 2) + 
  geom_line(aes(y = consumption_sum,col = "other_sum"),size = 1) +
  geom_point(aes(y = consumption_sum,col = "other_sum"),size = 2) +
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) + 
  expand_limits(x = 2007 , y = 0)  + 
  scale_y_continuous(labels = comma) + 
  scale_color_discrete(
    labels = c("other","kitchen","laundry room",
               "heating"
               # "total",
               
    ),
    name="") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("kW/hour")+ xlab("") + guides(aes()) + ggtitle("Consumption Year")

# consumption quarter

ggplot(data=results_quarter,aes(x = quarter)) + 
  geom_line(aes(y = Sub_metering_1_sum,col = "Sub_metering_1_sum"),size = 1) +
  geom_point(aes(y = Sub_metering_1_sum,col = "Sub_metering_1_sum"),size = 2) +  
  geom_line(aes(y = Sub_metering_2_sum,col = "Sub_metering_2_sum"),size = 1) + 
  geom_point(aes(y = Sub_metering_2_sum,col = "Sub_metering_2_sum"),size = 2) + 
  geom_line(aes(y = Sub_metering_3_sum,col = "Sub_metering_3_sum"),size = 1) + 
  geom_point(aes(y = Sub_metering_3_sum,col = "Sub_metering_3_sum"),size = 2) + 
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) + 
  # expand_limits(x = 2007 , y = 0)  + 
  scale_y_continuous(labels = comma) + 
  scale_color_brewer(palette = "Dark2",
                     labels = c("kitchen","laundry room","heating"),
                     name="") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("kW/hour")+ xlab("Quarter") + guides(aes()) + 
  ggtitle("Consumption Year") + 
  facet_wrap(~year) + 
  bbc_style()

# consumption month

test_month <- results_month %>% gather(gather_col,key="lines",value="values")
test_month <- test_month %>% select(month,values,year,lines) %>% 
  filter(year == 2010, values > 10 )
ggplot(data= test_month ,aes(x = month, y= values)) +
  geom_line(aes(color = lines))  
  facet_wrap(~year)


ggplot(data=results_month,aes(x = month)) + 
  geom_line(aes(y = Sub_metering_1_sum,col = "Sub_metering_1_sum"),size = 1) +
  geom_point(aes(y = Sub_metering_1_sum,col = "Sub_metering_1_sum"),size = 2) +  
  geom_line(aes(y = Sub_metering_2_sum,col = "Sub_metering_2_sum"),size = 1) + 
  geom_point(aes(y = Sub_metering_2_sum,col = "Sub_metering_2_sum"),size = 2) + 
  geom_line(aes(y = Sub_metering_3_sum,col = "Sub_metering_3_sum"),size = 1) + 
  geom_point(aes(y = Sub_metering_3_sum,col = "Sub_metering_3_sum"),size = 2) + 
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) + 
  # expand_limits(x = 2007 , y = 0)  + 
  scale_y_continuous(labels = comma) + 
  scale_color_brewer(palette = "Dark2",
                     labels = c("kitchen","laundry room","heating"),
                     name="") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("kW/hour")+ xlab("Quarter") + guides(aes()) + 
  ggtitle("Consumption Year") + 
  bbc_style() + 
  facet_wrap(~year, ncol = 4)

# consumption week

ggplot(data=results_day,aes(x = day)) + 
  # geom_line(aes(y = Sub_metering_1_sum,col = "Sub_metering_1_sum"),
  # size = 1) +
  # geom_point(aes(y = Sub_metering_1_sum,col = "Sub_metering_1_sum"),
  # size = 2) +  
  geom_line(aes(y = consumption_sum,col = "consumption_sum"),size = 1) +
  geom_point(aes(y = consumption_sum,col = "consumption_sum"),size = 2) +  
  # geom_line(aes(y = Sub_metering_2_sum,col = "Sub_metering_2_sum"),
  # size = 1) + 
  # geom_point(aes(y = Sub_metering_2_sum,col = "Sub_metering_2_sum"),
  # size = 2) + 
  # geom_line(aes(y = Sub_metering_3_sum,col = "Sub_metering_3_sum"),
  # size = 1) + 
  # geom_point(aes(y = Sub_metering_3_sum,col = "Sub_metering_3_sum"),
  # size = 2) +
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) + 
  # expand_limits(x = 2007 , y = 0)  + 
  scale_y_continuous(labels = comma) + 
  scale_color_brewer(palette = "Dark2",
                     labels = c("consumption"),
                     name="") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("kW/hour")+ xlab("Day") + guides(aes()) + ggtitle("Consumption",
                                                         "Month")+ 
  facet_grid(year ~ month)


# Plots ricardo -----------------------------------------------------------

gather_col <- c(
  "Sub_metering_1_sum",
  "Sub_metering_2_sum",
  "Sub_metering_3_sum",
  "other_sum",
  "consumption_sum"
)

plot_ricardo1 <- results_day %>% filter(year == 2008,month == 8)
plot_ricardo2 <-results_day %>% filter(year == 2009,month == 1)
plot_ricardo3 <-results_day %>% filter(year == 2010,month == 6)


# view(plot_ricardo1 %>% select(Sub_metering_1_sum))

ggplot(data=plot_ricardo1,aes(x = day,y = Sub_metering_1_sum)) + 
  geom_line(size = 1,colour = "#1380A1") +
  geom_hline(yintercept = 0) + 
  labs(title="Energy Consumption") + 
  bbc_style() + 
  theme(legend.title = element_blank()) + 
  scale_y_continuous(limits=c(0,12),
                     breaks = seq(0, 9, by = 3),
                     labels = c("","20 kWh", "40 kWh", "60 kWh"))


ggplot(data=plot_ricardo2,aes(x = day,y = Sub_metering_1_sum)) + 
  geom_line(size = 1,colour = "#1380A1") +
  geom_hline(yintercept = 0) + 
  labs(title="Energy Consumption") + 
  bbc_style() + 
  theme(legend.title = element_blank()) + 
  scale_y_continuous(limits=c(0,12),
                     breaks = seq(0, 9, by = 3),
                     labels = c("","20 kWh", "40 kWh", "60 kWh"))

ggplot(data=plot_ricardo3,aes(x = day,y = Sub_metering_1_sum)) + 
  geom_line(size = 1,colour = "#1380A1") +
  geom_hline(yintercept = 0) + 
  labs(title="Energy Consumption") + 
  bbc_style() + 
  theme(legend.title = element_blank()) + 
  scale_y_continuous(limits=c(0,12),
                     breaks = seq(0, 9, by = 3),
                     labels = c("","20 kWh", "40 kWh", "60 kWh")) + 
  geom_label(aes(x = 12, y = 9, label = "That wild party\n in your house"), 
               hjust = 0, 
               vjust = 0.5, 
               colour = "#555555", 
               fill = "white", 
               label.size = NA, 
               family="Helvetica", 
               size = 6)+ 
  geom_curve(aes(x = 12, y = 9, xend = 7, yend = 10), 
             colour = "#555555", 
             size=0.5, 
             curvature = +0.2,
             arrow = arrow(length = unit(0.03, "npc")))



# Plots dashboard -----------------------------------------------------------

dashboard <- results_day %>% filter(year == 2010,month == 2)

summarise_at(vars(ploting_cols),
             list(sum=~sum(.)/1000)) -> results_day


vars2 <- c("Sub_metering_1_sum","Sub_metering_2_sum",
           "Sub_metering_3_sum","other_sum")

# vars2 <- c("Sub_metering_1_sum","Sub_metering_2_sum",
#            "Sub_metering_3_sum","consumption_sum")

donut <- results_day %>%
  filter(year == 2010,month == 2) %>% group_by(month) %>% 
  summarise_at(vars(vars2),
               list(sum=~sum(.)))



data_year2 %>% group_by(year,quarter,month,day) %>% 
  summarise_at(vars(ploting_cols),
               list(sum=~sum(.)/1000)) -> results_day


# plot of consumption
ggplot(data=dashboard,aes(x = day,y = cost_sum)) + 
  geom_line(size = 1,colour = "#1380A1") +
  geom_point(size = 2,colour = "#1380A1") +
  geom_hline(yintercept = 0) + 
  labs(title="Daily Energy Cost", 
       subtitle = "February 2010") + 
  bbc_style() + 
  theme(legend.title = element_blank()) + 
  scale_y_continuous(limits=c(0,8),
                     breaks = seq(0, 8, by = 2),
                     labels = c("","2 €", "4 €", "6 €","8 €"))

# cambiamos los nombres y reorganizamos los datos

newnames <- c("Kitchen", "Laundry room","Water-heater and  air-conditioner",
              "Rest of the House")
gather_DASH <- c("Sub_metering_1_sum", "Sub_metering_2_sum",
                 "Sub_metering_3_sum","other_sum")
donut_DASH <- c("Sub_metering_1_sum_sum", "Sub_metering_2_sum_sum",
                 "Sub_metering_3_sum_sum","other_sum_sum")

names(results_day)[names(results_day) %in% gather_DASH] <- newnames

names(donut)[names(donut) %in% donut_DASH] <- newnames

plot_dash_consumptions <- results_day %>% 
filter(year == 2010,month == 2) %>%
  gather(newnames,key="lines",value="values") 

plot_dash_consumptions <- plot_dash_consumptions[,4:8]

table <- results_day %>% 
  filter(year == 2010,month == 2) 

write.csv(table,file = "datatable.csv",sep = ";")

ggplot(data = plot_dash_consumptions, aes(x = day, y = values)) +
  geom_line(aes(color = lines), size = 1) +
  geom_point(aes(color = lines), size = 2) +
  geom_hline(yintercept = 0) + 
  labs(title="Daily Energy Consumption in the house", 
       subtitle = "February 2010") + 
  bbc_style() + 
  theme(legend.title = element_blank()) + 
  scale_y_continuous(limits=c(0,25),
                     breaks = seq(0, 25, by = 5),
                     labels = c("","5 kWh",
                                "10 kWh","15 kWh",
                                "20 kWh","25 kWh")) + 
  scale_x_continuous(limits=c(0,30),
                     breaks = seq(1, 31, by = 5),
                     labels = c("1","5","10","15","20","25","31"))

donut_graph <- donut %>%
  gather(newnames,key="key",value="values") %>% 
  ggplot(aes(x = month, y = values)) +
  geom_col(aes(fill = key), size = 1, color = "white") + 
  coord_polar(theta = "y" ,direction = 1) +
  xlim(c(0,2.5)) + 
  bbc_style() + 
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) 

donut_graph

 donut %>%
  gather(newnames,key="key",value="values") %>% 
  ggplot(aes(x = month, y = values)) +
  geom_col(aes(fill = key), size = 1, color = "white") +
  xlim(c(0,2.5)) + 
  coord_polar("y", start = 0)+
  theme_void()

