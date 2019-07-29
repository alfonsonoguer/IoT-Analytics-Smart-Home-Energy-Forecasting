# Libraries ---------------------------------------------------------------

#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(rstudioapi, shiny, shinydashboard, DT, dplyr, highcharter,
               RMySQL,caret,tidyverse,ggplot2,chron,lubridate,RColorBrewer,
               scales, reshape,devtools,bbplot,colorspace,plotly,padr,imputeTS,
               plotly,forecast)


# Load the Data -----------------------------------------------------------



con = dbConnect(MySQL(), user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# dbListFields(con,"yr_2006")

data_year <- as_tibble(bind_rows(
  dbGetQuery(con, "SELECT Date,Time,Global_active_power,Sub_metering_1,
                       Sub_metering_2,Sub_metering_3 FROM yr_2007"),
  dbGetQuery(con, "SELECT Date,Time,Global_active_power,Sub_metering_1,
                       Sub_metering_2,Sub_metering_3 FROM yr_2008"),
  dbGetQuery(con, "SELECT Date,Time,Global_active_power,Sub_metering_1,
                       Sub_metering_2,Sub_metering_3 FROM yr_2009"),
  dbGetQuery(con, "SELECT Date,Time,Global_active_power,Sub_metering_1,
                       Sub_metering_2,Sub_metering_3 FROM yr_2010")))

# Parse the data ----------------------------------------------------------
names(data_year)[4:6] <- c("Kitchen","Laundry_Room","WH_AC")
## Combine Date and Time attribute values in a new attribute column
data_year <-cbind(data_year,paste(data_year$Date,data_year$Time),
                  stringsAsFactors=FALSE)
data_year <- as_tibble(data_year)
colnames(data_year)[7] <-"DateTime"

data_year <- data_year[,c(ncol(data_year), 1:(ncol(data_year)-1))]

data_year$DateTime <- ymd_hms(data_year$DateTime)
# lenght_NA <- length(data_year$DateTime)


# fill the NA and data manipulation---------------------------------------

data_year<-pad(data_year,break_above = 3)
data_year[,-(1:3)] <- sapply(data_year[,-(1:3)],function(x){ na_locf(x)})
# lenght_no_NA <- length(data_year$DateTime)
task2_data<- data_year %>% select(-c(2,3)) %>% 
  mutate(consumption = Global_active_power*1000/60,
         cost= consumption*0.0001472 ,
         other = consumption - Kitchen - 
           Laundry_Room - WH_AC,) %>% select(-Global_active_power)

task2_data[,-c(1,6)] <- round(task2_data[,-c(1,6)],digits = 0)


# Quartering the data -----------------------------------------------------

#Month
DataMonth <- task2_data
DataMonth$DateTime <- floor_date(task2_data$DateTime, unit = "month")
DataMonth <- DataMonth %>% group_by(DateTime) %>% summarise_all(sum) %>% 
  mutate(lastunit = date(DateTime))

#Day
DataDay <- task2_data
DataDay$DateTime <-floor_date(task2_data$DateTime,unit = "day")
DataDay <- DataDay %>% group_by(DateTime) %>% summarise_all(sum)  %>% 
  mutate(lastunit = day(DateTime))

#Hour
DataHour <- task2_data
DataHour$DateTime <-floor_date(task2_data$DateTime,unit = "hour")
DataHour <- DataHour %>% group_by(DateTime) %>%
  summarise_all(sum)  %>% 
  mutate(lastunit = hour(DateTime))

#Minute
DataMinute <- task2_data %>% mutate(lastunit = minute(DateTime))

# # Graphs ------------------------------------------------------------------
# 
# 
# # price
# ggplotly(
#   ggplot(data=tail(DataHour,n = 24),aes(x = DateTime,y = cost)) + 
#     geom_line(size = 1,colour = "#1380A1") +
#     geom_point(size = 2,colour = "#1380A1") +
#     geom_hline(yintercept = 0) + 
#     labs(title="Energy Cost Last 24h") + 
#     bbc_style() + 
#     theme(legend.title = element_blank())
# )
# # Consumption by LINES
# 
# tail(DataHour,n = 24) %>% select(-c("consumption","cost")) %>% 
#   gather(key = "Sub_metering", value = "measurement",-DateTime)
# 
# ggplotly(tail(DataHour,n = 24) %>% select(-c("consumption","cost")) %>% 
#            gather(key = "Sub_metering", value = "measurement",-DateTime) %>% 
#            ggplot(aes(x = DateTime) ) +
#            geom_line(aes(y= measurement, color = Sub_metering)) + 
#            labs(title = "2008-01-09 every 10 min", x="Time",
#                 y = "consumption") + 
#            scale_fill_continuous(name = "Title") + 
#            bbc_style()+ 
#           theme( axis.text.x = element_text(angle = 90))
# ) 
# 
# 
# 
# 
# #donut 
# 
# donut <- tail(DataHour,n = 24) %>% select(-c("consumption","cost",
#                                              "DateTime")) %>% 
#   summarise_all(sum) %>% 
#   gather(key = "labels", value = "values")
# 
# donut %>% plot_ly(labels = ~labels, values = ~values) %>%
#   add_pie(hole = 0.5) %>%
#   layout(title = "Donut charts using Plotly",  showlegend = F,
#      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# 

# Predictions -------------------------------------------------------------
negativeto0 <- function(x)
{
  if(x<0){
    return(0)
  }
  else{
    return(x)
  }
}
# MONTH

predmonth <- tail(DataMonth,n = 12)
month(predmonth$DateTime) <- month(predmonth$DateTime) + 12 

for (i in 2:(length(DataMonth)-1)) {
  ts_month <- ts(data = DataMonth[,i],
                 start = c(2006,12), frequency = 12)
  modelmonth <- HoltWinters(ts_month)
  predictions<-forecast(modelmonth,h = 12)
  predmonth[,i] <- as.numeric(predictions$mean) 
}

# DAY
data <- DataHour
predday <- tail(data,n = 30)
day(predday$DateTime) <- day(predday$DateTime) + 30 
for (i in 2:(length(data)-1)) {
  ts <- ts(data = data[,i],
           start = c(2006,12), frequency = 365)
  model <- HoltWinters(ts)
  predictions<-forecast(model,h = 30)
  predday[,i] <- as.numeric(predictions$mean) 
}
cols <- !names(predday) %in% c("lastunit","DateTime")
predday[,cols] <-  apply(predday[,cols], 1:2, negativeto0)
# HOUR
data <- DataHour
predhour <- tail(data,n = 24)
hour(predhour$DateTime) <- hour(predhour$DateTime) + 24
for (i in 2:(length(data)-1)) {
  ts <- ts(data = data[,i],
           start = c(2006,12), frequency = 8766)
  model <- HoltWinters(ts)
  predictions<-forecast(model,h = 24)
  predhour[,i] <- as.numeric(predictions$mean) 
}
cols <- !names(predhour) %in% c("lastunit","DateTime")
predhour[,cols] <-  apply(predhour[,cols], 1:2, negativeto0)
# # minute
# data <- DataMinute
# predmin <- tail(data,n = 60)
# day(predmin$DateTime) <- minute(predmin$DateTime) + 60
# for (i in 2:(length(data)-1)) {
#   ts <- ts(data = data[,i],
#            start = c(2006,12), frequency = 525960)
#   model <- HoltWinters(ts)
#   predictions<-forecast(model,h = 60)
#   predmin[,i] <- as.numeric(predictions$mean) 
# }


# 1 Dashbord ----------------------------------------------------------------

# USER INTERFACE
ui2 <- dashboardPage(
  dashboardHeader(title="Energy consumption"),
  
  # _1.1 sidebar --------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(id = "pestaña",
                menuItem("Graphs", tabName = "Graphs",
                         icon = icon("dashboard")),
                menuItem("Tables", tabName = "Tables",
                         icon = icon("align-justify")),
                menuItem("Predictions", tabName = "Predictions",
                         icon =icon("analytics"))
    )#cierro sidebarMenu
  ), #cierro  dashboardSidebar
  
  # _1.2 body -----------------------------------------------------------------
  
  dashboardBody(
    tabItems(

#   _1.2.1 body graphs----------------------------------------------------------

      tabItem(tabName = "Graphs", 
              tabsetPanel(type = "tabs", id = "tabsgraficos",
                          tabPanel("Hour",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_Hour1")),
                                       box(plotlyOutput("Plotlines_Hour1"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_Hour1"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          ),
                          tabPanel("Day",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_Day1")),
                                       box(plotlyOutput("Plotlines_Day1"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_Day1"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          ),
                          tabPanel("Week",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_Week1")),
                                       box(plotlyOutput("Plotlines_Week1"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_Week1"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          ),
                          tabPanel("Month",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_Month1")),
                                       box(plotlyOutput("Plotlines_Month1"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_Month1"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          ),
                          tabPanel("Year",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_Year1")),
                                       box(plotlyOutput("Plotlines_Year1"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_Year1"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          ),
                          tabPanel("Historic",
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_hist1")),
                                       box(plotlyOutput("Plotlines_hist1"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_hist1"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          )#cierro tabpanel
              )#cierro tabsetPanel
      ),#cierro tabItem

#   _1.2.2 body Tables----------------------------------------------------------

      tabItem(tabName = "Tables",
              tabsetPanel(type = "tabs", id = "tabstablas",
                          tabPanel("Hour",
                                   # First row
                                   fluidRow(
                                     dataTableOutput("tablaHour") 
                                   )#cierro fluidRow
                          ),
                          tabPanel("Day",
                                   # First row
                                   fluidRow(
                                     dataTableOutput("tablaDay") 
                                   )#cierro fluidRow
                          ),
                          tabPanel("Week",
                                   # First row
                                   fluidRow(
                                     dataTableOutput("tablaWeek") 
                                   )#cierro fluidRow
                          ),
                          tabPanel("Month",
                                   # First row
                                   fluidRow(
                                     dataTableOutput("tablaMonth") 
                                   )#cierro fluidRow
                          ),
                          tabPanel("Year",
                                   # First row
                                   fluidRow(
                                     dataTableOutput("tablaYear") 
                                   )#cierro fluidRow
                          ),
                          tabPanel("Historic",
                                   fluidRow(
                                     dataTableOutput("tablaHist") 
                                   )#cierro fluidRow
                          )#cierro tabpanel
              )#cierro tabsetPanel
      ),#cierro tabItem

#   _1.2.1 body predictions-----------------------------------------------------

      tabItem(tabName = "Predictions",
              tabsetPanel(type = "tabs", id = "tabspredictions",
                          tabPanel("Week",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_Week3")),
                                       box(plotlyOutput("Plotlines_Week3"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_Week3"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          ),
                          tabPanel("Month",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_Month3")),
                                       box(plotlyOutput("Plotlines_Month3"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_Month3"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          ),
                          tabPanel("Year",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste_Year3")),
                                       box(plotlyOutput("Plotlines_Year3"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut_Year3"))
                                     )#cierro h2
                                   )#cierro fluidRow
                          )
              )#cierro tabset  
      )#cierro tabItem
    )#cierro tabItems
  )#cierro dashboardBody
)#cierro dashboardPage

# server ------------------------------------------------------------------
# SERVER
server2 <- function(input, output) { 
  datos <- reactive({
    if(input$pestaña != "Predictions"){
      if(input$tabsgraficos == "Hour"){
        temp <- tail(DataMinute,n = 60)
      }
      if(input$tabsgraficos == "Day"){
        temp <-tail(DataHour,n = 24)
      }
      if(input$tabsgraficos == "Week"){
        temp <-tail(DataDay,n = 7)
      }
      if(input$tabsgraficos == "Month"){
        temp <- tail(DataDay,n = 31)
      }
      if(input$tabsgraficos == "Year"){
        temp <- tail(DataMonth,n = 12)
      }
      if(input$tabsgraficos == "Historic"){
        temp <- DataMonth
      }
    }
    else{
      if(input$tabspredictions == "Week"){
        temp <-head(predday,n = 7)
      }
      if(input$tabspredictions == "Month"){
        temp <- predday
      }
      if(input$tabspredictions == "Year"){
        temp <- predmonth
      }
    }
    return(temp) 
    # 
  })
  tablaDatos <- reactive({
    if(input$pestaña != "Predictions"){
      if(input$tabstablas == "Hour"){
        temp <- tail(DataMinute,n = 60)
      }
      if(input$tabstablas == "Day"){
        temp <-tail(DataHour,n = 24)
      }
      if(input$tabstablas == "Week"){
        temp <-tail(DataDay,n = 7)
      }
      if(input$tabstablas == "Month"){
        temp <- tail(DataDay,n = 31)
      }
      if(input$tabstablas == "Year"){
        temp <- tail(DataMonth,n = 12)
      }
      if(input$tabstablas == "Historic"){
        temp <- DataMonth
      }
    }
    temp
    # 
  })
  # 1.3 p.coste --------------------------------------------------------
  
  #plot coste 1 
  ggplotlycoste_graficos <- 
    reactive({ 
      ggplotly(
        ggplot(data = datos(),aes(x = DateTime,y = cost)) + 
          geom_line(size = 1,colour = "#1380A1") +
          geom_point(size = 2,colour = "#1380A1") +
          geom_hline(yintercept = 0) + 
          labs(title=  paste("Energy Cost Last", input$tabsgraficos)) + 
          bbc_style() + 
          theme(legend.title = element_blank())
        )
    })
  
  output$Plotcoste_Hour1 <- renderPlotly({ ggplotlycoste_graficos() })
  output$Plotcoste_Day1 <- renderPlotly({ ggplotlycoste_graficos() })
  output$Plotcoste_Week1 <- renderPlotly({ ggplotlycoste_graficos() })
  output$Plotcoste_Month1 <- renderPlotly({ ggplotlycoste_graficos() })
  output$Plotcoste_Year1 <- renderPlotly({ ggplotlycoste_graficos() })
  output$Plotcoste_hist1 <- renderPlotly({ ggplotlycoste_graficos() })
  
  #plot coste 2 
  ggplotlycoste_pred <- 
    reactive({ 
      ggplotly(
        ggplot(data = datos(),aes(x = DateTime,y = cost)) + 
          geom_line(size = 1,colour = "#1380A1") +
          geom_point(size = 2,colour = "#1380A1") +
          geom_hline(yintercept = 0) + 
          labs(title=  paste("Energy Next", input$tabspredictions)) + 
          bbc_style() + 
          theme(legend.title = element_blank())
      )
    })
  
  output$Plotcoste_Week3 <- renderPlotly({ ggplotlycoste_pred() })
  output$Plotcoste_Month3 <- renderPlotly({ ggplotlycoste_pred() })
  output$Plotcoste_Year3 <- renderPlotly({ ggplotlycoste_pred() })
  
  # 1.4 p.lineas --------------------------------------------------------
  
  #Print the plot lines1
  ggplotlylineas_graficos <- 
    reactive({ 
    ggplotly(datos() %>% select(-c("consumption","cost")) %>% 
               gather(key = "Sub_metering", value = "measurement",
                      -c(DateTime,lastunit)) %>% 
               ggplot(aes(x = lastunit) ) +
               geom_line(aes(y= measurement, color = Sub_metering)) + 
               labs(title=  paste("Energy Cost Last", input$tabsgraficos),
                    x="Time", y = "consumption") + 
               scale_fill_continuous(name = "Title") + 
               bbc_style()+ 
               theme( axis.text.x = element_text(angle = 90))
    )
  })
  output$Plotlines_Hour1 <- renderPlotly({  ggplotlylineas_graficos() })
  output$Plotlines_Day1 <- renderPlotly({   ggplotlylineas_graficos() })
  output$Plotlines_Week1 <- renderPlotly({  ggplotlylineas_graficos() })
  output$Plotlines_Month1 <- renderPlotly({ ggplotlylineas_graficos() })
  output$Plotlines_Year1 <- renderPlotly({  ggplotlylineas_graficos() })
  output$Plotlines_hist1 <- renderPlotly({  ggplotlylineas_graficos() })
  
  #Print the plot lines2
  
  ggplotlylineas_predictions <- 
    reactive({ 
      ggplotly(datos() %>% select(-c("consumption","cost")) %>% 
                 gather(key = "Sub_metering", value = "measurement",
                        -c(DateTime,lastunit)) %>% 
                 ggplot(aes(x = lastunit) ) +
                 geom_line(aes(y= measurement, color = Sub_metering)) + 
                 labs(title=  paste("Energy Cost Next", input$tabspredictions),
                      x="Time", y = "consumption") + 
                 scale_fill_continuous(name = "Title") + 
                 bbc_style()+ 
                 theme( axis.text.x = element_text(angle = 90))
      )
    })
  output$Plotlines_Week3 <- renderPlotly({ ggplotlylineas_predictions() })
  output$Plotlines_Month3<- renderPlotly({ ggplotlylineas_predictions() })
  output$Plotlines_Year3 <- renderPlotly({ ggplotlylineas_predictions() })
  
  
  # 1.5 p.Donut --------------------------------------------------------
  
  #Print the plot Donut1
  ggplotlyDonut_graficos <- 
    reactive({ 
      datos() %>% select(-c("consumption", "cost","DateTime","lastunit")) %>% 
        summarise_all(sum) %>% 
        gather(key = "labels", value = "values") %>% 
        plot_ly(labels = ~labels, values = ~values) %>%
        add_pie(hole = 0.5) %>%
        layout( showlegend = F,# title = "Donut charts using Plotly", 
               xaxis = list(showgrid = FALSE, zeroline = FALSE,
                            showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE,
                            showticklabels = FALSE))
      
    })

  output$Plotdonut_Hour1 <- renderPlotly({  ggplotlyDonut_graficos() })
  output$Plotdonut_Day1 <- renderPlotly({   ggplotlyDonut_graficos() })
  output$Plotdonut_Week1 <- renderPlotly({  ggplotlyDonut_graficos() })
  output$Plotdonut_Month1 <- renderPlotly({ ggplotlyDonut_graficos() })
  output$Plotdonut_Year1 <- renderPlotly({  ggplotlyDonut_graficos() })
  output$Plotdonut_hist1 <- renderPlotly({  ggplotlyDonut_graficos() })

  
  #Print the plot Donut2
  
  ggplotlyDonut_pred <- 
    reactive({ 
      datos() %>% select(-c("consumption", "cost","DateTime","lastunit")) %>% 
        summarise_all(sum) %>% 
        gather(key = "labels", value = "values") %>% 
        plot_ly(labels = ~labels, values = ~values) %>%
        add_pie(hole = 0.5) %>%
        layout(title = "Donut charts using Plotly",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE,
                            showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE,
                            showticklabels = FALSE))
      
    })

  output$Plotdonut_Week3 <- renderPlotly({ ggplotlyDonut_pred() })
  output$Plotdonut_Month3<- renderPlotly({ ggplotlyDonut_pred() })
  output$Plotdonut_Year3 <- renderPlotly({ ggplotlyDonut_pred() })
  
  # 1.6 T.datos --------------------------------------------------------
  
  # we render the table1 and table2
  output$tablaHour <- renderDataTable  ({ tablaDatos() })
  output$tablaDay <- renderDataTable   ({ tablaDatos() })
  output$tablaWeek <- renderDataTable  ({ tablaDatos() })
  output$tablaMonth <- renderDataTable ({ tablaDatos() })
  output$tablaYear <- renderDataTable  ({ tablaDatos() })
  output$tablaHist <- renderDataTable  ({ tablaDatos() })
  
}#cerramos sistem

# RUNNING APP
shinyApp(ui2, server2)


