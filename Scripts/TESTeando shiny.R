# 1 Dashbord ----------------------------------------------------------------

# USER INTERFACE
ui <- dashboardPage(
  dashboardHeader(title="Energy consumption"),
  
  # _1.1 sidebar --------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(id = "pestaña",
                menuItem("Graphs", tabName = "Graphs", icon = icon("dashboard")),
                menuItem("Tables", tabName = "Tables", icon = icon("align-justify")),
                menuItem("Predictions", tabName = "Predictions",
                         icon =icon("analytics")),
                selectInput(inputId = "Granularity", label = "Select a Time Interval",
                            choices=c("Hour","Day","Week","Month","Year","Historic"))
    )#cierro sidebarMenu
  ), #cierro  dashboardSidebar
  
  # _1.2 body -----------------------------------------------------------------
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Graphs", 
              tabsetPanel(type = "tabs", id = "testeando",
                          tabPanel("Hora",
                                   # First row
                                   fluidRow(
                                     h1(
                                       box(plotlyOutput("Plotcoste1")),
                                       box(plotlyOutput("Plotlines1"))
                                     ),#cierro h1
                                     h2(
                                       box(title = "Energy Distribution",
                                           plotlyOutput("Plotdonut1"))
                                       # box(title = "Values",dataTableOutput("tabla1"))  
                                     )#cierro h2
                                   )#cierro fluidRow
                                   ),
                          tabPanel("Historico",                                    # First row
                                   fluidRow(
                                     verbatimTextOutput("summary")
                                    )#cierro fluidRow)#,
                          )#cierro tabpanel
                          # tabPanel("Table", tableOutput("table"))
              )
      ),#cierro tabItem
      tabItem(tabName = "Tables",
              # First row
              fluidRow(
                dataTableOutput("tabla1") 
              )#cierro fluidRow
      ),#cierro tabItem
      tabItem(tabName = "Predictions",
              fluidRow(
                h1(
                  box(plotlyOutput("Plotcoste2")),
                  box(plotlyOutput("Plotlines2"))
                ),#cierro h1
                h2(
                  box(title = "Energy Distribution",
                      plotlyOutput("Plotdonut2"))
                  # box(title = "Values",dataTableOutput("tabla2"))  
                )#cierro h2
              )#cierro fluidRow
      )#cierro tabItem
    )#cierro tabItems
  )#cierro dashboardBody
)#cierro dashboardPage
# server ------------------------------------------------------------------

# SERVER
server <- function(input, output) { 

  datos <- reactive({

    if(input$pestaña != "Predictions"){
      if(input$Granularity == "Hour"){
        temp <- tail(DataMinute,n = 60)
      }
      if(input$Granularity == "Day"){
        temp <-tail(DataHour,n = 24)
      }
      if(input$Granularity == "Week"){
        temp <-tail(DataDay,n = 7)
      }
      if(input$Granularity == "Month"){
        temp <- tail(DataDay,n = 31)
      }
      if(input$Granularity == "Year"){
        temp <- tail(DataMonth,n = 12)
      }
      if(input$Granularity == "Historic"){
        temp <- DataMonth
      }
    }
    else{
      if(input$Granularity == "Hour"){
        temp <- head(predhour,n = 1)
      }
      if(input$Granularity == "Day"){
        temp <-head(predhour,n = 24)
      }
      if(input$Granularity == "Week"){
        temp <-head(predday,n = 7)
      }
      if(input$Granularity == "Month"){
        temp <- predday
      }
      if(input$Granularity == "Year"){
        temp <- predmonth
      }
      if(input$Granularity == "Historic"){
        temp <- predmonth
      }
    }
    temp
  # 
  })#cerramos reactive

  
  # 1.3 p.coste --------------------------------------------------------

  #plot coste 1 
  output$Plotcoste1 <- renderPlotly({
    ggplotly(
      ggplot(data = datos(),aes(x = DateTime,y = cost)) + 
        geom_line(size = 1,colour = "#1380A1") +
        geom_point(size = 2,colour = "#1380A1") +
        geom_hline(yintercept = 0) + 
        labs(title=  paste("Energy Cost Last",input$Granularity)) + 
        bbc_style() + 
        theme(legend.title = element_blank())
    )
  })
  
  #plot coste 2 
  output$Plotcoste2 <- renderPlotly({ggplotly(
      ggplot(data=datos(),aes(x = DateTime,y = cost)) + 
        geom_line(size = 1,colour = "#1380A1") +
        geom_point(size = 2,colour = "#1380A1") +
        geom_hline(yintercept = 0) + 
        labs(title="Energy Cost Last 24h") + 
        bbc_style() + 
        theme(legend.title = element_blank())
    )
  })
  
  # 1.4 p.lineas --------------------------------------------------------
  
  #Print the plot lines1
  
  output$Plotlines1 <- renderPlotly({
    ggplotly(datos() %>% select(-c("consumption","cost")) %>% 
               gather(key = "Sub_metering", value = "measurement",
                      -c(DateTime,lastunit)) %>% 
               ggplot(aes(x = lastunit) ) +
               geom_line(aes(y= measurement, color = Sub_metering)) + 
               labs(title = "2008-01-09 every 10 min", x="Time",
                    y = "consumption") + 
               scale_fill_continuous(name = "Title") + 
               bbc_style()+ 
               theme( axis.text.x = element_text(angle = 90))
    )
    
  })
  #Print the plot lines2
  
  output$Plotlines2 <- renderPlotly({
    ggplotly(datos() %>% select(-c("consumption","cost")) %>% 
               gather(key = "Sub_metering", value = "measurement",
                      -c(DateTime,lastunit)) %>% 
               ggplot(aes(x = lastunit) ) +
               geom_line(aes(y= measurement, color = Sub_metering)) + 
               labs(title = "2008-01-09 every 10 min", x="Time",
                    y = "consumption") + 
               scale_fill_continuous(name = "Title") + 
               bbc_style()+ 
               theme( axis.text.x = element_text(angle = 90))
    )
    
  })
  
  # 1.5 p.Donut --------------------------------------------------------

  #Print the plot Donut1
  
  output$Plotdonut1 <- renderPlotly({
    donut <- datos() %>% select(-c("consumption", "cost",
                                "DateTime")) %>% 
      summarise_all(sum) %>% 
      gather(key = "labels", value = "values")
    
    donut %>% plot_ly(labels = ~labels, values = ~values) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Donut charts using Plotly",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE,
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE,
                          showticklabels = FALSE))
    
  })
  
  #Print the plot Donut2
  
  output$Plotdonut2 <- renderPlotly({
    donut <- datos() %>% select(-c("consumption", "cost",
                                "DateTime")) %>% 
      summarise_all(sum) %>% 
      gather(key = "labels", value = "values")
    
    donut %>% plot_ly(labels = ~labels, values = ~values) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Donut charts using Plotly",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE,
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE,
                          showticklabels = FALSE))
    
  })
  
  
  # 1.6 T.datos --------------------------------------------------------
  
  # we render the table1 and table2
  output$tabla1 <- renderDataTable ({
    datos()
  })
  output$tabla2 <- renderDataTable ({tail(DataHour,n = 24)})
  
  
  output$summary <- renderPrint({
  datos()
  })
}#cerramos sistem

# RUNNING APP
shinyApp(ui, server)


