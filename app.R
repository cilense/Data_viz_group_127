library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(tidyr)
library(dplyr)
library(countrycode)
library(scales)
library(ggthemes)
library(hrbrthemes)
library(corrplot)
library(maps)


SS <- read_csv("000001.SS.csv")
DIA <- read_csv("DIA.csv")
FTSE <- read_csv("ISF.L.csv")
QQQ <- read_csv("QQQ.csv")
SPY <- read_csv("SPY.csv")
SPY2 <- read_csv("SPY.csv")
dat <- read.csv('largest_comp_growth.csv')
global <- read.csv("global_index.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Stock Market"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "page1", icon = icon("home")),
      menuItem("Global Market", tabName = "page2", icon = icon("globe")),
      menuItem("Domestic Market", tabName = "page3", icon = icon("dollar-sign")),
      menuItem("Top Companies", icon = icon("chart-line"),
               menuSubItem("Monthly Average Closing Price", tabName = "page4"),
               menuSubItem("Growth", tabName = "page5")
               ),
      menuItem("Data", tabName = "page6", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page1",
              fluidRow(
                box(
                  title = "Introduction", solidHeader = TRUE,
                  status = "success", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span(
                             p("Our group would like to visualize the historical data of various indices and individual stocks in the world stock market and use it as a basis for comparing them with each other in terms of growth."), 
                             p("The project includes stock indices from global markets, U.S. domestic stock indices, and stock performance of the top one hundred U.S. companies in terms of market capitalization", tags$strong("since January 1, 2010 through December 1, 2021."))
                           )
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "About the Datasets", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$strong("Dataset Source"),
                           br(),
                           tags$li(tags$span("Index: "),tags$a(href = "https://finance.yahoo.com/quote/DIA/history?p=DIA", "Yahoo Finance Historical Data")),
                           tags$li(tags$span("Comapanies Stock Price Historical Data: "),tags$a(href = "https://drive.google.com/drive/folders/11YMpUxWXbgnEcHlr3ulf_hj3pojwaAcT?usp=sharing", "Google Drive")),
                           tags$li(tags$span("Largest US Companies: "),tags$a(href = "https://companiesmarketcap.com/usa/largest-companies-in-the-usa-by-market-cap/", "companiesmarketcap.com")),
                           br(),
                           tags$span(
                             "All the datasets include the following columns:"),
                           br(),
                           tags$li("Date"), tags$li("Open"), tags$li("High"), tags$li("Low"), 
                           tags$li("Close"), tags$li("Adj Close"), tags$li("Volume"),
                           br(),
                           tags$span(
                             "For data analysis purpose, we added some columns during the data wrangling process:"),
                           br(),
                           fluidRow(column(6, tags$li("Monthly Average Closing Price"), tags$li("Growth"))),
                           tags$span(
                             "where growth is the increase in the average monthly closing price, or the increase in the first closing price, depending on the purpose of our study."),
                           br(),
                           br(),
                           tags$span("All the original datasets and the cleaned datasets can be downloaded from", tags$a(href = "https://github.com/cilense/Data_viz_group_127", "our project's GitHub page."))
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Teams", solidHeader = TRUE, 
                  status = "info", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           fluidRow(
                             column(3, tags$img(src="carey_logo.png", height=101, width=246, align ="center")),
                             column(9, tags$div("The team members are MSIS students at Johns Hopkins Carey Business School"),
                                    tags$li(tags$strong("Qinghuai Cai: "), "qcai13@jh.edu"),
                                    tags$li(tags$strong("Xijing Cheng: "), "xcheng25@jh.edu"),
                                    tags$li(tags$strong("Jinshuo Bai: "), "jbai19@jh.edu"),
                                    tags$li(tags$strong("Tingxuan Lin: "), "tlin51@jh.edu"))
                             )
                           )
                         )
                  )
                )
              ), 
      tabItem(tabName = "page2",
              h2("Global Indexes Comparison"),
        tabBox(width = 12,
               tabPanel(title = "Index Growth Trend by Month",
                        fluidRow(
                          column(width = 9, plotOutput("plot_global1", height = 550)),
                          column(width = 3, 
                                 sliderInput(inputId = "global_year", label = "Year",
                                             min = 2010, max = 2021, value = 2010,
                                             step = 1, animate = animationOptions(interval = 2000, loop = TRUE)),
                                 checkboxGroupInput(inputId = "index_choice", 
                                                    label = "Please choose one or multiple index(es)...",
                                                    choices = list("SS" = 'SS',
                                                                   "FTSE" = 'FTSE',
                                                                   "SPY" = 'SPY'),
                                                    selected = 'SS'),
                                 
                                 hr(),
                                 p(strong("Comparison")),
                                 p("The growth rate is the first closing price of the month compared to the first closing price of the previous month. 
                                   In the case of January of the second year, it is compared to December of last year."),
                                 p(strong("SS Index (Shanghai)")),
                                 p("A stock market index of all stocks that are traded at the Shanghai Stock Exchange."),
                                 p(strong("S&P 500 (US)")),
                                 p("A stock market index tracking the stock performance of 500 large companies listed on exchanges in the United States."),
                                 p(strong("FTSE 100 (London)")),
                                 p("A share index of the 100 companies listed on the London Stock Exchange with the highest market capitalisation."))
                          )
                        ),
               tabPanel(title = "Overall Growth 2010 - 2021", width = 12, plotOutput("plot_global2", height = 550)))
        ),
      tabItem(tabName = "page3",
              h2("US Indexes comparison"),
              
              fluidRow(
                box(
                  title = "Guide", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                  h4("The page shows the historical data regarding the 3 major ETFs for the US stock market indexes.The main target for the page is to have a comparisonn among the most popular and most significant ETFs.Through the plot below, we are able to see the trend and prices of the ETFs for the past 11 years(2010-2021). Let's find out which is the winner for the past 11 years！")
                ),
                
                box(
                  title = "Findings", solidHeader = TRUE,
                  status = "info", width = 12, collapsible = TRUE, collapsed = FALSE,
                  column(12, 
                         tags$li("Check box(es) to show the price chart for the ETFs"),
                         tags$li("You MAY check multiple boxes"   )        
                  ),
                  column(checkboxInput("QQQ1", label = "Show QQQ", value = TRUE),
                         checkboxInput("SPY1", label = "Show SPY", value = FALSE),
                         checkboxInput("DIA1", label = "Show DIA", value = FALSE),width = 4),
                  
                  br(),
                  
                  fluidRow(
                    column(12,plotlyOutput("plot2", width = 1200,height = 600))),
                  
                  
                  column(6,box(
                    title = "Analysis", collapsible = TRUE,collapsed = TRUE,status = "info",solidHeader = TRUE,width = 12,
                    tags$span("All 3 major ETFs had great growth over the past 11 years. 
                         QQQ,the Nasdaq ETF, has grown 829%.SPY, the S&P500 ETF, has grown 342%. DIA, the Dow Jones Industrial ETF, has grown 260%. Overall, the major technology sector, which is the nasdaq out perform the other 2 ETFs by a huge margin, which is clearly the winner for the past 11 years. ")
                  )),
                  column(6,box(title = "Stock Market News",status = "info",
                               solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                               tags$span(HTML('<iframe width="560" height="360" src="https://www.youtube.com/embed/NzOLOdsTlLQ" 
                   frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')))),
                  
                ),
                
                
                
                
                fluidRow( box(
                  title = "ETF Monthly Price Details",solidHeader = TRUE,
                  status = "primary",width = 12,collapsible = TRUE,collapsed = FALSE,
                  dataTableOutput("myTable")
                )                               )
                
                
                
              ),
      ),
      tabItem(tabName = 'page4',
              h2('Monthly Average Close'),
              fluidRow(
                box(h4('How have the stock prices of the one hundred largest U.S. companies by market capitalization changed today between 2010 and 2021, the time period following the global financial crisis?'),
                    h4('Which are the companies with the best growth rates from an investment perspective?'),
                    solidHeader = TRUE,
                    title = 'Research Question:',
                    width = 12,
                    status = 'primary',
                    collapsible = TRUE,
                    collapsed = TRUE
                )
              ),
              fluidRow(
                box(tags$div(h4(
                  "We combined all the historical data from the US stock market since 1973 and filtered out ",
                  tags$strong("the top 100 companies"),
                  " with the highest market capitalization today, and kept only the data from 2010 to 2021."
                )),
                solidHeader = TRUE,
                title = 'Description of Dataset:',
                width = 12,
                status = 'primary',
                collapsible = TRUE,
                collapsed = TRUE
                )
              ),
              fluidRow(
                box(tags$div(h4(
                  "In this section, we choose largest-comp-growth as our dataset to analyze the company performance. We choose 100 companies and their month-average-close price from 2010 to 2021. The line chart can show the price change in increase. By comparison of their price change, we can figure out their company performance, the larger change in increase, the better performance the company has."
                )),
                solidHeader = TRUE,
                title = 'Analysis:',
                width = 12,
                status = 'primary',
                collapsible = TRUE,
                collapsed = TRUE
                )
              ),
              br(),
              fluidRow(
                column(6,
                       selectizeInput('stock',
                                      'Stocks:',
                                      choices = dat$stock,
                                      selected=1,
                                      multiple = F
                       )
                )
              ),
              br(),
              fluidRow(
                column(6,
                       selectizeInput('year',
                                      'Year:',
                                      choices = dat$Year,
                                      selected=1,
                                      multiple = F
                       )
                )
              ),
              br(),
              fluidRow(column(12,plotlyOutput('plot_average_month')))
      ),
      tabItem(tabName = 'page5',
              h2('Monthly Growth'),
              fluidRow(
                box(h4('How have the stock prices of the one hundred largest U.S. companies by market capitalization changed today between 2010 and 2021, the time period following the global financial crisis?'),
                    h4('Which are the companies with the best growth rates from an investment perspective?'),
                    solidHeader = TRUE,
                    title = 'Research Question:',
                    width = 12,
                    status = 'primary',
                    collapsible = TRUE,
                    collapsed = TRUE
                )
              ),
              fluidRow(
                box(tags$div(h4(
                  "We combined all the historical data from the US stock market since 1973 and filtered out ",
                  tags$strong("the top 100 companies"),
                  " with the highest market capitalization today, and kept only the data from 2010 to 2021."
                )),
                solidHeader = TRUE,
                title = 'Description of Dataset:',
                width = 12,
                status = 'primary',
                collapsible = TRUE,
                collapsed = TRUE
                )
              ),
              fluidRow(
                box(tags$div(h4(
                  "100 companies with a total market value of $6,258 billion; 19 companies with a total market value of $2,968 billion; technology companies with a total market value of $2,968 billion; 10 companies with a total market value of $6,258 billion; $2,876 billion; oil and gas $8,877 billion , with a total value of $2,428 billion; corporate companies, with a total value of $222 billion; 55 telecom companies, with a total value of $782 billion; 555 industrial companies, with a total value of $2,504 billion; utilities $1 billion, with a total value of $1,180; basic materials companies 1 billion dollars.",
                  br(),
                  br(),
                  "According to the growth analysis, the growth of different industries in different time periods is different. In the initial stage after the economic crisis, the economic recovery cannot be separated from infrastructure construction. The corresponding cyclical industries, such as cement and steel, have grown rapidly. , Compared with the growth of technology companies, the growth of technology companies is slightly worse. In the early stage of development, the prospects for future technology development are also uncertain. The strong profitability attribute of value enhancement helps technology companies to start growing rapidly in a situation where the economic situation is good, which also includes factors such as policy support."
                )),
                solidHeader = TRUE,
                title = 'Analysis:',
                width = 12,
                status = 'primary',
                collapsible = TRUE,
                collapsed = TRUE
                )
              ),
              br(),
              fluidRow(
                column(6,
                       selectizeInput('growth_stock',
                                      'Stocks:',
                                      choices = dat$stock,
                                      selected=1,
                                      multiple = F
                       )
                )
              ),
              br(),
              fluidRow(
                column(6,
                       selectizeInput('growth_year',
                                      'Year:',
                                      choices = dat$Year,
                                      selected=1,
                                      multiple = F
                       )
                )
              ),
              br(),
              fluidRow(column(12,plotlyOutput('plot_growth')))
      ),

  
      tabItem(tabName = "page6",
              tabBox(id = NULL, selected = NULL, title = NULL, width = 12,
                     tabPanel("SS", dataTableOutput("mytable1")),
                     tabPanel("DIA", dataTableOutput("mytable2")),
                     tabPanel("FTSE", dataTableOutput("mytable3")),
                     tabPanel("QQQ", dataTableOutput("mytable4")),
                     tabPanel("SPY", dataTableOutput("mytable5")),
                     tabPanel("largest_comp_growth", dataTableOutput("mytable6")),
                     tabPanel("global_index", dataTableOutput("mytable7"))
                     
              )
      )
    )
  )
      
)


server <- function(input, output, session) {

 
# page 2

  
  output$plot_global1 = renderPlot({
    global %>% 
      filter(Year == input$global_year) %>%
      filter(index %in% input$index_choice) %>%
      ggplot(aes(x = Date, y = Growth_rate, group = index, color = index)) + 
      geom_point(size = 4) +
      geom_line(size = 1.5, alpha = 0.7) +
      ylim(-0.25, 0.25) + 
      labs(y = "Growth Rate", x = "First Closing Price of the Month") + 
      annotate("text", x = 10, y = 0.2, size = 20, alpha = 0.3, label = input$global_year) + 
      theme_light() + 
      scale_x_discrete(labels=month.abb)
  })

  output$plot_global2 = renderPlot({
    global %>%
      group_by(index) %>%
      filter(Date == "2010-01-01" | Date == "2021-12-01") %>%
      mutate(difference = (Close - lag(Close))/lag(Close)) %>%
      filter(difference != 0) %>%
      ggplot(aes(x = reorder(index, difference), y = difference, fill = index)) +
      geom_bar(stat='identity', alpha = 0.7) +
      coord_flip() +
      labs(y = "Overall Growth in 11 years", x = "Index") +
      scale_y_continuous(labels = scales::percent) +
      theme_classic()
  })
  
# page 3
  ETF=read.csv("ETF.csv")
  ETFdata=read.csv("ETF1.csv")
  
  ETF$Date=as.Date(ETF$Date,"%Y-%m-%d")
  ETFdata$Date=as.Date(ETFdata$Date,"%Y/%m/%d")
  
  ETFdata$QQQ=round(ETFdata$QQQ, digits = 2)
  ETFdata$SPY=round(ETFdata$SPY, digits = 2)
  ETFdata$DIA=round(ETFdata$DIA, digits = 2)
  
  data_QQQ=filter(ETF, stock =="QQQ")
  data_SPY=filter(ETF, stock =="SPY")
  data_DIA=filter(ETF, stock =="DIA")
  #class(ETF$Date)
  
  
  f1=ETF%>%
    filter(stock =="QQQ")%>%
    ggplot(aes(Date, Close,color=stock)) +
    geom_line() +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    labs(title = "Historical Values",
         x = "Date",
         y = "Value",)
  f1=ggplotly(f1)  
  
  f2=ETF%>%
    filter(stock =="SPY")%>%
    ggplot(aes(Date, Close,color=stock)) +
    geom_line() +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    labs(title = "Historical Values",
         x = "Date",
         y = "Value",)
  f2=ggplotly(f2)
  
  f3=ETF%>%
    filter(stock =="DIA")%>%
    ggplot(aes(Date, Close,color=stock)) +
    geom_line() +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    labs(title = "Historical Values",
         x = "Date",
         y = "Value",)
  f3=ggplotly(f3)
  
  f4=ETF%>%
    filter(stock !="DIA")%>%
    ggplot(aes(Date, Close,color=stock)) +
    geom_line() +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    labs(title = "Historical Values",
         x = "Date",
         y = "Value",)
  f4=ggplotly(f4)
  
  
  f5=ETF%>%
    filter(stock !="SPY")%>%
    ggplot(aes(Date, Close,color=stock)) +
    geom_line() +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    labs(title = "Historical Values",
         x = "Date",
         y = "Value",)
  f5=ggplotly(f5)
  
  f6=ETF%>%
    filter(stock !="QQQ")%>%
    ggplot(aes(Date, Close,color=stock)) +
    geom_line() +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    labs(title = "Historical Values",
         x = "Date",
         y = "Value",)
  f6=ggplotly(f6)
  f6=ETF%>%
    filter(stock !="QQQ")%>%
    ggplot(aes(Date, Close,color=stock)) +
    geom_line() +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    labs(title = "Historical Values",
         x = "Date",
         y = "Value",)
  f6=ggplotly(f6)
  
  f7=ETF%>%
    #filter(stock !="QQQ")%>%
    ggplot(aes(Date, Close,color=stock)) +
    geom_line() +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
    labs(title = "Historical Values",
         x = "Date",
         y = "Value",)
  f7=ggplotly(f7)
  
  
  
  output$myTable = renderDataTable({
    
    d1=datatable(ETFdata)
    
    
    
    
    return(d1)
    
    
  })
  
  
  output$plot2 = renderPlotly({
    
    if(input$QQQ1==FALSE && input$SPY1 ==FALSE && input$DIA1 ==FALSE ){
      
      f=f7
    }
    
    
    if(input$QQQ1==TRUE && input$SPY1 ==FALSE && input$DIA1 ==FALSE ){
      
      f=f1
    }
    
    if(input$QQQ1==FALSE && input$SPY1 ==TRUE && input$DIA1 ==FALSE ){
      
      f=f2
    }
    if(input$QQQ1== FALSE && input$SPY1 ==FALSE && input$DIA1 ==TRUE ){
      
      f=f3
    }
    
    if(input$QQQ1==TRUE && input$SPY1 ==TRUE && input$DIA1 ==FALSE ){
      
      f=f4
    }
    
    if(input$QQQ1==TRUE && input$SPY1 ==FALSE && input$DIA1 ==TRUE ){
      
      f=f5
    }
    if(input$QQQ1==FALSE && input$SPY1 ==TRUE && input$DIA1 ==TRUE ){
      
      f=f6
    }
    
    if(input$QQQ1==TRUE && input$SPY1 ==TRUE && input$DIA1 ==TRUE ){
      
      f=f7
    }
    
    return(f)
    
  })
  
  
# page 4
  output$plot_average_month<-renderPlotly(
    dat%>%filter(stock==input$stock & Year==input$year)%>%mutate(month=1:12)%>%
      ggplot(aes(x=month,y=monthly_average_close))+geom_point()+
      geom_line()+theme_bw()+
      labs(x='Month',y='Monthly Average Close',
           title=paste0('Monthly Average in ',input$year))+
      scale_x_continuous(breaks=1:12,labels=month.abb)
  ) 
  
# page 5
  output$plot_growth<-renderPlotly(
    dat%>%filter(stock==input$growth_stock & Year==input$growth_year)%>%mutate(month=1:12)%>%
      ggplot(aes(x=month,y=Growth))+geom_point()+
      geom_line()+theme_bw()+
      labs(x='Month',y='Monthly Growth',
           title=paste0('Monthly Growth in ',input$growth_year))+
      scale_x_continuous(breaks=1:12,labels=month.abb)
  ) 
  

# page 6  
  output$mytable1 <- renderDataTable({
    datatable(SS, rownames= FALSE)
  })
  
  output$mytable2 <- renderDataTable({
    datatable(DIA, rownames= FALSE)
  })
  
  output$mytable3 <- renderDataTable({
    datatable(FTSE, rownames= FALSE)
  })
  
  output$mytable4 <- renderDataTable({
    datatable(QQQ, rownames= FALSE)
  })
  
  output$mytable5 <- renderDataTable({
    datatable(SPY, rownames= FALSE)
  })
  
  output$mytable6 <- renderDataTable({
    datatable(dat, rownames= FALSE)
  })
  
  output$mytable7 <- renderDataTable({
    datatable(global, rownames= FALSE)
  })
  
  
  
}

shinyApp(ui = ui, server = server)

