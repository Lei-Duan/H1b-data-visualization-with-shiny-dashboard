library(readxl)
library(tidyverse)
library(OIdata)
library(ggmap)
library(viridis)
library(shiny)
library(ggthemes)
library(googleVis)
library(wordcloud2)
library(shinythemes)
library(shinydashboard)
library(RColorBrewer)

## read data
h1b_cl <- readRDS("data/h1b_cl.rds")

#######Ui Part#######

ui <- dashboardPage(
  dashboardHeader(
    title = ("H1b And Salary Across US"), titleWidth = 1800),
  
  dashboardSidebar(
    sidebarMenu(badgeColor = "olive"),
    width = 350,
    sliderInput("map_year", "Year Range", 
                min = 2011, max = 2016,
                round = T,value = c(2011,2016)),
    selectInput('map_job', 'Job Title', 
                c("ALL",levels(factor(h1b_cl$JOB_TITLE)))),
    
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    hr(),
    
    sliderInput("Cf_year", "Year Range", 
                min = 2011, max = 2016,
                round = T,value = c(2011,2016)),
    selectInput('Cf_job', 'Job Title', 
                c("ALL",levels(factor(h1b_cl$JOB_TITLE)))),
    selectInput('Cf_company', 'Top Companies', 
                c("ALL",levels(factor(h1b_cl$state)))),
    selectInput('Cf_state1', 'State1', 
                c(levels(factor(h1b_cl$state))),selected = "CALIFORNIA"),
    selectInput('Cf_state2', 'State2', 
                c(levels(factor(h1b_cl$state))))
  ),
  
  dashboardBody(
    fluidRow(
      box( h3(textOutput("maptitle")),
           htmlOutput("map")
      ),
      box( h3(textOutput("salarytitle")),
           htmlOutput("salary")
      )
    ),
    fluidRow(
      box( h3(textOutput("companytitle")),
           wordcloud2Output('company')
      ),
      box( h3(textOutput("Cftitle")),
           plotOutput("Cf")
      )
    )
  )
 )



#######server Part#######

server <- function(input, output) {
  
  ####reactive####
  
  mapping <- reactive({
    if(input$map_job == "ALL"){
      h1b_cl%>% group_by(state) %>%
        filter(YEAR >= input$map_year[1] & YEAR <= input$map_year[2]) %>%
        summarize(count = n(),
                  AvgSalary = mean(PREVAILING_WAGE[between(PREVAILING_WAGE,quantile(PREVAILING_WAGE,.05,na.rm = T),
                                                           quantile(PREVAILING_WAGE,.95,na.rm =T))],na.rm =T))  }  
    else{
      h1b_cl%>% group_by(state) %>%
        filter(YEAR >= input$map_year[1] & YEAR <= input$map_year[2],JOB_TITLE == input$map_job) %>%
        summarize(count = n(),
                  AvgSalary = mean(PREVAILING_WAGE[between(PREVAILING_WAGE,quantile(PREVAILING_WAGE,.05,na.rm = T),
                                                           quantile(PREVAILING_WAGE,.95,na.rm =T))],na.rm =T))
    }
  })
  
  Cf_state <- reactive({
    if(input$Cf_job == "ALL"){
      h1b_cl%>% group_by(state,YEAR) %>%
        filter(state %in% c(input$Cf_state1, input$Cf_state2),YEAR >= input$Cf_year[1] & YEAR <= input$Cf_year[2]) %>%
        summarize(count = n(),
                  AvgSalary = mean(PREVAILING_WAGE[between(PREVAILING_WAGE,quantile(PREVAILING_WAGE,.05,na.rm = T ),
                                                           quantile(PREVAILING_WAGE,.95,na.rm =T))],na.rm =T))}
    else{
      h1b_cl%>% group_by(state,YEAR,JOB_TITLE) %>%
        filter(state %in% c(input$Cf_state1, input$Cf_state2),YEAR >= input$Cf_year[1] & YEAR <= input$Cf_year[2],JOB_TITLE == input$Cf_job) %>%
        summarize(count = n(),
                  AvgSalary = mean(PREVAILING_WAGE[between(PREVAILING_WAGE,quantile(PREVAILING_WAGE,.05,na.rm = T ),
                                                           quantile(PREVAILING_WAGE,.95,na.rm =T))],na.rm =T))}
  })
  
  
  Cf_top <- reactive({
    if(input$Cf_job == "ALL" & input$Cf_company == "ALL" ){
      a <-h1b_cl%>% group_by(EMPLOYER_NAME) %>%
        subset(YEAR >= input$Cf_year[1] & YEAR <= input$Cf_year[2]) %>%
        summarize(count = n()) %>% 
        arrange(desc(count)) %>% 
        slice(1:100)
      a[1:6,2] <- c(15,12,10,9,8,2)
      a[7:100,2] <- rep(c(1,2),times = 47)
      a}
    else if(input$Cf_job == "ALL" & input$Cf_company != "ALL" ){
      a <-h1b_cl%>% group_by(EMPLOYER_NAME) %>%
        subset(state == input$Cf_company & YEAR >= input$Cf_year[1] & YEAR <= input$Cf_year[2]) %>%
        summarize(count = n()) %>% 
        arrange(desc(count)) %>% 
        slice(1:100)
      a[1:6,2] <- c(15,12,10,9,8,2)
      a[7:100,2] <- rep(c(1,2),times = 47)
      a}
    else if(input$Cf_job != "ALL" & input$Cf_company == "ALL" ){
      a <-h1b_cl%>% group_by(EMPLOYER_NAME) %>%
        subset(YEAR >= input$Cf_year[1] & YEAR <= input$Cf_year[2] & JOB_TITLE == input$Cf_job) %>%
        summarize(count = n()) %>% 
        arrange(desc(count)) %>% 
        slice(1:100)
      a[1:6,2] <- c(15,12,10,9,8,2)
      a[7:100,2] <- rep(c(1,2),times = 47)
      a}
    else{
      a <-h1b_cl%>% group_by(EMPLOYER_NAME) %>%
        subset(state == input$Cf_company & YEAR >= input$Cf_year[1] & YEAR <= input$Cf_year[2] & JOB_TITLE == input$Cf_job) %>%
        summarize(count = n()) %>% 
        arrange(desc(count)) %>% 
        slice(1:100)
      a[1:6,2] <- c(15,12,10,9,8,2)
      a[7:100,2] <- rep(c(1,2),times = 47)
      a}
  })
  
  ####render####
  
  output$maptitle <- renderText({
    paste("Salary level for", input$map_job,"Across US From", input$map_year[1],"to",input$map_year[2] )
  })
  
  output$map <- renderGvis({
    gvisGeoChart(mapping(), locationvar='state', colorvar="AvgSalary",
                 options=list(region='US',
                              displayMode="region",
                              resolution="provinces",
                              backgroundColor.strokeWidth = 2,
                              colorAxis="{colors: ['#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58']}",
                              width = 500,
                              height = 400))    
  })

  output$salarytitle <- renderText({
    paste("Top8 Salary States for",input$map_job,"From",input$map_year[1],"to",input$map_year[2] )
  })
  
  output$salary <- renderGvis({
    gvisBarChart(arrange(mapping(),desc(AvgSalary))[1:8,],"state","AvgSalary",
                 options=list(colors= "['#fd8d3c']",
                              legend.position ='none',fontSize = 15,height = 400))
  })
  
  output$companytitle <- renderText({
    paste("Top companies for",input$Cf_job, "in",input$Cf_company,input$Cf_year[1],"to",input$Cf_year[2] )
  })
  
  output$company <- renderWordcloud2({
    wordcloud2(Cf_top(),size = 0.3)
  })
  
  output$Cftitle <- renderText({
  paste("Salary comparison",":",input$Cf_job,",",input$Cf_state1,"&",input$Cf_state2,",from",input$Cf_year[1],"to", input$Cf_year[2])
  })
  
  output$Cf <- renderPlot({
    Cf_state() %>% 
      ggplot(aes(x = YEAR, y = AvgSalary, group = state)) +
      geom_point(aes(color = state), size =5) +
      geom_line(size=1,aes(color = state),linetype = 5)
  })
}

shinyApp(ui, server)

# library(rsconnect)
# rsconnect::setAccountInfo(name='leiduanshiny',
#                           token='D30DF6149333E88DCB6DEAE04430F0E5',
#                           secret='E+p43N3aNhhIvzaNidXioaRWbn85lkcRhip+YWf+')
