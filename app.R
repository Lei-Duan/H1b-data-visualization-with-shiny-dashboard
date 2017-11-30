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
#################################################### clean and prepare data#########################################
#===================================================================================================================

# load dataset
h1b <- read.csv("h1b_kaggle.csv")

# get R to display unscientific notation in ggplot
options(scipen = 10000)

# trim strings in EMPLOYER_NAME to avoid messy looking in plot
levels(h1b$EMPLOYER_NAME) <- gsub("INC", "", levels(h1b$EMPLOYER_NAME))
levels(h1b$EMPLOYER_NAME) <- gsub("LLP", "", levels(h1b$EMPLOYER_NAME))
levels(h1b$EMPLOYER_NAME) <- gsub("LLC", "", levels(h1b$EMPLOYER_NAME))

# classify data into main categories
h1b$JOB_TITLE <- ifelse(grepl("SOFTWARE", h1b$JOB_TITLE), "SOFTWARE DEVELOPMENT",
                        ifelse(h1b$JOB_TITLE == "BUSINESS ANALYST", "BUSINESS ANALYST", 
                               ifelse(grepl("CONSULTANT", h1b$JOB_TITLE), "CONSULTANT",
                                      ifelse(h1b$JOB_TITLE == "TECHNOLOGY ANALYST - US" | h1b$JOB_TITLE == "PROGRAMMER ANALYST"|
                                               h1b$JOB_TITLE == "SYSTEMS ANALYST" | h1b$JOB_TITLE == "COMPUTER SYSTEMS ANALYST", 
                                             "DATABASE/DATA SCIENTIST/ANALYST",
                                             ifelse(grepl("PROFESSOR", h1b$JOB_TITLE) | h1b$JOB_TITLE == "RESEARCH ASSOCIATE", 
                                                    "PROFESSOR/RESEARCHER",
                                                    ifelse(h1b$JOB_TITLE == "ACCOUNTANT", "ACCOUNTANT", "OTHERS"))))))
## Split WORKSITE in to city and state
h1b <- separate(h1b,WORKSITE, into = c("city","state"), sep = ",")
h1b$state <- tolower(h1b$state) %>% str_trim()
h1b$city <- tolower(h1b$city) %>% str_trim()


# Determine No. of NAs and condense dataset by removing all NAs
sum(is.na(h1b)) # 228,569 NAs in total

h1b_cl <- h1b  %>%
  select(EMPLOYER_NAME, lon, lat, city, state, JOB_TITLE, YEAR, PREVAILING_WAGE) %>% 
  group_by(EMPLOYER_NAME) %>% 
  na.omit(lon) %>% 
  na.omit(lat) %>% 
  na.omit(JOB_TITLE)

# NAs check
sum(is.na(h1b_cl)) # 0 NAs

# saveRDS(h1b_cl,"h1b_cl.rds")

###############################################Plot data with Shiny package#########################################
#===================================================================================================================
read.
#######Ui Part#######

ui <- dashboardPage(
  # theme = shinytheme("sketchy"),
  dashboardHeader(
    title = ("H1b And Salary Across US"), titleWidth = 1800
    # h4("Group 4: Lei Duan, Qingqing Long, Xiaochen Li, Nehal Jain")
  ),
  
  dashboardSidebar(
    sidebarMenu(badgeColor = "olive"),
    width = 350,
    sliderInput("map_year", "Year Range", min = 2011, max = 2016,round = T,value = c(2011,2016)),
    selectInput('map_job', 'Job Title', c("ALL",levels(factor(h1b_cl$JOB_TITLE)))),
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    hr(),
    sliderInput("Cf_year", "Year Range", min = 2011, max = 2016,round = T,value = c(2011,2016)),
    selectInput('Cf_job', 'Job Title', c("ALL",levels(factor(h1b_cl$JOB_TITLE)))),
    selectInput('Cf_state1', 'State1', c(levels(factor(h1b_cl$state)))),
    selectInput('Cf_state2', 'State2', c(levels(factor(h1b_cl$state)))),
    selectInput('Cf_company', 'Top Companies', c("ALL",levels(factor(h1b_cl$state))))
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
    paste("Application amount Across US From", input$map_year[1],"to",input$map_year[2] )
  })
  
  output$map <- renderGvis({
    gvisGeoChart(mapping(), locationvar='state', colorvar="count",
                 options=list(region='US',displayMode="region",resolution="provinces",backgroundColor.strokeWidth = 2,
                              colorAxis="{colors: ['#9ECAE1','#4292C6', '#2171B5', '#08519C','#08306B']}"))    
  })
  
  output$salarytitle <- renderText({
    paste("Top8 Salary States for",input$map_job,"From",input$map_year[1],"to",input$map_year[2] )
  })
  
  output$salary <- renderGvis({
    gvisBarChart(arrange(mapping(),desc(AvgSalary))[1:8,],"state","AvgSalary",
                 options=list(colors = "['#dd3497']",fontSize = 15,height = 350))
  })
  # ggplot() + 
  #   # geom_polygon(aes(x = lon, y = lat, fill = AvgSalary,group = group)) +
  #   # scale_fill_viridis(discrete = F,name = "H1b",option = "D",begin = 1, end = 0.3) +
  #   geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), col = "white", fill = "grey90") +
  #   geom_point(aes(x = lon, y = lat, size = count,color = AvgSalary),data = mapping(), show.legend = TRUE) +
  #   scale_size_area(max_size = 12 )+
  #   scale_colour_gradient(low = "#8E0F2E", high = "darkblue") +
  #   labs(title = "Application Amount and Salary Across US",
  #        subtitle = paste("From",input$map_year[1],"to", input$map_year[2])) + 
  #   theme_void()+
  #   theme(plot.title = element_text(size=30,colour = "black",face = "bold",vjust = 0.5, hjust = 0.5),
  #         plot.subtitle = element_text(size=15,colour = "black",face = "bold",vjust = 0.5, hjust = 0.5))
  
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

