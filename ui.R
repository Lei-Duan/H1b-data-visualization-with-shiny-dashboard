
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