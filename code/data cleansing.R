## set up
library(readxl)
library(tidyverse)
library(OIdata)
library(ggmap)
library(viridis)


################################################ clean and prepare data#########################################
#===================================================================================================================

# load dataset
h1b <- read.csv("h1b_kaggle.csv")

# get R to display unscientific notation in ggplot
options(scipen = 10000)

# trim strings in EMPLOYER_NAME to avoid messy looking in plot
levels(h1b$EMPLOYER_NAME) <- gsub("INC", "", levels(h1b$EMPLOYER_NAME))
levels(h1b$EMPLOYER_NAME) <- gsub("LLP", "", levels(h1b$EMPLOYER_NAME))
levels(h1b$EMPLOYER_NAME) <- gsub("LLC", "", levels(h1b$EMPLOYER_NAME))

# classify data into mainb  categories
h1b$JOB_TITLE <- ifelse(grepl("SOFTWARE", h1b$JOB_TITLE), "SOFTWARE DEVELOPMENT",
                        ifelse(h1b$JOB_TITLE == "BUSINESS ANALYST", "BUSINESS ANALYST",
                               ifelse(grepl("CONSULTANT", h1b$JOB_TITLE), "CONSULTANT",
                                      ifelse(h1b$JOB_TITLE == "TECHNOLOGY ANALYST - US"|
                                               h1b$JOB_TITLE == "PROGRAMMER ANALYST"|
                                               h1b$JOB_TITLE == "SYSTEMS ANALYST" | 
                                               h1b$JOB_TITLE == "COMPUTER SYSTEMS ANALYST",
                                             "DATABASE/DATA SCIENTIST/ANALYST",
                                             ifelse(grepl("PROFESSOR", h1b$JOB_TITLE) | 
                                                      h1b$JOB_TITLE == "RESEARCH ASSOCIATE",
                                                    "PROFESSOR/RESEARCHER",
                                                    ifelse(h1b$JOB_TITLE == "ACCOUNTANT", 
                                                           "ACCOUNTANT", 
                                                           "OTHERS"))))))
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

saveRDS(h1b_cl,"h1b_cl.rds")

