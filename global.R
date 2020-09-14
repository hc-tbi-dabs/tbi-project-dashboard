
# dependencies ----
library(shiny)
library(shinymanager)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(purrr)
library(tidyr)
library(data.table)
library(stringi)
library(plotly)
library(readxl)
library(scales)
library(magrittr)
library(DT)
library(ggrepel)
library(devtools)
# library(shinyauthr)
library(shinyjs)
library(lubridate)
library(orca)
library(processx)
library(rsconnect)
library(webshot)
library(timevis)

credentials <- data.frame(
  user = c("dabs"),
  password = c("TBI"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)

# all raw data from dattbi.xlsx ----
source('functions.R')
description <- read_excel('dattbi.xlsx',1)
schedule <- read_excel('dattbi.xlsx',2)
budget <- read_excel('dattbi.xlsx',3)
budget_yr <- read_excel('dattbi.xlsx',4)
status <- read_excel('dattbi.xlsx',5) # all_proj in old
all_proj <- read_excel('dattbi.xlsx',5)
proj_risk <- read_excel('dattbi.xlsx',6)
proj_issue <- read_excel('dattbi.xlsx',7)
capital <- read_excel('dattbi.xlsx',8)

# removing IPA01 from descriptions, this is DABs and have no schedule, budget, risk or issues
# won't show in individual
description <- description %>%
  filter(!(description$IP=="A01"))

# date of which data is updated ----
dat<-substring(file.info('dattbi.xlsx')$mtime,1,11)


# data cleaning removed all rows with any NA observations----
# systematic
nona_description <- description %>%
  drop_na()
nona_schedule <- schedule %>%
  drop_na()
nona_budget <- budget %>%
  drop_na()
nona_budget_yr <- budget_yr %>%
  drop_na()
nona_status <- status %>%
  drop_na()
nona_proj_risk <- proj_risk %>%
  drop_na()
nona_proj_issue <- proj_issue %>%
  drop_na()
nona_capital <- capital %>%
  drop_na()

# formatting dates ----
schedule$Actual_date<-as.Date(as.character(schedule$Actual_date))
schedule$Approved_finish_date<-as.Date(as.character(schedule$Approved_finish_date))
proj_issue$`Target Date for Resolution`<-as.Date(as.character(proj_issue$`Target Date for Resolution`))

# pasting schedule IP to major milestone blurb ----
schedule$Major.Milestone<-paste0(schedule$IP,":",schedule$Major.Milestone)
schedule$Major.Milestone.Standard<-paste0(schedule$Project,":",schedule$Major.Milestone.Standard)

# list of ip and directorate ----
ip<-c(description$IP[!is.na(description$IP)])
directorate<-c('All') # Martin wants it to only say all

# status cleaning ----  
status$`Overall Project Health`[is.na(status$`Overall Project Health`)]<-'Blue'
status$status[is.na(status$status)]<-'Not yet started'

# capital ----
capital$IP <- as.character(capital$IP)
capital$`Authority vs. Expenditures`<-'Project Authority'

# data budget ----
budget_yr%<>%left_join(capital)
budget_yr$Value<-ifelse(is.na(budget_yr$Value),0,budget_yr$Value)
budget_yr$Capital<-ifelse(is.na(budget_yr$Capital),0,budget_yr$Capital)
budget_yr$Non_Capital<-budget_yr$Value-budget_yr$Capital
budget_yr$year<-as.numeric(substr(budget_yr$Year,1,4))

# sorted budget ----
# for reporting purposes
sorted_budget<-budget[order(-budget$`Approved Budget`),]

# schedule data wrangling ----

no_completed_schedule <- schedule[!grepl("completed", schedule$Schedule.Health.Standard),]

# count completed, covid delayed ----

schedule_completed <- schedule%>%filter(grepl('completed',Schedule.Health.Standard,ignore.case=T))
covid_delayed <- schedule%>%filter(grepl('covid',Schedule.Health,ignore.case=T))

# project stages counts ----

stage_1 <- status%>%filter(grepl('1',stage,ignore.case=T))
stage_2 <- status%>%filter(grepl('2',stage,ignore.case=T))
stage_3 <- status%>%filter(grepl('3',stage,ignore.case=T))
stage_4 <- status%>%filter(grepl('4',stage,ignore.case=T))
planning <- status%>%filter(grepl('planning',stage,ignore.case=T))