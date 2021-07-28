#
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
library(shinyjs)
library(lubridate)
library(orca)
library(processx)
library(rsconnect)
library(webshot)
library(timevis)
library(lubridate)
library(rmarkdown)
library(htmltools)
library(shinydashboard)
library(shinydashboardPlus)
library(timevis)
library(webshot)
library(scales)
library(shinythemes)
library(dashboardthemes)


source('functions.R')


#' CSS Colors:
#'
#' white         	#FFFFFF 	rgb(255,255,255)
#' snow 	        #FFFAFA 	rgb(255,250,250)
#' honeydew     	#F0FFF0 	rgb(240,255,240)
#' mintcream     	#F5FFFA 	rgb(245,255,250)
#' azure 	        #F0FFFF 	rgb(240,255,255)
#' aliceblue 	    #F0F8FF 	rgb(240,248,255)
#' ghostwhite 	  #F8F8FF 	rgb(248,248,255)
#' whitesmoke 	  #F5F5F5 	rgb(245,245,245)
#' seashell 	    #FFF5EE 	rgb(255,245,238)
#' beige 	        #F5F5DC 	rgb(245,245,220)
#' oldlace 	      #FDF5E6 	rgb(253,245,230)
#' floralwhite 	  #FFFAF0 	rgb(255,250,240)
#' ivory 	        #FFFFF0 	rgb(255,255,240)
#' antiquewhite 	#FAEBD7 	rgb(250,235,215)
#' linen 	        #FAF0E6 	rgb(250,240,230)
#' lavenderblush 	#FFF0F5 	rgb(255,240,245)
#' mistyrose 	    #FFE4E1 	rgb(255,228,225)
#'


credentials <- data.frame(
  user             = c("dabs"),
  password         = c("TBI"),
  stringsAsFactors = F
)


description <- read_excel('dattbi-nov-18-2020.xlsx', 1)
schedule    <- read_excel('dattbi-nov-18-2020.xlsx', 2)
budget      <- read_excel('dattbi-nov-18-2020.xlsx', 3)
budget_yr   <- read_excel('dattbi-nov-18-2020.xlsx', 4)
status      <- read_excel('dattbi-nov-18-2020.xlsx', 5) #' all_proj in old
all_proj    <- read_excel('dattbi-nov-18-2020.xlsx', 5)
proj_risk   <- read_excel('dattbi-nov-18-2020.xlsx', 6)
proj_issue  <- read_excel('dattbi-nov-18-2020.xlsx', 7)
capital     <- read_excel('dattbi-nov-18-2020.xlsx', 8)


#' removing IPA01 from descriptions, this is DABs and have no schedule,
#' budget, risk or issues won't show in individual
description <- description %>%
  filter(!(description$IP == "A01"))


data_date <- substring(file.info("dattbi.xlsx")$mtime, 1, 11)
min_date <- min(schedule$Approved_finish_date)
max_date <- max(schedule$Approved_finish_date)

#' Data cleaning:
nona_description <- description %>% drop_na()
nona_schedule    <- schedule    %>% drop_na()
nona_budget      <- budget      %>% drop_na()
nona_budget_yr   <- budget_yr   %>% drop_na()
nona_status      <- status      %>% drop_na()
nona_proj_risk   <- proj_risk   %>% drop_na()
nona_proj_issue  <- proj_issue  %>% drop_na()
nona_capital     <- capital     %>% drop_na()

#' Formatting dates:
schedule$Actual_date                    <- as.Date(as.character(schedule$Actual_date))
schedule$Approved_finish_date           <- as.Date(as.character(schedule$Approved_finish_date))
proj_issue$`Target Date for Resolution` <- as.Date(as.character(proj_issue$`Target Date for Resolution`))

#' Pasting schedule IP to major milestone blurb:
schedule$Major.Milestone <- paste0(schedule$IP, ":", schedule$Major.Milestone)
schedule$Major.Milestone.Standard <- paste0(schedule$Project, ":", schedule$Major.Milestone.Standard)

#' List of ip and directorate:
ip          <- c(description$IP[!is.na(description$IP)])
directorate <- c("All") #' Martin wants it to only say all

#' Status cleaning:
status$`Overall Project Health`[is.na(status$`Overall Project Health`)] <- "Blue"
status$status[is.na(status$status)]                                     <- "Not yet started"

#' Capital:
capital$IP                           <- as.character(capital$IP)
capital$`Authority vs. Expenditures` <- "Project Authority"

#' Data budget:
budget_yr             %<>% left_join(capital)
budget_yr$Value       <-   ifelse(is.na(budget_yr$Value), 0, budget_yr$Value)
budget_yr$Capital     <-   ifelse(is.na(budget_yr$Capital), 0, budget_yr$Capital)
budget_yr$Non_Capital <-   budget_yr$Value-budget_yr$Capital
budget_yr$year        <-   as.numeric(substr(budget_yr$Year, 1, 4))

# sorted budget ----
# for reporting purposes
sorted_budget<-budget[order(-budget$`Approved Budget`),]

# schedule data wrangling ----

no_completed_schedule <- schedule[!grepl("completed", schedule$Schedule.Health.Standard),]

# count completed, covid delayed ----

schedule_completed <- schedule %>%
  filter(grepl("completed", Schedule.Health.Standard, ignore.case = T))

schedule_completed_project_names <- schedule %>%
  filter(grepl("completed", Schedule.Health.Standard, ignore.case = T)) %>%
  select("IP") %>%
  unique()

covid_delayed <- schedule %>%
  filter(grepl("covid", Schedule.Health, ignore.case = T))

covid_delayed_project_names <- schedule %>%
  filter(grepl("covid", Schedule.Health, ignore.case = T)) %>%
  select("IP") %>%
  unique()

#' Project Stages
#' only Innovation projects have stages 1, 2, 3 and 4.
ipp_status <- status %>%
  filter(grepl("^\\d", IP)) %>%
  filter(`Overall Project Health` != "Blue")

ipp_stage_1  <- ipp_status %>%
  filter(or(
    grepl('1',          stage),
    grepl("Initiation", stage, ignore.case = T)))

ipp_stage_2  <- ipp_status %>%
  filter(or(
    grepl('2',        stage),
    grepl("Planning", stage, ignore.case = T)))

ipp_stage_3  <- ipp_status %>%
  filter(or(
    grepl('3',         stage),
    grepl("Execution", stage, ignore.case = T)))

ipp_stage_4  <- ipp_status %>%
  filter(or(
    grepl('4',       stage),
    grepl("Closure", stage, ignore.case = T)))

a_team_status <- status %>%
  filter(grepl("^A\\d", IP))

a_stage_1  <- a_team_status %>%
  filter(or(
    grepl('1',          stage),
    grepl("Initiation", stage, ignore.case = T)))

a_stage_2  <- a_team_status %>%
  filter(or(
    grepl('2',        stage),
    grepl("Planning", stage, ignore.case = T)))

a_stage_3  <- a_team_status %>%
  filter(or(
    grepl('3',         stage),
    grepl("Execution", stage, ignore.case = T)))

a_stage_4  <- a_team_status %>%
  filter(or(
    grepl('4',       stage),
    grepl("Closure", stage, ignore.case = T)))


#' Stream 1
stream_1 <- status %>%
  filter(or(
    grepl("Stream I$", Project, ignore.case = F),
    grepl("planning",  stage, ignore.case = T)))

#' Stream 2
stream_2 <- status %>%
  filter(or(
    grepl("Stream II$", Project, ignore.case = F),
    grepl("testing",  stage, ignore.case = T)))

#' Project Status
caution  <- status %>% filter(grepl("caution",   status, ignore.case = T))
on_track <- status %>% filter(grepl("On-Track",  status, ignore.case = T))
delayed  <- status %>% filter(grepl("Delayed",   status, ignore.case = T))

#' Project Health
green  <- status %>% filter(grepl("Green",   `Overall Project Health`, ignore.case = T))
yellow <- status %>% filter(grepl("Yellow",  `Overall Project Health`, ignore.case = T))
red    <- status %>% filter(grepl("Red",     `Overall Project Health`, ignore.case = T))

completed <- function(completion_date) {
  not(is.na(completion_date))
}

late <- function(completed, approved_finish_date, actual_date) {
  ifelse(completed, approved_finish_date - actual_date, NA)
}

far_out <- function(completed, projection) {
  ifelse(completed, NA, as.numeric(projection))
}

expected_completion_date <- function(completed, approved_finish_date, projection) {
  ifelse(
    is.na(projection),
    NA,
    ifelse(
      not(completed),
      approved_finish_date + months(as.integer(projection)),
      NA
    )
  ) %>% as_date()
}


label <- function(indicator, months) {

  ifelse(
    months <= 3,
    paste(indicator, "within-3-months", sep = "-"),
    ifelse(
      3 < months & months < 6,
      paste(indicator, "within-3-to-6-months", sep = "-"),
      ifelse(
        months>= 6,
        paste(indicator, "more-than-6-months", sep = "-"),
        NA)
    )
  )

}


health_label <- function(late, far_out) {
  ifelse(
    is.na(late),
    label("incomplete", far_out),
    label("completed", late)
  )
}


schedule %<>% mutate(Completed = completed(Actual_date),
                     Late = late(Completed, Approved_finish_date, Actual_date),
                     Far_out = far_out(Completed, Schedule.Health.Standard),
                     Expected_completion_date = expected_completion_date(Completed, Approved_finish_date, Far_out),
                     Health = health_label(Late, Far_out))
