library(lubridate)
library(rmarkdown)
library(htmltools)
library(shinydashboard)
library(shinydashboardPlus)
 
if (!require("timevis")) {
  install.packages("timevis")
}

if (!require("webshot")) {
  install.packages("webshot")
}

if (!require("exportwidget")) {
  devtools::install_github("timelyportfolio/exportwidget")
}

library(webshot)
library(timevis)
library(exportwidget)

#' TODO: remove hard-coded numbers.
#' TODO: update data?

style = "

  h2 {
    font-family: 'Arial';
    margin-left:20px;
    font-weight: bold;
    line-height: 1.1;
    color: #2E4053;
  }
  
  .main-header .logo {
    font-family: Arial, Helvetica, sans-serif;
    font-weight: bold;
    font-size:   20px;
  }
  
  .skin-blue .main-header .logo:hover {
    background-color: #000833;
  }
  
  .skin-blue .main-header .logo {
    background-color: #000833;
  }
  
  .skin-blue .main-header .navbar {
    background-color: #000833;
  }
  
  .skin-blue .main-sidebar {
    background-color: #000833;
  }
  
  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
     background-color: #0278A4;
  }
  
  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
     background-color: #0278A4;
  }
  
  #downloadData {
    color: black;
    margin-left: 10px;
  }
  
  #downloadreport_individual {
    color: black;
    margin-left: 10px;
  }

  #downloadreport_overview {
    color: black;
    margin-left:10px;
  }
  
  .small-box.bg-red {
    background-color: #C00000 !important;
  }

"
          
inactivity <- "
  function idleTimer() {
    var t = setTimeout(logout, 120000);
      window.onmousemove = resetTimer;
      window.onmousedown = resetTimer;
      window.onclick     = resetTimer;
      window.onscroll    = resetTimer;
      window.onkeypress  = resetTimer;
    
    function logout() { window.close(); }
    
    function resetTimer() {
      clearTimeout(t);
      t = setTimeout(logout, 120000);  // milliseconds
    }
  }
  idleTimer();"

ui <- secure_app(
  head_auth = tags$script(inactivity),
  
  dashboardPagePlus(
    
    dashboardHeaderPlus(
      fixed      = T,
      title      = paste0('TBI Projects Dashboard \n as of ', dat),
      titleWidth = 512),

    dashboardSidebar(
      width = 256,
     
      sidebarMenu(
        id = "sidebar",
        menuItem(text = "Overview",   tabName = "overview"),
        menuItem(text = "Individual", tabName = "individual"),
        menuItem(text = "About",      tabName = "explanations"),

        conditionalPanel(
          condition = "input.sidebar == 'individual' ",
          selectInput(
            inputId = "selectip",
            label   = "Select an IP project",
            choices = ip)),
        
        conditionalPanel(
          condition = "input.sidebar == 'overview' ",
          
          selectInput(
            inputId = "selectdir",
            label   = "Select a Directorate",
            choices = directorate),
                                   
          actionButton(
            inputId = "info",
            label   = "View IP Name",
            icon    = icon("eye"))),
        
        br(), br(),
        
        tags$b("Download:", style = "margin-left: 10px;"),
        
        br(), br(),
       
        downloadButton(outputId = "downloadData", label = "Data"),
        
        br(), br(),
        
        conditionalPanel(
          condition = "input.sidebar == 'individual' ",
          downloadButton(outputId = "downloadreport_individual", label = "Report")),

        conditionalPanel(
          condition="input.sidebar == 'overview' ",
          downloadButton(outputId = "downloadreport_overview", label = "Report")),

        br(), br(), br(), br(), br(),

        actionButton(
          inputId = "contact",
          label   = "Contact us",
          icon    = icon("phone")))),
    
    dashboardBody(
      tags$head(tags$style(HTML(style))),
    
    tabItems(
      tabItem(
        tabName = "overview",
        
        fluidRow(
          
          gradientBox(
            title         = "IT Projects",
            width         = 12,
            icon          = "fa fa-heart",
            gradientColor = "maroon", 
            boxToolSize   = "xs", 
            closable      = F,
            footer        = "Footer",
            valueBoxOutput(outputId = "stage_1", width = 3),
            valueBoxOutput(outputId = "stage_2", width = 3),
            valueBoxOutput(outputId = "stage_3", width = 3),
            valueBoxOutput(outputId = "stage_4", width = 3))),
        
         fluidRow(
           box(
             title = "Innovation Projects Status",
             status = "primary",
             boxProfile(
               src = "https://minutes.co/wp-content/uploads/2019/10/best-innovation-teams.jpg",
               title = "Innovation Projects",
               subtitle = "What are Innovation Projects?",
               boxProfileItemList(
                 bordered = T,
                 boxProfileItem(
                   title = "Planning",
                   description = valueBoxOutput(outputId = "testing"))),
                 boxProfileItem(
                   title = "Testing",
                   description = valueBoxOutput(outputId = "planning"))))),
       
        fluidRow(
          box(width = 12,
              title = "Tasks",
              valueBoxOutput(outputId = "delayed",   width = 6),
              valueBoxOutput(outputId = "completed", width = 6))),
        
        uiOutput("overall_project_health"),
        
        uiOutput("ui_output2"),
             
        fluidRow( 
          box(width = 12,
              title = "Fiscal Year Schedule",
              footer = textOutput("caption"),
              withSpinner(timevis::timevisOutput("timevis_plot_all"))))),
      
      tabItem(
        tabName = "individual",
        
        fluidRow(
          box(width = 12,
              uiOutput("project_name"),
              valueBoxOutput("overall"),
              valueBoxOutput("overall_stage"),
              valueBoxOutput("directorate"))),
              
        fluidRow(
          box(title = 'Project Budget',
              tabsetPanel(
                tabPanel(title='Breakdown by Year',
                         withSpinner(plotlyOutput('budget_plt', height=450))),
                      tabPanel(title='Table',
                               DT::dataTableOutput('budget_tbl', height=450)))
                    ),
                box(
                  title = 'Projections',
                  withSpinner(plotOutput('budget_all', height = 490)))
              ),
              
              
              fluidRow(
                column(12,
                       box(title='Project Risks',width=NULL,
                           DT::dataTableOutput('proj_risk_tb')))
              ),
              
              fluidRow(
                column(12,
                       box(title='Project Issues',width=NULL,
                           DT::dataTableOutput('proj_issue_tb')))
              ),
              
              fluidRow(
                column(12,
                       box(title='Schedule',width=NULL,
                           withSpinner(timevis::timevisOutput('timevis_plot_individual')),
                           br(),
                           br(),
                           DT::dataTableOutput('schedule_tb'))))),
      
      tabItem(
        tabName = "explanations",
              
        fluidRow(
          width = 12,
          tags$h2("Explanation of Status Indicators & Project Stages")),
            
        fluidRow(
          column(
            width = 12,
            includeHTML("explanations.html"))))))))