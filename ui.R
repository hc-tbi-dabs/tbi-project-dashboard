library(lubridate)
library(rmarkdown)
library(htmltools)
library(shinydashboard)
library(shinydashboardPlus)
library(timevis)
library(webshot)
library(exportwidget)
library(scales)


left_menu <- tagList(
  
  dropdownBlock(
    id    = "download-menu",
    title = "Download",
    icon  = icon("download"),
    
    downloadButton(
      outputId = "download_data",
      label    = "Data",
      icon     = icon("download")),
    
    conditionalPanel(
      condition = "input.sidebar == 'individual' ",
      
      downloadButton(
        outputId = "downloadreport_individual", 
        label    = "Report",
        icon     = icon("download"))),
    
    conditionalPanel(
      condition = "input.sidebar == 'overview' ",
      
      downloadButton(
        outputId = "downloadreport_overview",
        label    = "Report",
        icon     = icon("download")))),
  
  actionButton(
    inputId = "contact",
    label   = "Contact",
    icon    = icon("phone")))


#' @todo: put the date somwhere.
#' tagList(paste0('As of ', dat))


header <- dashboardHeaderPlus(
  left_menu  = left_menu,
  fixed      = F,
  title      = paste0("TBI Projects")
)


sidebar_menu <- sidebarMenu(
  id = "sidebar",
  
  menuItem(
    text    = "Overview",
    tabName = "overview",
    icon    = icon("globe-africa")),
  
  menuItem(
    text    = "Individual",
    tabName = "individual",
    icon    = icon("search")),
  
  menuItem(
    text    = "About",
    tabName = "explanations",
    icon    = icon("info")),
  
  conditionalPanel(
    condition = "input.sidebar == 'individual' ",
    
    selectInput(
      inputId = "selectip",
      label   = "Select an IP project",
      choices = ip)))


sidebar <- dashboardSidebar(sidebar_menu)


rightsidebar <- rightSidebar(
  title = "Right Sidebar",
  background = "dark",
  
  rightSidebarTabContent(
    id     = 1,
    title  = "Tab 1",
    icon   = "desktop",
    active = T,
    sliderInput(
      inputId = "obs",
      label   = "Number of observations:",
      min     = 0,
      max     = 1000,
      value   = 500)),
  
  rightSidebarTabContent(
    id    = 2,
    title = "Tab 2",
    textInput(inputId = "caption",
              label = "Caption",
              "Data Summary")),
  
  rightSidebarTabContent(
    id    = 3,
    icon  = "paint-brush",
    title = "Tab 3",
    numericInput(inputId = "obs",
                 label = "Observations:",
                 value = 10,
                 min   = 1,
                 max   = 100)))


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "overview",
     
      boxPlus(
        title = "Nav Pills",
        status = "info",
        footer_padding = FALSE,
        
        footer = navPills(
          
          navPillsItem(
            active = TRUE,
            pillName = "Item 1",
            pillColor = "green",
            pillIcon = NULL,
            pillText = "Some text here"),
          
          navPillsItem(
            pillName = "Item 2",
            pillColor = "red",
            pillIcon = "fa fa-angle-down",
            pillText = "10%"))),
      
      box(
        title = "Box with a green boxPad",
        status = "warning",
        fluidRow(
          column(width = 6),
          column(
            width = 6,
            boxPad(
              color = "green",
              
              descriptionBlock(header = "8390",
                               text = "VISITS"),
              
              descriptionBlock(header = "30%",
                               text = "REFERRALS"),
              
              descriptionBlock(header = "70%",
                               text = "ORGANIC"))))),
      
      gradientBox(
        title         = "IT Projects",
        icon          = "fa fa-heart",
        gradientColor = "teal",
        boxToolSize   = "md",
        closable      = F,
        footer        = "This is a test."),
      
      gradientBox(
        title         = "IT Projects",
        icon          = "fa fa-heart",
        gradientColor = "teal",
        boxToolSize   = "md",
        closable      = F,
        footer        = tagList(
          dashboardLabel("Label 1", status = "info"),
          dashboardLabel("Label 2", status = "success"),
          dashboardLabel("Label 3", status = "warning"),
          dashboardLabel("Label 4", status = "primary"),
          dashboardLabel("Label 5", status = "danger"),
        ),
        
        textOutput(outputId = "stage_1"),
        textOutput(outputId = "stage_2"),
        textOutput(outputId = "stage_3"),
        textOutput(outputId = "stage_4")
      ),
      
      widgetUserBox(
        title = "Innovation Projects Status",
        icon   = "fa fa-heart",
        gradientColor = "red",
        boxProfile(
          src = "https://minutes.co/wp-content/uploads/2019/10/best-innovation-teams.jpg",
          title = "Innovation Projects",
          subtitle = "What are Innovation Projects?",
          footer        = "Footer",
          boxProfileItemList(
            bordered = T,
            boxProfileItem(
              title = "Planning",
              description = textOutput(outputId = "testing")
            )
          ),
          boxProfileItem(
            title = "Testing",
            description = textOutput(outputId = "planning")))),
      
      box(
        width = 12,
        title = "Tasks",
        valueBoxOutput(outputId = "delayed",   width = 6),
        valueBoxOutput(outputId = "completed", width = 6)),
      
    
    uiOutput("overall_project_health"),
    
    uiOutput("project_portfolio_budget"),
    
    fluidRow(
      box(
        width = 12,
        title = "Fiscal Year Schedule",
        footer = textOutput("caption"),
        withSpinner(timevis::timevisOutput("timevis_plot_all"))))),
    
    tabItem(
      tabName = "individual",
      
      fluidRow(
        box(
          width = 12,
          uiOutput("project_name"),
          valueBoxOutput("overall"),
          valueBoxOutput("overall_stage"),
          valueBoxOutput("directorate"))),
      
      fluidRow(
        box(
          title = "Project Budget",
          tabsetPanel(
            tabPanel(
              title = "Breakdown by Year",
              withSpinner(plotlyOutput("budget_plt", height = 450))))),
        box(
          title = "Projections",
          withSpinner(plotOutput("budget_all", height = 490)))),
      
      fluidRow(
        column(
          width = 12,
          box(
            title = "Project Risks",
            width = NULL,
            dataTableOutput("proj_risk_tb")))),
      
      fluidRow(
        column(
          width = 12,
          box(
            title = "Project Issues",
            width = NULL,
            dataTableOutput("proj_issue_tb")))),
      
      fluidRow(
        box(title = "Schedule",
            withSpinner(timevisOutput("timevis_plot_individual"))))),
    
    tabItem(
      tabName = "explanations",
      fluidRow(
        uiOutput("explanations_header")),
      
      fluidRow(
        includeHTML("explanations.html")))))


ui <- secure_app(
  head_auth = tags$script(inactivity),
  
  dashboardPagePlus(
    collapse_sidebar = T,
    header       = header,
    sidebar      = sidebar,
    rightsidebar = rightsidebar,
    body         = body))