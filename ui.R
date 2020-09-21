library(lubridate)
library(rmarkdown)
library(htmltools)
library(shinydashboard)
library(shinydashboardPlus)
library(timevis)
library(webshot)
library(scales)


left_menu <- tagList(
  dropdownBlock(
    id    = "download-menu",
    title = "Download",
    icon  = icon("download"),
    
    downloadButton(
      outputId = "download_data",
      label    = "Data",
      icon     = icon("download")
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'individual' ",
      
      downloadButton(
        outputId = "downloadreport_individual",
        label    = "Report",
        icon     = icon("download")
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'overview' ",
      
      downloadButton(
        outputId = "downloadreport_overview",
        label    = "Report",
        icon     = icon("download")
      )
    )
  ),
  
  actionButton(
    inputId = "contact",
    label   = "Contact",
    icon    = icon("phone")
  )
)





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
    icon    = icon("globe-africa")
  ),
  
  menuItem(
    text    = "Individual",
    tabName = "individual",
    icon    = icon("search")
  ),
  
  menuItem(
    text    = "About",
    tabName = "explanations",
    icon    = icon("info")
  )
  
)


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
      value   = 500
    )
  ),
  
  rightSidebarTabContent(
    id    = 2,
    title = "Tab 2",
    textInput(inputId = "caption",
              label = "Caption",
              "Data Summary")
  ),
  
  rightSidebarTabContent(
    id    = 3,
    icon  = "paint-brush",
    title = "Tab 3",
    numericInput(
      inputId = "obs",
      label = "Observations:",
      value = 10,
      min   = 1,
      max   = 100
    )
  )
)


body <- dashboardBody(tabItems(
  tabItem(
    tabName = "overview",
    
    column(
      width = 12,
      br(),
        boxProfile(
          title = "TBI Projects",
          subtitle = tagList(paste("Last updated:", data_date)),
          src = "https://minutes.co/wp-content/uploads/2019/10/best-innovation-teams.jpg",
         
      
      boxPlus(
        width = 12,
          solidHeader = T,
          status = "success",
          background    = NULL,
          boxToolSize   = "md",
          closable      = F,
          collapsible   = T,
          title = "Project Health and Current Stage",
          footer = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.",
          fluidRow(
              #' @todo: make interactivity work!
              #' @todo: group by project type, IP, IT, Innovation
              #' maybe want to have different tabs?
            br(),
            box(
              title = "Stage 1",
              width = 3,
              background = "light-blue",
              tagList(as.list(stage_1$IP))
            ),
            box(
              title = "Stage 2",
              width = 3,
              background = "purple",
              tagList(as.list(stage_1$IP))
            ),
            box(
              title = "Stage 3",
              width = 3,
              background = "blue",
              tagList(as.list(stage_1$IP))
            ),
            box(
              title = "Stage 4",
              width = 3,
              background = "navy",
              tagList(as.list(stage_1$IP))
            ),
            box(
              title = "On Track",
              width = 3,
              background = "green",
              tagList(as.list(on_track$IP))
            ),
            box(
              width = 3,
              title = "Caution",
              background = "orange",
              tagList(as.list(caution$IP))
            ),
            box(
              title = "Delayed",
              width = 3,
              background = "red",
              tagList(as.list(delayed$IP))
            ),
              box(
              title = "Planning",
              width = 3,
              background = "black",
              tagList(as.list(planning$IP))
            ),
              box(
              title = "Testing",
              width = 3,
              background = "black",
              tagList(as.list(testing$IP))
            ),
            box(
              title = "Completed",
              width = 3,
              background = "black",
              tagList(as.list(schedule_completed_project_names$IP))
            ),
          
          
          )),
    )),
    
    br(),
    
    column(
      width = 12,
      tags$h1("Project Health"),
     
      #' fluidRow(
      #'   box(
      #'     title = "Green Health",
      #'     width = 3,
      #'     background = "green",
      #'     tagList(as.list(green$IP))
      #'   ),
      #'    box(
      #'     title = "Yellow Health",
      #'     width = 3,
      #'     background = "yellow",
      #'     tagList(as.list(yellow$IP))
      #'   ),
      #'    box(
      #'     title = "Red Health",
      #'     width = 3,
      #'     background = "red",
      #'     tagList(as.list(red$IP))
      #'   )
      #' ), 
      tags$p(
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
      ),
      
      uiOutput("overall_project_health")),
    
    column(
      width = 12,
      tags$h1("Project Budget")
      
      
    ),
    
    column(width = 12, uiOutput("project_portfolio_budget")),
    
    column(
      width = 12,
      tags$h1("Scheduling"),
      
      tags$p(
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.")),
  
    fluidRow(
      gradientBox(
        icon = "fa fa-calendar",
        gradientColor = "maroon",
        width = 12,
        title = "Fiscal Year Schedule",
        "Tasks completed before 2020 are hidden.",
        footer = withSpinner(timevis::timevisOutput("timevis_plot_all"))
      )
    ),
    #' @todo: need to remove logout button, these BR are a work-aroud.
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    
  ),
  
  
  tabItem(
    tabName = "individual",
    
    column(
      width = 12,
      
      selectInput(
        inputId = "selectip",
        label   = "Select an IP project",
        choices = ip
      ),
      
      widgetUserBox(
        title = textOutput("project_name"),
        subtitle = "Something useful",
        type = 2,
        color = "teal",
        src = "https://upload.wikimedia.org/wikipedia/commons/d/d8/Crystal_Project_Package_games_board.png",
        boxToolSize = "xs",
        width = 12,
        tags$p(
          "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
        ),
        footer = tagList(
          valueBoxOutput("overall"),
          valueBoxOutput("overall_stage"),
          valueBoxOutput("directorate")
        )
      )
    ),
    
    fluidRow(column(
      width = 12,
           withSpinner(plotlyOutput("budget_plt")),
      
      boxPlus(
        title = "Projections",
        footer = "Something.",
        withSpinner(plotOutput("budget_all"))),
      
    )),
    
    fluidRow(column(
      width = 12,
      box(
        title = "Project Risks",
        width = NULL,
        dataTableOutput("proj_risk_tb")
      )
    )),
    
    fluidRow(column(
      width = 12,
      box(
        title = "Project Issues",
        width = NULL,
        dataTableOutput("proj_issue_tb")
      )
    )),
    
    fluidRow(column(
      width = 12,
      box(title = "Schedule",
          withSpinner(
            timevisOutput("timevis_plot_individual")
          ))
    ))
  ),
  
  tabItem(
    tabName = "explanations",
    
    column(width = 12,
           fluidRow(uiOutput(
             "explanations_header"
           ))),
    
    column(width = 12,
           fluidRow(includeHTML(
             "explanations.html"
           )))
  )
))


ui <- secure_app(
  head_auth = tags$script(inactivity),
  
  dashboardPagePlus(
    collapse_sidebar = T,
    header       = header,
    sidebar      = sidebar,
    rightsidebar = rightsidebar,
    body         = body
  )
)