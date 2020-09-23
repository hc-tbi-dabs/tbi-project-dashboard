library(lubridate)
library(rmarkdown)
library(htmltools)
library(shinydashboard)
library(shinydashboardPlus)
library(timevis)
library(webshot)
library(scales)

#' @todo: capitalists want to cut up your life and fight over who gets to own
#' the pieces.
#'
#' Valid statuses are: primary, success, info, warning, danger, navy, teal, purple, orange, maroon, black.

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

    fluidRow(
    column(
      width = 12,

      boxPlus(
        width = 12,
          solidHeader = T,
          status = "primary",
          background    = NULL,
          boxToolSize   = "md",
          closable      = F,
          collapsible   = T,
          title = "Progress",
          fluidRow(
              #' @todo: code should be repeated call to a function, not so much
              #' copy and pasting...
              #' @todo: group by project type, IP, IT, Innovation
              #' #todo: want to have different tabs?
            box(
              width = 3,
              title = "Innovation Projects",
              status = "primary",
              solidHeader = T,
            box(
              title = "Stage 1",
              width = 12,
              status = "primary",
              #' @todo: please make the color reflect project health?!
              #' I have started the function colorfulDashboardBadge, needs
              #' implementation.
              tagList(
                apply(
                  X = stage_1,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            box(
              title = "Stage 2",
              width = 12,
              status = "primary",
              tagList(
                apply(
                  X = stage_2,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            box(
              title = "Stage 3",
              width = 12,
              status = "primary",
              tagList(
                apply(
                  X = stage_3,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            box(
              title = "Stage 4",
              status = "primary",
              width = 12,
              tagList(
                apply(
                  X = stage_4,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            )),
            box(
              width = 3,
              status = "primary",
              title = "IP Projects",
              solidHeader = T,
            box(
              title = "On Track",
              width = 12,
              tagList(
                apply(
                  X = on_track,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            box(
              width = 12,
              title = "Caution",
              tagList(
                apply(
                  X = caution,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            box(
              title = "Delayed",
              width = 12,
              tagList(
                apply(
                  X = delayed,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            )
            )
            ,
            box(
              width = 3,
              title = "IP Projects",
              status = "primary",
              solidHeader = T,
              box(
              title = "Planning",
              width = 12,
              tagList(
                apply(
                  X = planning,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
              box(
              title = "Testing",
              width = 12,
              tagList(
                apply(
                  X = testing,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            )),

          )),
    ),

    column(
      width = 12,

      box(
        width = 12,
          collapsible = T,
          closable = F,
        title = "Balance",
          status = "success",
          solidHeader = T,
          box(
            width = 4,
          collapsible = F,
          closable = F,
          title = "Innovation Projects",
          solidHeader = T,
          status = "success",
         withSpinner(plotlyOutput("innovation_projects_health"))),

        box(
            width = 4,
          collapsible = F,
          closable = F,
          title = "A Team Projects",
          solidHeader = T,
          status = "success",
          withSpinner(plotlyOutput("a_team_projects_health"))),

           box(
            width = 4,
          collapsible = F,
          closable = F,
          title = "IP Projects",
          status = "success",
          solidHeader = T,

          withSpinner(plotlyOutput("ip_projects_health"))),
        footer = tagList(
          dashboardLabel("On Track", status = "success"),
          dashboardLabel("Caution", status = "warning"),
          dashboardLabel("Delayed", status = "danger")
        )

        ),

      uiOutput("overall_project_health")),

    column(
      width = 12,
    boxPlus(
      width = 12,
      uiOutput("project_portfolio_budget"))),

    column(
      width = 12,
      tags$h1("Scheduling"),
      br(),
      boxPlus(
        status = "navy",
        enable_dropdown = T,
        dropdown_icon = "wrench",
        dropdown_menu = dropdownItemList(
          dropdownItem("Setting 1"),
          dropdownItem("Setting 2"),
          dropdownItem("Setting 3")
        ),
        collapsible = T,
        closable = F,
        width = 12,
        title = "Fiscal Year Schedule",
          footer = "Tasks completed before 2020 are hidden.",
        tagList(
            dateRangeInput(inputId = "slider-date", label = "Date Range", min = 0, max = 1, width = "80%")
        ),
        box(
          solidHeader = T,
          width = 12,
        withSpinner(timevisOutput("timevis_plot_all")))
      )
    )
    #' @todo: need to remove logout button, these BR are a work-aroud.

  ),
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
