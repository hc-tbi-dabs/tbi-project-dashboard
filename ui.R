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
          status = "primary",
          background    = NULL,
          boxToolSize   = "md",
          closable      = F,
          collapsible   = T,
          title = "Progress",
         footer = tagList(
          dashboardLabel("Green Health", status = "success"),
          dashboardLabel("Yellow Health", status = "warning"),
          dashboardLabel("Red Health", status = "danger")


        ),


          fluidRow(
              #' @todo: code should be repeated call to a function, not so much
              #' copy and pasting...
              #' @todo: group by project type, IP, IT, Innovation
              #' #todo: want to have different tabs?
              #'

            box(
              width = 3,
              status = "primary",
              title = "All Projects",
              solidHeader = T,
            boxPad(
              tags$h3("On Track"),
              br(),
              width = 12,
              tagList(
                apply(
                  X = on_track,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            boxPad(
              tags$h3("Caution"),
              br(),
              tagList(
                apply(
                  X = caution,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            boxPad(
              tags$h3("Delayed"),
              br(),
              tagList(
                apply(
                  X = delayed,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            )),

            box(
              title = "Innovation Projects",
              status = "primary",
              solidHeader = T,
              width = 3,
            box(
              width = 12,
              status = "primary",
              solidHeader = T,
              title = "Stream I",
              boxPad(
              tags$h3("Planning"),
              br(),
              tagList(
                apply(
                  X = stream_1_planning,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
              boxPad(
              tags$h3("Testing"),
              width = 12,
              tagList(
                apply(
                  X = stream_1_testing,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            )),

            box(
              width = 12,
              title = "Stream II",
              status = "primary",
              solidHeader = T,

              boxPad(
              tags$h3("Planning"),
              br(),
              tagList(
                apply(
                  X = stream_2_planning,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
              boxPad(
              tags$h3("Testing"),
              width = 12,
              tagList(
                apply(
                  X = stream_2_testing,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ))),

            box(
              width = 3,
              title = "Other IT Projects",
              status = "primary",
              solidHeader = T,
            boxPad(
              tags$h3("Stage 1: Initiation"),
              br(),
              #' @todo: please make the color reflect project health?!
              #' I have started the function colorfulDashboardBadge, needs
              #' implementation.
              tagList(
                apply(
                  X = a_stage_1,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            boxPad(
              tags$h3("Stage 2: Planning"),
              br(),
              tagList(
                apply(
                  X = a_stage_2,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            boxPad(
              tags$h3("Stage 3: Execution"),
              br(),
              tagList(
                apply(
                  X = a_stage_3,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            boxPad(
              tags$h3("Stage 4: Closure"),
              br(),
              tagList(
                apply(
                  X = a_stage_4,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            )),


            box(
              width = 3,
              title = "Investment Planning Projects",
              status = "primary",
              solidHeader = T,
            boxPad(
              tags$h3("Stage 1: Initiation"),
              br(),
              #' @todo: please make the color reflect project health?!
              #' I have started the function colorfulDashboardBadge, needs
              #' implementation.
              tagList(
                apply(
                  X = ipp_stage_1,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            boxPad(
              tags$h3("Stage 2: Planning"),
              br(),
              tagList(
                apply(
                  X = ipp_stage_2,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            boxPad(
              tags$h3("Stage 3: Execution"),
              br(),
              tagList(
                apply(
                  X = ipp_stage_3,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))
            ),
            boxPad(
              tags$h3("Stage 4: Closure"),
              br(),
              tagList(
                apply(
                  X = ipp_stage_4,
                  MARGIN = 1,
                  FUN = function(x) (colorfulDashboardBadge(x))))))))),
    column(
      width = 12,

      box(
        width = 12,
          collapsible = T,
          closable = F,
        title = "Balance",
          status = "success",
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
          title = "Other IT Projects",
          solidHeader = T,
          status = "success",
          withSpinner(plotlyOutput("a_team_projects_health"))),

           box(
            width = 4,
          collapsible = F,
          closable = F,
          title = "Investment Planning Projects",
          status = "success",
          solidHeader = T,

          withSpinner(plotlyOutput("ip_projects_health"))),
        footer = tagList(
          dashboardLabel("On Track", status = "success"),
          dashboardLabel("Caution", status = "warning"),
          dashboardLabel("Delayed", status = "danger")))),

    column(
      width = 12,
    box(
      status = "info",
       collapsible = T,
       closable = F,
       title = "Budget",
      width = 12,
      uiOutput("project_portfolio_budget"))),

    column(
      width = 12,
      boxPlus(
        status = "navy",
        collapsible = T,
        closable = F,
        width = 12,
        title = "Fiscal Year Schedule",
        footer = "Tasks completed before 2020 are hidden.",
        boxPlus(
          status = "navy",
          width = 4,
          closable = F,
          collapsed = F,
          collapsible = F,
          dateRangeInput(
            start = max_date - months(3),
            end = max_date,
            inputId = "main-page-date-slider",
            label = "Date Range",
            min = min_date,
            max = max_date)),
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

      box(
        title = textOutput("project_name"),
        status = "info",
        width = 12,
        tagList(
          valueBoxOutput("overall"),
          valueBoxOutput("overall_stage"),
          valueBoxOutput("directorate")
        )
      )
    ),

    column(
      width = 12,
      fluidRow(
        width = 12,
        box(
          title = "Project Budget",
          status = "info",
          solidHeader = T,
          width = 9,
        withSpinner(plotlyOutput("budget_plt"))),
        uiOutput("project_portfolio_budget_individual"))),

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
      box(width = 12,
          title = "Schedule",
          withSpinner(
            timevisOutput("timevis_plot_individual")
          ))
    ))
  ),

  tabItem(
    tabName = "explanations",

    fluidRow(width = 12,
           column(
             width = 12,
             includeHTML(
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
    body         = body,
    tags$head(
      tags$style(HTML("
                        .IP_A03         { background-color: azure 	         }
                        .IP_A04         { background-color: aliceblue 	     }
                        .IP_A05         { background-color: ghostwhite 	    }
                        .IP_A06         { background-color: whitesmoke 	    }
                        .IP_628         { background-color: seashell 	      }
                        .IP_710         { background-color: beige 	         }
                        .IP_704         { background-color: oldlace 	       }
                        .IP_705         { background-color: floralwhite 	   }
                        .IP_Kelpie      { background-color: ivory 	         }
                        .IP_IP000       { background-color: antiquewhite    }
                        .IP_Cyclops     { background-color: linen 	         }
                        .IP_Hummingbird { background-color: lavenderblush   }
                        .IP_Cipher      { background-color: mistyrose 	     }
                      ")))
  )
)

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
