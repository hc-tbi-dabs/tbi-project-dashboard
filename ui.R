
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
# runApp(host = "192.168.0.110", port=8080)

ui<-secure_app(
  head_auth = tags$script(inactivity),
  dashboardPage(
  dashboardHeader(title=paste0('TBI Projects Dashboard \n as of ',dat),
                  titleWidth=500),
  
  dashboardSidebar(width=150,
                   sidebarMenu(id='sidebar',
                               menuItem('Overview',tabName='overview'),
                               menuItem('Individual',tabName='individual'),
                               menuItem('About',tabName='explanations'),
                               #menuItem('KPIs',tabName='kpis'),
                               
                               conditionalPanel(
                                 condition="input.sidebar == 'individual' ",
                                 selectInput('selectip',label="Select an IP project",choices=ip)
                               ),
                               conditionalPanel(
                                 condition="input.sidebar == 'overview' ",
                                 selectInput('selectdir',label="Select a Directorate",choices=directorate),
                                 actionButton('info','View IP Name',icon=icon('eye'))
                               ),
                               
                               br(),br(),
                               tags$b('Download:',style="margin-left:10px;"),
                               br(),
                               br(),
                               tags$style(type="text/css", "#downloadData {color: black;margin-left:10px;}"),
                               downloadButton('downloadData','Data'),
                               br(),
                               br(),
                               conditionalPanel(
                                 condition="input.sidebar == 'individual' ",
                                 tags$style(type="text/css", "#downloadreport_individual {color: black;margin-left:10px;}"),
                                 downloadButton('downloadreport_individual','Report')
                               ),
                               conditionalPanel(
                                 condition="input.sidebar == 'overview' ",
                                 tags$style(type="text/css", "#downloadreport_overview {color: black;margin-left:10px;}"),
                                 downloadButton('downloadreport_overview','Report')
                               ),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               actionButton('contact','Contact us',icon=icon('phone')))
  ),
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: Arial, Helvetica, sans-serif;
        font-weight: bold;
        font-size: 20px;
      }

     .skin-blue .main-header .logo:hover{
        background-color: #000833;
     }

     .skin-blue .main-header .logo{
        background-color: #000833;
     }

     .skin-blue .main-header .navbar{
       background-color: #000833;
     }

     .skin-blue .main-sidebar {
       background-color: #000833;
     }

     .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
       background-color: #0278A4;

    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
       background-color: #0278A4;

    '))),
    
    tabItems(
      
      tabItem(tabName='overview',
              
              fluidRow(
                div(style='display: inline-block;vertical-align:center;width:100px;', uiOutput('project_name2')),
                div(style="display: inline-block;vertical-align:center; width: 300px;",HTML("<br>")),
                bsModal('modal','IP Name','info',tableOutput('ip_tbl')),
                br(),
                br()
                #valueBoxOutput("completed"),
                #valueBoxOutput("delayed"),
                # valueBoxOutput("planning")
              ),
              box(
                width = 8,
                title = "IP Projects",
                valueBoxOutput("stage_1", width = 3),
                valueBoxOutput("stage_2", width = 3),
                valueBoxOutput("stage_3", width = 3),
                valueBoxOutput("stage_4", width = 3)),
              box(
                width = 4,
                title = "Innovation Projects",
                valueBoxOutput("planning", width = 6),
                valueBoxOutput("testing", width = 6)),
              uiOutput('ui_output1'),
              uiOutput('ui_output2'),
              div(style="text-align:center;
                        color:#4086b8;
                        font-size:10;
                        font-weight:bold;
                  ",textOutput("caption")),
              uiOutput('ui_output3')
              
              # fluidRow(
              #   box(title='Project Risks',
              #       plotOutput('projrisk')),
              #   box(title='Project Issues',
              #       plotOutput('projissue'))
              # )
      ),
      
      
      tabItem(tabName='individual',
              
              fluidRow(width=12,
                       uiOutput('project_name'),
                       tags$style(".small-box.bg-red {background-color: #C00000 !important;}"),
                       valueBoxOutput('overall'),
                       valueBoxOutput('overall_stage'),
                       valueBoxOutput('directorate')
                       
              ),
              
              fluidRow(
                
                # box(title='Project Functionality',height='500px',
                #     tabsetPanel(id='tabs',
                #                 
                #                 tabPanel(title='Table',
                #                          DT::dataTableOutput("function_tb"))
                #     )),
                
                box(title='Project Budget',
                    tabsetPanel(
                      tabPanel(title='Breakdown by Year',
                               withSpinner(plotlyOutput('budget_plt', height=450))),
                      tabPanel(title='Table',
                               DT::dataTableOutput('budget_tbl', height=450)))
                    ),
                box(
                  title='Projections',
                  withSpinner(plotOutput('budget_all', height=490)))
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
                           withSpinner(plotlyOutput('schedule_plt')),
                           br(),
                           br(),
                           DT::dataTableOutput('schedule_tb')))
              )
              
      ),
      
      tabItem(tabName='explanations',
              
              fluidRow(width=12,
                       uiOutput('explanations_header')
              ),
              
              fluidRow(
                column(12,
                       includeHTML("explanations.html"))
              )
      ),
      
      tabItem(tabName='kpis',
              
              fluidRow(width=12,
                       uiOutput('kpis_header')
              )
      )
    )
    
  )             
)
)