#!/usr/bin/env Rscript
#' TODOs:
#' 
#' - Remove for now the "Select a Directorate".
#' - Stage 1, 2, 3, 4 should be labeled in X axis for Project Health and Current Stage
#' - Differentiate between Stream / Stage in Individual Page 

shinyServer(
  function(input, output, session) {
    
    output$res_auth <- renderPrint({reactiveValuesToList(result_auth)})
    
    result_auth <- secure_server(
      check_credentials = check_credentials(credentials))
 
    observeEvent(
      eventExpr = input$contact,
      handlerExpr = {
        showModal(
          modalDialog(title = "Contact",
                      HTML(paste(
                        "If you have technical questions regarding the application, please contact:",
                        br(),
                        br(),
                        "Jodi Qiao",
                        br(),
                        "Jr Data Scientist",
                        br(),
                        "Data Analytics and Business Solutions",
                        br(),
                        "TBI, POD",
                        br(),
                        "di.qiao@canada.ca; dqiao100@uottawa.ca")),
                      easyClose = T))
    })
 
   
  output$project_name <- renderText({
    #' @todo: maybe we don't need to treat project names differently?
    #'  
    
    project_names <- c("Cipher",
                       "Cyclops",
                       "Hummingbird",
                       "Kelpie")
    
    project_name <- all_proj %>%
      filter(IP == input$selectip) %>%
      pull(`Project`)
    
    if (any(startsWith(project_names, project_name))) {
      return(h2(project_name))
    }
    
    return(h2(paste0(input$selectip, ": ", name)))
    
  })
 
   
  ip_selected <- reactive({list(ip = input$selectip, ips = all_proj$IP)})
 
   
  project_selected <- reactive({
    list(ip = input$selectip,
         names = all_proj$`Project`[all_proj$IP == input$selectip])
  })
 
   
  report_title <- reactive({
    
    ip <- input$selectip
    project_title <- project_selected()$names[project_selected()$ip == ip]
    
    if (ip != project_title) {
      report_title <- paste(ip, project_title)
    } else {
      report_title <- ip
    }
    return(report_title)
  })
  
  
  df_budget_summary <- reactive({
    
    budget %>%
      filter(IP %in% ip_selected()$ips) %>%
      left_join(all_proj %>% select(IP = IP)) %>%
      summarise(
        `Approved Budget`                         = sum(`Approved Budget`, na.rm = T),
        `Forecasted Total Expenditures`           = sum(`Forecasted Total Expenditures`, na.rm = T),
        `Expenditure to Date`                     = sum(`Expenditure to Date`, na.rm = T),
        `Project Forecasted Expenditures 2020-21` = sum(`Variance / Remaining budget`, na.rm = T))
    
  })
 
   
  amount_approved_budget <- reactive({
    
    .f <- dollar_format()
    .f(df_budget_summary()[["Approved Budget"]])
  
  })
  
  amount_forcasted_total_expenditures <- reactive({
    
    .f <- dollar_format()
    .f(df_budget_summary()[["Forcasted Total Expenditures"]])
  
  })
 
  amount_expenditure_to_date <- reactive({
    
    .f <- dollar_format()
    .f(df_budget_summary()[["Expenditure to Date"]])
  
  })
  
  amount_projected_forecasted_expenditures <- reactive({
    
    .f <- dollar_format()
    .f(df_budget_summary()[["Project Forecasted Expenditures 2020-21"]])
  
  })
  
  output$break_down_by_year <-renderPlotly({
    
    df <- budget_yr %>%
      filter(IP %in% ip_selected()$ips) %>%
      group_by(Year, year, `Authority vs. Expenditures`) %>%
      summarise(
        Capital = sum(Capital, na.rm = T),
        Non_Capital = sum(Non_Capital, na.rm = T))
    
    budget_plot(df)
    
  })
 
   
  output$budget_tbl <- renderDataTable({
    ds <- budget_yr %>%
      filter(IP == input$selectip) %>%
      spread(`Authority vs. Expenditures`, Value) %>%
      select(-year) %>%
      mutate_at(c('Capital',
                  'Non_Capital',
                  'Project Authority',
                  'Project Expenditures'),
                dollar)

    datatable(data = ds,
              options = list(searching = FALSE,
                             pageLength = 5,
                             lengthMenu = c(5, 10, 15, 20),
                             scrollX = T))
  })
  
  
  output$budget_tbl2<-DT::renderDataTable({
    
    ds<-budget_yr%>%
      filter(IP %in% ip_selected()$ips)%>%
      group_by(Year,year,`Authority vs. Expenditures`)%>%
      summarise(capital=sum(Capital,na.rm=T),
                non_capital=sum(Non_Capital,na.rm=T),
                value=sum(Value,na.rm=T))%>%
      mutate_at(c('capital','non_capital','value'),dollar)
    
                                        # left_join(all_proj%>%select(IP=IP,internal_external=`Internal or External`))%>%
                                        # group_by(var,Year,internal_external)%>%
                                        # summarise(value=sum(value,na.rm=T))%>%
                                        # mutate(value=dollar(value))%>%
                                        # spread(var,value)
    
                                        # Jodi added this scrollbar to the table that is too long
    datatable(
      data = ds,
      options = list(searching = FALSE,
                     pageLength = 5,
                     lengthMenu = c(5, 10, 15, 20),
                     scrollX = T))
  })
  
  
  schedule_overview <- reactive({
    #' TODO: This logic looks backwards.
    schedule <- schedule %>%
      filter(IP %in% ip_selected()$ips) %>%
      left_join(all_proj) %>%
      select(IP = IP)

    if(input$selectdir == "All") {
      schedule <- schedule
    }

    return(schedule)
  })
  
  no_completed_schedule_overview <- reactive({
    #' TODO: This logic looks backwards.
    no_completed_schedule <- no_completed_schedule %>%
      filter(IP %in% ip_selected()$ips) %>%
      left_join(all_proj %>% select(IP = IP))
    
    if(input$selectdir == "All") {
      no_completed_schedule <- no_completed_schedule
    }
    
    return(no_completed_schedule)
  })

  output$timevis_plot_all <- timevis::renderTimevis({
    #' @description: plot all projects.
    #' 
    #' @todo: Should have just one function for all and specific...
    #' @todo: dates at bottom look wrong
    #' @todo: add colors
    #' @todo: group projects, timevis has grouping abilities.
    
    df <- schedule %>%
      filter(year(Approved_finish_date) >= year(today()))

    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),
           "There is no information on Approved_finish_date")
    ))
    
    shiny::validate((
      need(any(!is.na(df$Actual_date)),
           "There is no information on Actual_date")
    ))
    
    shiny::validate((
      need(any(!is.na(df$Schedule.Health.Standard)),
           "There is no information on Schedule.Health")
    ))
    
    makeContent <- function(df) {
      #' @description: Take the information found in the dataframe, and use it
      #' to make colorful stuff inside the timevis boxes. 
      
      status <- NA 

      #' This is just an example code I copied, it won't work, it's just to
      #' get you started:
      #'        
      sprintf(
        "<table><tbody>
        <tr><td>%s<br>%s</td></tr>
        </tbody></table>",
        df["Directorate"], df["Major.Milestone"], df["Schedule.Health.Standard"])

    }
    content <- apply(df, 1, makeContent)
    data <- tibble(
      id      = 1:nrow(df),
      content = content,
      start   = format(df["Approved_finish_date"][[1]], "%Y-%m-%d"),
      end     = rep(NA, nrow(df)),
      group = df["IP"]
    )
    data_groups <- tibble(id=unique(df["IP"]), content=unique(df["IP"]))
    timevis(data, groups=data_groups)
  })


  output$timevis_plot_individual <- timevis::renderTimevis({

    df <- schedule %>%
      filter(IP == ip_selected()$ip)

    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),
           "There is no information on Approved_finish_date")
    ))
    
    shiny::validate((
      need(any(!is.na(df$Actual_date)),
           "There is no information on Actual_date")
    ))
    
    shiny::validate((
      need(any(!is.na(df$Schedule.Health.Standard)),
           "There is no information on Schedule.Health")
    ))

    data <- tibble(
      id      = 1:nrow(df),
      content = df["Major.Milestone"],
      start   = format(df["Approved_finish_date"][[1]], "%Y-%m-%d"),
      end     = rep(NA, nrow(df))
    )

    timevis(data)    
  })
  
  
  
  output$schedule_plt2 <- renderPlot({
    df <- schedule_overview() %>%
      filter(!is.na(Approved_finish_date)) %>%
      filter(if(Schedule.Health.Standard == "completed") {
               Actual_date >= as.IDate(paste0(as.character(year(now())), "-01-01"))})

    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),
           "There is no information on project schedule")
    ))

    incProgress(0.5)

    timeplot(df)
  })

  output$schedule_tb <- renderDataTable({
    df<-schedule%>%filter(IP==ip_selected()$ip)%>%
      select(Milestone=Major.Milestone,
             `Baseline Finish Date`=Approved_finish_date,
             `Actual/Forecasted Finish Date`=Actual_date)

    datatable(df,options = list(dom = 'tip'), rownames = FALSE)
  })

  output$schedule_tb2 <- renderDataTable({

    df <- schedule_overview() %>%
      select(Milestone = Major.Milestone,
             `Baseline Finish Date` = Approved_finish_date,
             `Actual/Forecasted Finish Date` = Actual_date)

    datatable(data = df,
              options = list(dom = 'tip'), rownames = FALSE)
    
  })

                                        # ========= End of Schedule
                                        # ========= Project Risk ----

  output$proj_risk_tb<-DT::renderDataTable({
    options<- list(pageLength=5,
                   scrollX=TRUE,
                   autoWidth=T,
                   columnDefs=list(list(width='500px',targets=2),
                                   list(width='50px',targets=3),
                                   list(width='50px',targets=4)))

    df<-proj_risk%>%filter(IP == input$selectip)%>%
      select(3:7)
    datatable(df,options=options)%>%
      formatStyle('Probability',
                  backgroundColor=styleEqual(c("Green","Yellow","Red"),
                                             c( "#00B050", "#FFC000", "#C00000"))
                  )%>%
      formatStyle('Impact',
                  backgroundColor=styleEqual(c("Green","Yellow","Red"),
                                             c( "#00B050", "#FFC000", "#C00000")))

  })
  
  output$proj_issue_tb<-DT::renderDataTable({
    
    options<- list(pageLength=5,
                   scrollX=TRUE,
                   autoWidth=T,
                   columnDefs=list(list(width='500px',targets=2),
                                   list(width='40px',targets=3)))
    
    df<-proj_issue%>%filter(IP == input$selectip)%>%
      select(3:7)
    datatable(df,options=options)%>%
      formatStyle('Impact',
                  backgroundColor=styleEqual(c("Green","Yellow","Red"),
                                             c( "#00B050", "#FFC000", "#C00000"))
                  )
    
  })
  
  output$projrisk<-renderPlot({
    
    shiny::validate({
      need(nrow(proj_risk%>%
                filter(IP %in% ip_selected()$ips & !is.na(Risk)))>0,'Data Not Available')
    })
    
    
    proj_risk %>%
      filter(IP %in% ip_selected()$ips & !is.na(Risk)) %>%
      count(x = Risk,
            sort = TRUE) %>%
      mutate(
        Risk = reorder(Risk, n)) %>%
      ggplot(aes(x=Risk,y=n))+geom_col(fill='#1f77b4') +
      scale_y_continuous(breaks=c(0,2,4,6,8)) +
      labs(x='',y='') +
      geom_text(aes(label=n,hjust=-1)) +
      coord_flip() +
      theme_minimal() +
      theme(axis.title.x=element_blank(),
            axis.text.x =element_text(size=10),
            axis.text.y =element_text(size=11),
            axis.title.y =element_blank())
    
    
  })
  
  
  
  
  output$projissue<-renderPlot({
    
    
    shiny::validate({
      need(nrow(proj_issue%>%
                filter(IP %in% ip_selected()$ips & !is.na(Issue)))>0,'Data Not Available')
    })
    
    proj_issue%>%
      filter(IP %in% ip_selected()$ips & !is.na(Issue))%>%
      count(Issue,sort=TRUE)%>%
      mutate(Issue=reorder(Issue,n))%>%
      ggplot(aes(x=Issue,y=n))+geom_col(fill='#1f77b4')+
      scale_y_continuous(breaks=c(0,2,4,6,8))+
      labs(x='',y='')+
      geom_text(aes(label=n,hjust=-1))+
      coord_flip()+
      theme_minimal()+
      theme(axis.title.x=element_blank(),
            axis.text.x =element_text(size=10),
            axis.text.y =element_text(size=11),
            axis.title.y =element_blank())
    
  })
  
  
  output$overall2 <- renderPlotly({
    #' IP Projects

    status$IP2 <- paste0(status$IP)
    
    df <- status %>%
      filter(grepl("\\d", IP)) %>%
      filter(`Overall Project Health` != 'Blue') %>%
      filter(IP %in% ip_selected()$ips) %>%
      left_join(budget[,c('IP','Approved Budget')])
    
    df$status<-factor(df$status,levels=c('On-Track','Caution','Delayed'))
    
    p <- status_plot(df, "IP Projects")
    
    ggplotly(p, tooltip = 'text') %>%
      config(displayModeBar = F) %>%
      layout(xaxis = list(showgrid = F),
             yaxis = list(showgrid = F))
  })
  
  output$overall3 <- renderPlotly({
    #' Innovation Projects

    status$IP2<-paste0(status$IP)
    
    df<-status %>%
      filter(!grepl("\\d", IP)) %>%
      filter(`Overall Project Health`!='Blue')%>%
      filter(IP %in% ip_selected()$ips)%>%
      left_join(budget[,c('IP','Approved Budget')])
    
    df$ status <- factor(df$status,levels=c('On-Track','Caution','Delayed'))
    
    p <- status_plot(df, "Innovation Projects")
    
    ggplotly(p, tooltip = 'text') %>% 
      config(displayModeBar = F) %>%
      layout(xaxis = list(showgrid = F),
             yaxis = list(showgrid = F))
  })

  
  output$overall_project_health <- renderUI({
    fluidRow(width = 12,
             box(title='IP Projects: Health',
                 width = 7,
                 withSpinner(plotlyOutput('overall2'))),

             box(title = "Innovation Projects: Health",
                 width = 5,
                 withSpinner(plotlyOutput("overall3"))))
  })


  output$project_portfolio_budget <- renderUI({
    #' @comment: Valid colors are: red, yellow, aqua, blue, light-blue, green,
    #' navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
    
    fluidRow(
      boxPlus(
        width         = 12,
        icon          = "fa fa-heart",
        background    = NULL,
        boxToolSize   = "md",
        closable      = F,
        collapsible = T,
        footer        = "This is a test.",
        title = "Project Portfolio Budget",
       
        fluidRow( 
          width = NULL,
          height = NULL,
        boxPlus(
          width  = 12,
          title = "Breakdown by Year",
          footer = "This is some extra info.",
          status = NULL,
          solidHeader = T,
          background = NULL,
          collapsible = T,
          closable = F,
          enable_label = FALSE,
          label_text = NULL,
          label_status = "primary",
          enable_dropdown = FALSE,
          dropdown_icon = "wrench",
          dropdown_menu = NULL,
          enable_sidebar = FALSE,
          sidebar_content = NULL,
          sidebar_title = NA_character_,
          sidebar_width = 25,
          sidebar_background = "#222d32",
          sidebar_start_open = FALSE,
          sidebar_icon = "cogs",
          footer_padding = TRUE,
          withSpinner(plotlyOutput("break_down_by_year")))),
       
        fluidRow( 
        boxPlus(
          title = "Projections",
          solidHeader = T,
          collapsible = T,
          closable      = F,
          background = NULL,
          footer = "This is some info.",
          datatable(df_budget_summary())),
        
        boxPlus(
          solidHeader = FALSE,
          title = "Projections",
          background = NULL,
          width = 12,
          status = "danger",
          footer = fluidRow(
            column(
              width = 6,
              descriptionBlock(
                header = amount_expenditure_to_date(), 
                numberColor = "green", 
                text = "Expenditures to Date", 
                rightBorder = TRUE,
                marginBottom = TRUE 
              )
            ),
            column(
              width = 6,
              descriptionBlock(
                number = amount_forcasted_total_expenditures(), 
                numberColor = "green", 
                numberIcon = "fa fa-caret-up",
                header = "$35,210.43", 
                text = "TOTAL REVENUE", 
                rightBorder = F,
                marginBottom = T
              )
            )),
           fluidRow( 
            column(
              width = 6,
              descriptionBlock(
                number = amount_approved_budget(), 
                numberColor = "green", 
                numberIcon = "fa fa-caret-up",
                header = "$35,210.43", 
                text = "TOTAL REVENUE", 
                rightBorder = T,
                marginBottom = F
              )
            ),
            column(
              width = 6,
              descriptionBlock(
                number = amount_projected_forecasted_expenditures(),
                numberColor = "red", 
                numberIcon = "fa fa-caret-down",
                header = "1200", 
                text = "GOAL COMPLETION", 
                rightBorder = F,
                marginBottom = F
              )
            )
          )
        ),
        
        boxPlus(
          width         = 12,
          icon          = "fa fa-heart",
          background    = NULL,
          boxToolSize   = "md",
          closable      = F,
          collapsible   = T,
          footer        = "This is a test.",
          title = "Project Health and Current Stage",
          datatable(summary_status_and_count()) 
          
          ))))
    
  })
  
  
  summary_status_and_count <- reactive({
    #' @todo: is this code necessary for anything?    
    #' all_proj$IP2<-paste0(all_proj$IP,':\n',substr(all_proj$`Internal or External`,1,1))
    #' 

    
    .status <- function(col) {
     col  
    }
    
    .project_name_badges <- function(col) {
      ..label <- function(x) dashboardLabel("Label 1", status = "info")
      paste(col, collapse = '\n')
    }
    
    df <- all_proj %>%
      filter(IP %in% ip_selected()$ips) %>%
      group_by(stage, status) %>%
      summarise(
        IP     = .project_name_badges(IP),
        count  = n(),
        status = .status(status))
    
    })
  
  
  output$overall <- renderValueBox({
    status<-all_proj%>%
      filter(IP == ip_selected()$ip)%>%
      select(status,`Overall Project Health`)
    
    
    valueBox(tags$p(status$`Overall Project Health`, style = "font-size: 80%;"),
             subtitle='Overall Project Health',color=tolower(status$`Overall Project Health`),
             width=3)
    
  })
  
  output$overall_stage<-renderValueBox({
    status<-all_proj%>%
      filter(IP ==input$selectip)%>%
      pull(stage)
    
    valueBox(tags$p(status, style = "font-size: 80%;"),
             subtitle='Project Stage',color='purple',width=3)
  })
  
  output$directorate <- renderValueBox({
    
    internal <- all_proj %>%
      filter(IP == input$selectip) %>%
      pull(`Directorate`)

    valueBox(tags$p(internal, style = "font-size: 80%;"),
             subtitle='Directorate ',color='maroon',width=3)
  })
  
  output$completed<-renderValueBox({
    valueBox(
      value = nrow(schedule_completed),
      subtitle = 'Completed',
      color = 'light-blue')
  })
  
  output$delayed<-renderValueBox({
    valueBox(
      value = nrow(covid_delayed),
      subtitle = 'Delayed',
      color='light-blue')
  })

  
  #' Number of Projects in Each of Stage 1, 2, 3, 4
  output$stage_1 <- reactive({nrow(stage_1)})
  output$stage_2 <- reactive({nrow(stage_2)})
  output$stage_3 <- reactive({nrow(stage_3)})
  output$stage_4 <- reactive({nrow(stage_4)})

   
  #' Number of Projects in Planning or Testing 
  output$planning <- reactive({nrow(planning)})
 
  #' nrow(testing)
  output$testing <- reactive({1})
 
  
  output$caption <- renderText({
    text="*** tasks completed before 2020 are hidden in main plot ***"
  })
  
  
  output$download_data<-downloadHandler(
    
    filename<-function(){
      paste('TBI Dashboard','xlsx',sep='.')
    },
    
    content<-function(file){
      file.copy('dattbi.xlsx',file)
    }
  )
  
  output$downloadreport_overview <- downloadHandler(
    
    filename = function() {
      paste0("TBI Investment Projects", '-', Sys.Date(), ".pdf")},
    
    content = function(file) {
      #' @description: first call plotPNG to save some of the plots as pictures.

      .make_timevis <- function() {
        df <- schedule %>%
          filter(IP == ip_selected()$ip)
        data <- tibble(
          id      = 1:nrow(df),
          content = df["Major.Milestone"],
          start   = format(df["Approved_finish_date"][[1]], "%Y-%m-%d"),
          end     = rep(NA, nrow(df))
        )
        timevis(data)
      }

      saveWidget(widget        = .make_timevis(),
                 file          = "timevis.html",
                 selfcontained = F)      

      markdown_source <- normalizePath("report_overall.Rmd")
      overall_source  <- normalizePath("overall.png")
      
      #' temporarily switch to the temp dir, in case you do not have write
      #' permission to the current working directory

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      file.copy(markdown_source, "report_overall.Rmd", overwrite = TRUE)
      file.copy(overall_source,  "overall.png")
      
      out <- render("report_overall.Rmd", pdf_document())
      
      file.rename(out, file)
    }
  )
  
  output$downloadreport_individual<-downloadHandler(
    
    filename=function(){
      paste0(ip_selected()$ip,'-',project_selected()$names,'-',Sys.Date(),'.pdf')
    },
    
    content = function(file) {
      src <- normalizePath('report_individual.Rmd')
      img_name <- paste0(ip_selected()$ip,'.png')
      src2 <- normalizePath(img_name)
      
      #' temporarily switch to the temp dir, in case you do not have write
      #' permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report_individual.Rmd', overwrite = TRUE)
      file.copy(src2, img_name)
      
      library(rmarkdown)
      out <- render('report_individual.Rmd', pdf_document())
      file.rename(out, file)
    }
    
  )
  
})
