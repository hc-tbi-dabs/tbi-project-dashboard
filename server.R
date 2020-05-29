

shinyServer(function(input, output,session) {
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  # plot_height<-reactive({
  #   
  #   n<-all_proj%>%filter(IP %in% ip_selected()$ips)%>%distinct(`Internal or External`)
  #   
  #   if(input$internal=='Yes' & nrow(n)==2){
  #     height<-600
  #   }else{
  #     height<-400
  #   }
  #   
  #   height
  # })
  # 
  # 
  # plot_height2<-reactive({
  #   
  #   n<-all_proj%>%filter(IP %in% ip_selected()$ips)%>%distinct(`Internal or External`)
  #   
  #   if(input$internal=='Yes' & nrow(n)==2){
  #     height<-800
  #   }else{
  #     height<-450
  #   }
  #   
  #   height
  # })
  
  # ========= Contact Button ----
  
  observeEvent(input$contact,{
    
    showModal(modalDialog(
      title='Contact Us',
      HTML(paste(
        "If you have any questions regarding data source or data quality, please contact:",br(),
        "Sarah-Emily Carle",br(),
        "Management, Program Support",br(),
        "Business Informatics Division",br(),
        "RMOD, HFPB",br(),
        "sarah-emily.carle@canada.ca",br(),br(),
        "If you have technical questions regarding the application, please contact:",br(),
        "Nanqing (Nancy) Zhu",br(),
        "Data Scientist",br(),
        "Business Informatics Division",br(),
        "RMOD, HFPB",br(),
        "nanqing.zhu@canada.ca"
      )),
      easyClose=T
    ))
  })
  
  # ========= End of Contact Button
  # ========= Texts/Headers of Dashboard ----
  
  # this is the "view IP" table
  output$ip_tbl<-renderTable(
    all_proj[,1:2]%>%mutate(IP=as.character(IP))
  )
  
  output$project_name<-renderUI({
    
    # Project by IP name header
    name<-all_proj%>%filter(IP== input$selectip)%>%pull(`Project`)
    project_name<-paste0('IP',input$selectip,' ',name)
    if(startsWith(project_name,"IPCipher") | startsWith(project_name,"IPCyclops") | startsWith(project_name,"IPHummingbird") | startsWith(project_name,"IPKelpie")){
      project_name<-name
    } else if(startsWith(project_name,"IPIP000")){
      project_name<-paste("IP000",name)
    }
    h2(project_name,
       style = "font-family: 'Arial';margin-left:20px;
        font-weight: bold; line-height: 1.1;
        color: #2E4053;")
  })
  
  output$project_name2<-renderUI({
    
    # "Project:All" header
    project_name<-paste0('Projects:',input$selectdir)
    h2(project_name,
       style = "font-family: 'Arial';margin-left:20px;
        font-weight: bold; line-height: 1.1;
        color: #2E4053;")
  })
  
  # ========= End of Texts/Headers of Dashboard
  # ========= Select IP Side Menu ----
  
  ip_selected<-reactive({
    
    ip<-input$selectip
    
    if(input$selectdir=='All'){
      ips<-all_proj$IP
    }else{
      ips<-all_proj$IP[all_proj$`Directorate`==input$selectdir]
    }
    
    return(list(ip=ip,ips=ips))
  })
  
  # ========= End of Select IP Side Menu 
  
  # ========= Removed Functionality Plots
  
  # ========= Project Portfolio Budget ----
  
  output$budget_all<-renderPlot({
    ds<-budget%>%filter(IP==input$selectip)%>%
      summarise(`Approved Budget`=sum(`Approved Budget`,na.rm=T),
                `Expenditure to Date`=sum(`Expenditure to Date`,na.rm=T),
                `Remaining Budget Projected`=sum(`Variance / Remaining budget`,na.rm=T))%>%
      gather(cat)
    
    budget_plot2(ds)
  })

  output$budget_all2<-renderPlot({
    
    ds<-budget%>%
      filter(IP %in% ip_selected()$ips)%>%
      left_join(all_proj%>%select(IP=IP))%>%
      summarise(`Approved Budget`=sum(`Approved Budget`,na.rm=T),
                `Expenditure to Date`=sum(`Expenditure to Date`,na.rm=T),
                `Remaining Budget Projected`=sum(`Variance / Remaining budget`,na.rm=T))%>%
      gather(cat,value)
    
    budget_plot2(ds)
    
  })
  
  #   ds<-budget%>%
  #     filter(IP %in% ip_selected()$ips)%>%
  #     left_join(all_proj%>%select(IP=IP))%>%
  #     summarise(`Approved Budget`=sum(`Approved Budget`,na.rm=T),
  #               `Expenditure to Date`=sum(expenditure_to_date,na.rm=T),
  #               `Remaining Budget Projected`=sum(`Variance between remaining approved budget projected spending`,na.rm=T))%>%
  #     gather(cat,value)
  #   
  #   budget_plot2(ds,TRUE)
  #   
  #   
  # })
  
  
  output$budget_plt<-renderPlotly({
    ds<-budget_yr%>%filter(IP==input$selectip)
      #spread(`Authority vs. Expenditures`,Value)%>%
    
    budget_plot(ds)
    #ggplotly(p,tooltip = "text")%>%layout(margin=list(b=50),xaxis=list(tickangle=-45))
  })
  
  
  output$budget_plt2<-renderPlotly({
    
    ds<-budget_yr%>%
      filter(IP %in% ip_selected()$ips)%>%
      #left_join(all_proj%>%select(IP=IP,internal_external=`Internal or External`))%>%
      group_by(Year,year,`Authority vs. Expenditures`)%>%
      summarise(Capital=sum(Capital,na.rm=T),
                Non_Capital=sum(Non_Capital,na.rm=T))
    
    budget_plot(ds)
    
    #ggplotly(p,tooltip = "text")%>%layout(margin=list(b=50),xaxis=list(tickangle=-45),
    #                                      legend=list(y=1,x=0.7))
    
    
  })
  
  
  output$budget_tbl<-DT::renderDataTable({
    
    ds<-budget_yr%>%filter(IP==input$selectip)%>%
      spread(`Authority vs. Expenditures`,Value)%>%
      select(-year)%>% # not sure why but big Y is needed here
      mutate_at(c('Capital','Non_Capital','Project Authority', 'Project Expenditures'),dollar)
    
    DT::datatable
    
    datatable(ds, options = list(searching = FALSE,pageLength = 5,lengthMenu = c(5, 15, 20), scrollX = T))
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
    
    DT::datatable
    
    # Jodi added this scrollbar to the table that is too long
    # https://stackoverflow.com/questions/46709404/adjusting-the-width-of-the-datatable-using-dt-in-r
    datatable(ds, options = list(searching = FALSE,pageLength = 10,lengthMenu = c(10, 15, 20), scrollX = T))
  })
  
  # ========= End of Project Portfolio Budget
  # ========= Schedule ----
  
  schedule_overview<-reactive({
    schedule<-schedule%>%filter(IP %in% ip_selected()$ips)%>%
      left_join(all_proj%>%select(IP=IP))
      # filter(grepl('Start Date|End Date|Go live',Major.Milestone,ignore.case=T))

    if(input$selectdir=='All'){
      schedule<-schedule
       # filter(grepl('Go live',Major.Milestone,ignore.case=T))
    }

    return(schedule)
  })
  
  # the schedule without any of the completed projects
  no_completed_schedule_overview<-reactive({
    no_completed_schedule<-no_completed_schedule%>%filter(IP %in% ip_selected()$ips)%>%
      left_join(all_proj%>%select(IP=IP))
    # filter(grepl('Start Date|End Date|Go live',Major.Milestone,ignore.case=T))
    
    if(input$selectdir=='All'){
      no_completed_schedule<-no_completed_schedule
      # filter(grepl('Go live',Major.Milestone,ignore.case=T))
    }
    
    return(no_completed_schedule)
  })
  

  output$schedule_plt<-renderPlotly({

    df<-schedule%>%filter(IP==ip_selected()$ip)

    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),'There is no information on Approved_finish_date')
    ))
    
    shiny::validate((
      need(any(!is.na(df$Actual_date)),'There is no information on Actual_date')
    ))
    
    shiny::validate((
      need(any(!is.na(df$Schedule.Health.Standard)),'There is no information on Schedule.Health')
    ))

    timeplot(df)%>%ggplotly(height=450,tooltip=NULL)%>%
      layout(legend=list(orientation='h', y=-10,x=0.2))
  })
  
  ## again, this is the plot in overview
  output$schedule_plt2<-renderPlot({


    df<-no_completed_schedule_overview()%>%filter(!is.na(Approved_finish_date))

    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),'There is no information on project schedule')
    ))

    incProgress(0.5)

    timeplot(df)
    # ggplotly(timeplot(df,T),height=450,tooltip=NULL)%>%
    #         layout(legend=list(orientation='h', y=-10,x=0.2))


  })

  output$schedule_tb<-DT::renderDataTable({
    df<-schedule%>%filter(IP==ip_selected()$ip)%>%
      #filter(grepl('Start Date|End Date|Go live',Major.Milestone,ignore.case=T))%>%
      select(Milestone=Major.Milestone,
             `Baseline Finish Date`=Approved_finish_date,
             `Actual/Forecasted Finish Date`=Actual_date)

    DT::datatable(df,options = list(dom = 'tip'), rownames = FALSE)
  })


  output$schedule_tb2<-DT::renderDataTable({

    df<-schedule_overview()%>%
      select(Milestone=Major.Milestone,
             `Baseline Finish Date`=Approved_finish_date,
             `Actual/Forecasted Finish Date`=Actual_date
             )

    DT::datatable(df,options = list(dom = 'tip'), rownames = FALSE)
  })

  # # ========= End of Schedule
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
    
    
    proj_risk%>%
      filter(IP %in% ip_selected()$ips & !is.na(Risk))%>%
      count(Risk,sort=TRUE)%>%
      mutate(Risk=reorder(Risk,n))%>%
      ggplot(aes(x=Risk,y=n))+geom_col(fill='#1f77b4')+
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
  
  # ========= End of Project Risk
  # ========= Project Issue ---- 
  
  
  
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
  
  # ========= End of Project Issue
  # ========= Overall Project Health ----
  
  output$overall2<-renderPlotly({
    status$IP2<-paste0(status$IP)
    status$IP2<-map(status$IP2,is.sf_proj)
    df<-status%>%
      filter(`Overall Project Health`!='Blue')%>%
      filter(IP %in% ip_selected()$ips)%>%
      left_join(budget[,c('IP','Approved Budget')])
    df$status<-factor(df$status,levels=c('On-Track','Caution','Delayed'))
    p<-status_plot(df)
    ggplotly(p,tooltip='text')#%>%layout(legend=list(y=1,x=0.8))
  })
  
  
  output$ui_output1<-renderUI({
    fluidRow(
      box(title='Overall Project Health', width=12,
          withSpinner(plotlyOutput('overall2',height=450)))
    )
  })
  
  # ========= End of Overall Project Health
  # ========= UI Boxes for Overview ----
  
  # seems like for Overview
  output$ui_output2<-renderUI({
    fluidRow(
      # box(title='Project Functionality',
      #     tabsetPanel(id='tabs',
      #                 tabPanel(title='Graph',
      #                          withSpinner(plotOutput("function_plt",height=450)))
      #     )),
      box(title='Project Portfolio Budget',
          tabsetPanel(
            tabPanel(title='Breakdown by Year',
                     withSpinner(plotlyOutput('budget_plt2',height=450))),
            tabPanel(title='Table',
                     DT::dataTableOutput('budget_tbl2')),
            tabPanel(title='Projections',
                     withSpinner(plotOutput('budget_all2',height=450))))
      ),
      box(title='Project Health and Current Stage',
          withSpinner(plotlyOutput('overall_stage2',height=490)))
    )
    
  })
  
  
  output$ui_output3<-renderUI({
    fluidRow(
      box(title='Schedule',width=12,
          withSpinner(plotOutput('schedule_plt2',height=500)),
          br(),
          br(),
          DT::dataTableOutput('schedule_tb2'))
    )
    
  })
  
  
  # ========= End of UI Boxes
  # ========= ValueBoxes for Individual ----
  
  output$overall_stage2<-renderPlotly({
    
    # all_proj$IP2<-paste0(all_proj$IP,':\n',substr(all_proj$`Internal or External`,1,1))
    
    df<-all_proj%>%
      filter(IP %in% ip_selected()$ips)%>%
      group_by(stage,status)%>%
      summarise(IP=paste(IP,collapse='\n'),count=n())
    df$status<-factor(df$status,levels=c('On-Track','Caution','Delayed','Not yet started'))
    
    p=stage_plot(df)
    
    ggplotly(p,tooltip='none')%>%
      layout(margin = list(b = 40, l=30))
    # g=ggplotGrob(p)
    # g$layout$clip[g$layout$name == "panel"] = "off"
    # grid.draw(g)
  })
  
  
  output$overall<-renderValueBox({
    status<-all_proj%>%
      filter(IP == ip_selected()$ip)%>%
      select(status,`Overall Project Health`)
    
    
    valueBox(tags$p(status$status, style = "font-size: 80%;"),
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
  
  output$directorate<-renderValueBox({
    internal<-all_proj%>%
      filter(IP ==input$selectip)%>%
      pull(`Directorate`)

    valueBox(tags$p(internal, style = "font-size: 80%;"),
             subtitle='Directorate ',color='maroon',width=3)
  })
  
  output$completed<-renderValueBox({
    count<-nrow(schedule_completed)
    
    valueBox(tags$p(count, style = "font-size: 80%;"),
             subtitle='Number of Completed Tasks',color='light-blue',width=3,icon=icon("trophy"))
  })
  
  output$delayed<-renderValueBox({
    count<-nrow(covid_delayed)
    
    valueBox(tags$p(count, style = "font-size: 80%;"),
             subtitle='Number of Delayed Tasks',color='light-blue',width=3,icon=icon("medkit"))
  })
  
  output$stage_1<-renderValueBox({
    count<-nrow(stage_1)
    
    valueBox(tags$p(count, style = "font-size: 80%;"),
             subtitle='Number of Stage One Projects',color='light-blue',width=3,icon=icon("battery-empty"))
  })
  
  output$stage_2<-renderValueBox({
    count<-nrow(stage_2)
    
    valueBox(tags$p(count, style = "font-size: 80%;"),
             subtitle='Number of Stage Two Projects',color='light-blue',width=3,icon=icon("battery-quarter"))
  })
  
  output$stage_3<-renderValueBox({
    count<-nrow(stage_3)
    
    valueBox(tags$p(count, style = "font-size: 80%;"),
             subtitle='Number of Stage Three Projects',color='light-blue',width=3,icon=icon("battery-half"))
  })
  
  output$stage_4<-renderValueBox({
    count<-nrow(stage_4)
    
    valueBox(tags$p(count, style = "font-size: 80%;"),
             subtitle='Number of Stage Four Projects',color='light-blue',width=3,icon=icon("battery-three-quarters"))
  })
  
  output$planning<-renderValueBox({
    count<-nrow(planning)
    
    valueBox(tags$p(count, style = "font-size: 80%;"),
             subtitle='Number of Covid Delayed Tasks',color='light-blue',width=3)
  })
  
  
  # ========= End of ValueBoxes
})