library(plotly)

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


budget_plot<-function(ds){
  
  ds$total<-ds$Capital+ds$Non_Capital
  
  ds<-ds%>%
    mutate(label=case_when(`Authority vs. Expenditures`=='Project Expenditures' ~ paste0('Expenditure:$',prettyNum(Non_Capital,big.mark=',')),
                           `Authority vs. Expenditures`=='Project Authority' & Capital !=0 ~ paste0('Authority: $',prettyNum(total,big.mark=','),'\n',
                                                                           'Non-Capital: $',prettyNum(Non_Capital,big.mark=',')),
                           `Authority vs. Expenditures`=='Project Authority' & Capital ==0 ~ paste0('Authority: $',prettyNum(total,big.mark=','))))
  
  
  plot_ly(ds%>%filter(`Authority vs. Expenditures`=='Project Authority'),x=~year-0.2,y=~Capital,type='bar',name='Project Authority - Capital',
          marker=list(color='rgb(252,205,201)'),
          hoverinfo='text',
          text=~paste('Capital: $',prettyNum(Capital,big.mark=','))) %>%
    add_trace(y=~Non_Capital,name='Project Authority - Non-Capital',marker=list(color='rgb(248,118,109)'),
              hoverinfo='text',
              text=~ label)%>%
    layout(barmode='stack',yaxis=list(title='Budget')) %>%
    add_trace(data=ds%>%filter(`Authority vs. Expenditures`=='Project Expenditures'),x=~year+0.2,y=~Non_Capital,type='bar',name='Project Expenditure',
              marker=list(color='rgb(0,191,196)'),
              hoverinfo='text',
              text=~label)%>%
    layout(xaxis=list(title='Fiscal Year',
                      ticktext=list("2016-17","2017-18","2018-19","2019-20","2020-21","2021-22","2022-23"),
                      tickvals = list(2016, 2017, 2018, 2019, 2020,2021,2022),
                      tickmode='array')) %>%
    layout(legend=list(y=1,x=0.7))
}

dollar_y <- function(x){
  paste0('$', round(x / 10 ^ 6, 1), 'M')
}

budget_plot2<-function(ds){
  
  min<-ifelse(min(ds$value)<0,abs(min(ds$value))*-1.2,0)
  max<-max(ds$value)*1.2
  
    
  ds$col<-ifelse(ds$value>=0,'#1f77b4','#980008')
  
  p<-ggplot(ds,aes(x=cat,y=value,fill=col))+geom_bar(stat='identity',position='dodge')+
    scale_fill_manual(values=c('#1f77b4','#980008'))+
    guides(fill=FALSE)
  
  p+scale_y_continuous(labels=dollar_y,limits=c(min,max))+
    labs(y='Budget Amount')+
    geom_text(aes(label=dollar(value),vjust=ifelse(value>0,-1,1.5)),position = position_dodge(width = 1))+
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.text.x =element_text(size=11,family='sans',color='#494949'),
          legend.text=element_text(size=12,family='sans',color='#494949'),
          legend.justification = 'top')+
    coord_flip()
  
}

# ========= Timeplot ----

timeplot<-function(df){  # removed argument internal
  
  status_levels <- c(
    "forecasted completion date is within 3 months of baseline date", # green
    "forecasted completion date within 3-6 months of baseline date", # yellow
    "forecasted completion over 6 months of baseline date", # red
    "completed" # black
  )
  
  status_colors <- c( "#00B050", "#FFC000", "#C00000", "#000000")
  
  df$Schedule.Health.Standard <- factor(df$Schedule.Health.Standard,
                                        levels = status_levels,
                                        ordered = TRUE)
  
  positions <- c(0.4, -0.4, 0.5, -0.5,0.9,-0.9,1.2,-1.25)
  
  directions <- c(1, -1)
  
  line_pos <- data.frame(
    "date" = sort(unique(df$Actual_date), na.last = T),
    "position"=rep(positions, length.out=length(unique(df$Actual_date))),
    "direction"=rep(directions, length.out=length(unique(df$Actual_date)))
  )
  
  df<-left_join(df,line_pos,by=c('Actual_date'='date'))
  text_offset <- 0.25
  
  df$month_count <- ave(df$Actual_date==df$Actual_date, df$Actual_date, FUN=cumsum)
  df$text_position <- (df$month_count * text_offset * df$direction) + df$position
  
  month_buffer <- 6 # was 4 but 6 makes the projects that don't have tasks in 2018-2019 look more centered
  
  # Jodi's sanity check
  
  from <- min(df$Actual_date,na.rm=TRUE) - months(month_buffer)
  to <- max(df$Actual_date,na.rm=TRUE) + months(month_buffer)
  
  if (is.na(from)) {
    from <- min(df$Actual_date,na.rm=TRUE)
  }
  
  if (is.na(to)) {
    to <- max(df$Actual_date,na.rm=TRUE)
  }
  
  month_date_range <- seq(from, to, by='month')
  month_df <- data.frame(month_date_range)
  month_df$month_format <- paste0(year(month_df$month_date_range),' ',quarters(month_df$month_date_range))
  
  month_df$month_format <- ifelse(
    month_df$month_format == lag(month_df$month_format, default=''),
    '',
    month_df$month_format)
  
  timeline_plot <- ggplot(
    data = df,
    aes(x = Actual_date,
        y = 0,
        label = Major.Milestone,
        color = Schedule.Health.Standard)) +
    
    scale_color_manual(values = status_colors,
                       labels = status_levels,
                       drop = FALSE) +
    
    labs(col="") +
    theme_classic() +
    geom_hline(yintercept = 0, color = "black", size=0.3) +
    
    # Plot vertical segment lines for milestones
    geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=Actual_date), color='black', size=0.2)+
    geom_point(aes(y=0), size=3)+  # Plot scatter points at zero and date
    geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format, angle=45),vjust=0.5, color='gray23',size=3)+
    geom_text(aes(y=text_position,label=Major.Milestone),size=3, family='sans')+
    theme(
      legend.text=element_text(size=8,color='#494949')
    )
    
  # Don't show axes, appropriately position legend
  timeline_plot<-timeline_plot+
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line.x =element_blank(),
          legend.position="bottom")
  
  return(timeline_plot)
}


is.sf_proj <- function(proj_name) {
  #' Seth's Function that Removes "IP" from SF projects
  if(startsWith(proj_name, "Cipher") | startsWith(proj_name,"Cyclops") | startsWith(proj_name,"Hummingbird") | startsWith(proj_name,"Kelpie") | startsWith(proj_name,"IP000")){
    return(proj_name)
  } else {
    return(paste0("IP", proj_name))
  }
}

status_plot<- function(df, x_axis_label) {
 
  y_max <- max(df[["Approved Budget"]])
  y_upper_limit <- y_max + 0.2 * y_max
 
  cols <- c("On-Track" = "#00B050",
            "Caution"  = "#FFC000",
            "Delayed"  = "#C00000")
  
  label = ""
  
  df %>%
    arrange(status) %>%
    ggplot(
      aes(x = as.character(IP),
          y = `Approved Budget`,
          size = `Approved Budget`,
          color = status,
          text = dollar(`Approved Budget`))) +
    scale_color_manual(values = cols) +
    geom_point(alpha = 0.3) +
    geom_text(aes(label = IP2), size = 4) +
    scale_size_continuous(
      breaks = seq(from = y_max/6, to = y_max, length.out = 6), range=c(0, 36)) +
    scale_y_continuous(
      limits = c(0, y_upper_limit),
      breaks = seq(from = y_max/6, to = y_max, length.out = 6),
      labels = dollar_y) +
    theme_minimal() +
    labs(x = x_axis_label) +
    annotate("text",
             x = length(unique(df$IP)),
             y = 17*10^6,
             label = label,
             size = 3) +
    theme(axis.text.x = element_blank(),
          legend.position = 'none')
}

stage_plot <- function(df) {
  
  cols <- c("On-Track"        = "#00B050",
            "Caution"         = "#FFC000",
            "Delayed"         = "#C00000",
            "Not yet started" = "#1f77b4")
  
  label = ""
  
  ggplot(
    data = df,
    aes(x = stage, y = count, group = status, fill = status)) +
    geom_bar(stat='identity',position='dodge',width=0.9,alpha=0.9)+
    scale_fill_manual(values=cols)+
    scale_y_continuous(breaks=c(0,1,2,3,4,5))+
    geom_text(aes(y=count-0.5,label=IP),position=position_dodge(width=0.9),size=2.5)+
    annotate("text",x=length(unique(df$stage)),y=3,label=label,size=3,color='#494949')+
    theme_minimal()+
    labs(y='Number of IT Projects')+
    theme(axis.title.x=element_blank(),
          #axis.text.x =element_text(size=12),
          legend.title=element_blank())
  #legend.justification = 'top',
  #legend.text=element_text(size=12,color='#494949'),
  #legend.spacing = unit(1.0,'cm'))
  
}

