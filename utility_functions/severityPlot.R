severityPlot = function(scales,  TimeOnXAxis = TRUE){
  
  s = scales %>% filter(plotGroup > 0) %>% arrange(scale, assessmentDateTime)
  
  dfDummy =  s %>% # dummy data to set the limits of the y-axis
   mutate(value  = ifelse(is.na(scaleMax), value, scaleMax)) %>% 
    bind_rows(s %>%
                mutate(value = ifelse(is.na(scaleMin), value, scaleMin)))
    

  l = s %>% arrange(plotGroup, high_cut,  scale) %>% select(high_cut,  level) %>% unique  # problem with duplicated factor levels if e.g. positive = 4, and positive = 1
  
  s$level = factor(s$level, levels = l$level)
  
  if(nrow(s) < 1){return(NULL)}
  
  if (length(unique(s$assessmentDateTime)) == 1) {
    p = ggplot(s, aes(x=scale, y=value, fill = level)) +
      geom_blank(data = dfDummy)
    
    if(any(!is.na(s$low_cut)) & any(!is.na(s$high_cut))){
    p = p + 
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = low_cut, ymax = high_cut ), alpha = 0.4)} 
    
    p = p +   
      geom_segment( aes(x=scale, xend=scale, y=0, yend=value)) + 
      geom_point( size=5, color="darkblue", fill=alpha("white", 0.3), alpha=0.7, shape=21, stroke=2) +
      ggtitle(paste(s$assessmentDateTime, s$assessmentName, sep = " - ")) 
    

    
  } else {
    
    if(TimeOnXAxis){ 
      p =  ggplot(s, aes(x = assessmentDate, y = value, colour = scale, group = scale)) +
        xlab("Assessment Date") +
        scale_x_date(date_labels = "%Y %b %d")  +
        geom_blank(data = dfDummy) 
      
      if(any(!is.na(s$low_cut)) & any(!is.na(s$high_cut))){
        p = p +
          geom_rect(aes(xmin = min(assessmentDate) -1,
                        xmax = max(assessmentDate) + 1,
                        ymin = low_cut,
                        ymax = high_cut,
                        fill = level),
                     alpha = 0.9,
                     colour = "white"
                    )   
      
      }
      } else {
        
        p =  ggplot(s, aes(x = assessmentName, y = value, colour = scale, group = scale)) +
          xlab("Assessment") +
          + geom_blank(data = dfDummy)
        
        if(any(!is.na(s$low_cut)) & any(!is.na(s$high_cut))){
          p = p + 
            geom_rect(aes(
                        xmin = -Inf,
                        xmax = Inf,
                        ymin = low_cut,
                        ymax = high_cut,
                        fill = level),
                      alpha = 0.9,
                      colour = "white")}  
        
      }
    
      p = p + 
        geom_point(size=3.5,
                  fill=alpha("white", 1),
                   alpha=0.7,
                   shape=21,
                   stroke=2,
                   position = position_jitter(width = .3, height = 0, seed = 1)) 
      
      
      p = p + 
        geom_line(alpha = 0.5, 
                  lwd = 2,
                  position = position_jitter(width = .3, height = 0, seed = 1)) 
      

  
     }
    
    
  p = p +  
    theme_light() +
 #   scale_colour_brewer(palette = "Set2", direction = 1) +
    scale_fill_brewer(type = "seq", palette = "Reds", direction = 1) +
    ylab("value") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
  
  if(length(s$plotGroup %>% na.omit %>% unique) > 1){
    p = p +
      facet_wrap(. ~ plotGroup, scales = "free")}
  

  

  
  
  
  return(p)
  
} 