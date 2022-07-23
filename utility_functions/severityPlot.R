severityPlot = function(scales,  TimeOnXAxis = TRUE){
  
  s = scales %>% filter(plotGroup > 0)

if(!TimeOnXAxis){
  ggplot(s, aes(x = assessmentName, y = value, shape = scale, group = scale, fill = level)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = low_cut, ymax = high_cut ), alpha = 0.9) +
    geom_point(size = 3.5) +
    geom_line(alpha = 0.3, lwd = 2) + 
    ylim(c(min(s$scaleMin)), max(s$scaleMax)) + 
    scale_fill_brewer(type = "qual", palette = 2, direction = -1) +
    ylab("value") +
    xlab("Assessment") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
    
   
  
} else {
  ggplot(s, aes(x = assessmentDate, y = value, shape = scale, group = scale, fill = level)) +
    geom_rect(aes(xmin = min(assessmentDate) -5, xmax = max(assessmentDate) + 5, ymin = low_cut, ymax = high_cut ), alpha = 0.9) +
    geom_point(size = 3.5) +
    geom_line(alpha = 0.3, lwd = 2) + 
    ylim(c(min(s$scaleMin)), max(s$scaleMax)) + 
    scale_fill_brewer(type = "qual", palette = 2, direction = -1) +
    ylab("value") +
    xlab("Assessment Date") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    scale_x_date(date_labels = "%Y %b %d")  
}
  
  } 

