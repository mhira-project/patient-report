severityPlot = function(questListData){
  ggplot() +
    geom_point(data = questListData[["values"]], aes(x = assessmentName, y = value, shape = variable), size = 3.5) +
    geom_line(data = questListData[["values"]], aes(x = assessmentName, y = value, group = variable), alpha = 0.3, lwd = 2) +
    geom_rect(data = questListData[["cut_offs"]],
              aes(xmin = 0, ymin = low_cut, xmax = length(unique(questListData[["values"]]$assessmentName)) +1 , ymax = high_cut , fill = level),
              alpha = 0.3 ) +
    scale_fill_brewer(type = "seq", palette = 7) +
    ylab("value") +
    xlab("") +
    ggtitle((unique(questListData[["values"]]$questionnaireName))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))} 
