interpretTable = function(scales, transMatrix, showScale = F, lang){
  

df = scales %>%  
  arrange(desc(assessmentDateTime), assessmentName, text_order) %>%
  select(
    time = assessmentDateTime,
    assessment = assessmentName,
    scales = variable,
    interpretation = interpretation,
    recommendation = recommendation,
    warning = warning) 



if (showScale == F){

df = df %>% 
        group_by(assessment) %>%
        mutate(interpretation = paste(interpretation %>% na.omit(), collapse = " "),
               recommendation = paste(recommendation %>% na.omit(), collapse = " "),
               warning = any(warning)) %>%
        select(-scales) %>%
        unique() 

} else {
      df$assessment[duplicated(df$assessment)] <- NA
}

df$time = as.character(df$time)


colnames(df) <- transMatrix[c("time","assessment","interpretation","recommendation", "warning"), lang]


dfRendered = df %>% 
              datatable(options = list(order = list(list(1, 'desc')))) %>% 
              formatStyle(valueColumns = transMatrix["warning", lang], # https://rstudio.github.io/DT/010-style.html
                          columns = c(transMatrix["warning", lang]), 
                          backgroundColor = styleEqual(c(TRUE), c('red')),
                          color = styleEqual(c(TRUE), c('white'))
                    ) 

return(dfRendered) 
  
}