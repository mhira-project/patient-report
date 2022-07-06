interpretTable = function(scales, showScale = F){
  

df = scales %>%  
  arrange(desc(assessmentDateTime), questionnaireShortName, text_order) %>%
  select(
    assessment = assessmentName,
    scales = variable,
    interpretation = interpretation,
    recommendation = recommendation,
    warning = warning) 

df$assessment = paste(scales$assessmentName,
                      scales$assessmentDateTime,
                      sep = " ") 


if (showScale == F){


  
df = df %>% 
        group_by(assessment) %>%
        mutate(interpretation = paste(interpretation %>% na.omit(), collapse = " "),
               recommendation = paste(recommendation %>% na.omit(), collapse = " "),
               warning = any(warning)) %>%
        select(-scales) %>%
        unique() 
        

    } else {df$assessment[duplicated(df$assessment)] <- NA}

return(df) 
  
}

# https://rstudio.github.io/DT/010-style.html