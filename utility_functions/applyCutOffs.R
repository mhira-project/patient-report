applyCutOffs = function(scales, questionnaireScripts){
  print("applyCutOffs")
  
  
  qS = questionnaireScripts %>%
    filter(name == "cutoffs")
  

cutoffs = data.frame()

  for (r in 1:nrow(qS)){
   
     cutoffs = data.frame(questionnaireVersionId = qS$questionnaireId[r], fread(qS$scriptText[r])) %>%
      bind_rows(cutoffs) 
    
  }
 

cutoff_colnames = c('questionnaireVersionId', 'scale', 'low_cut', 'high_cut', 'level', 'warning', 'text_order', 'interpretation', 'recommendation')
required_colums_cutoffs =  all(cutoff_colnames %in% colnames(cutoffs))

if(!required_colums_cutoffs){
  showNotification(
    "A cutoff file used in at least one of the questionnaires is missing one or more of the required columns",
    type = "error",
    duration = 20)}


cutoffs = cutoffs %>% select(all_of(cutoff_colnames)) 


result = scales %>% 
  full_join(cutoffs, by = c("questionnaireVersionId" = "questionnaireVersionId", "variable" = "scale")) %>%
  group_by(questionnaireId, variable) %>% 
  mutate(max_scale = high_cut == max(high_cut))  %>% 
  filter((low_cut <= value & high_cut > value & max_scale == F) | (low_cut <= value & high_cut >= value & max_scale == T) | (is.na(low_cut) & is.na(high_cut))) %>% 
  select(- max_scale) %>% 
  ungroup


 return(result)

}
