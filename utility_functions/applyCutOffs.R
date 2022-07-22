applyCutOffs = function(scales, questionnaireScripts){
  print("applyCutOffs")
  
  
  qS = questionnaireScripts %>%
    filter(name == "cutoffs")
  

 
cutoffs = data.frame()

  for (r in 1:nrow(qS)){
    cutoffs = data.frame(questionnaireVersionId = qS$questionnaireId[r], fread(qS$scriptText[r])) %>%
      bind_rows(cutoffs)
    
  }
 
 
result = scales %>% 
  full_join(cutoffs, by = c("questionnaireVersionId" = "questionnaireVersionId", "variable" = "scale")) %>%
  group_by(questionnaireId, variable) %>% 
  mutate(max_scale = high_cut == max(high_cut))  %>% 
  filter((low_cut <= value & high_cut > value & max_scale == F) | (low_cut <= value & high_cut >= value & max_scale == T) | (is.na(low_cut) & is.na(high_cut))) %>% 
  select(- max_scale)


 return(result)

}