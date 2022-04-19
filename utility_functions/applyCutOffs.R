applyCutOffs = function(scales, questionnaireScripts){
  print("applyCutOffs")
  
  
  qS = questionnaireScripts %>%
    filter(name == "cutoffs")
  
  if(is_empty(qS)){
    showNotification(
      "No cutoffs file found.
      You need to add these evaluation routines to your questionnaires",
      type = "error",
      duration = 20)
    session$close()}
  

 
 
cutoffs = data.frame()

  for (r in 1:nrow(qS)){
    cutoffs = data.frame(questionnaireVersionId = qS$questionnaireId[r], fread(qS$scriptText[r])) %>%
      bind_rows(cutoffs)
    
  }
 
 
result = scales %>% 
  full_join(cutoffs, by = c("questionnaireVersionId" = "questionnaireVersionId", "variable" = "scale")) %>%
  filter((low_cut <= value & high_cut > value) | (is.na(low_cut) & is.na(high_cut))) 



 return(result)

}




