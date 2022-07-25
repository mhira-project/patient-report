applyCutOffs = function(scales, questionnaireScripts){
  print("applyCutOffs")
  
  
  
  cutoffColnames = c('scale', 'low_cut', 'high_cut', 'level', 'warning', 'text_order', 'interpretation', 'recommendation')
  
  qS = questionnaireScripts %>%
  filter(name == "cutoffs")
  
  if(nrow(qS) == 0){
    df = scales %>%
      full_join( 
        as_tibble(
          matrix(nrow = 0, ncol = length(cutoffColnames) + 1),
          .name_repair = ~ c('questionnaireId',cutoffColnames)) %>%
        mutate(scale = as.character(scale))) 
    
    df$cutoffs = NA
    
    return(df)}
  
  
  
  cutoffs = data.frame()

  for (r in 1:nrow(qS)){
   
     cutoffs = data.frame(questionnaireVersionId = qS$questionnaireId[r], fread(qS$scriptText[r])) %>%
      bind_rows(cutoffs) 
    
  }
 
requiredColumsCutoffs =  all(cutoffColnames %in% colnames(cutoffs))

if(!requiredColumsCutoffs){
  showNotification(
    "A cutoff file used in at least one of the questionnaires is missing one or more of the required columns",
    type = "error",
    duration = 20)}


cutoffs = cutoffs %>% select(c("questionnaireVersionId", all_of(cutoffColnames))) # if the cutoff files change, this needs to be adapted


result1 = scales %>% 
  full_join(cutoffs, by = c("questionnaireVersionId" = "questionnaireVersionId", "scale" = "scale")) 

dfCutoffs = result1 %>% 
    group_by(questionnaireId, scale, assessmentDateTime) %>%          
    mutate(all_cutoffs = paste0("[", level, ": ", low_cut, " - ", high_cut, "]")) %>%
    summarise(all_cutoffs = ifelse(is.na(level), NA, paste(all_cutoffs, collapse = "<br/>"))) %>%
    unique %>%
    ungroup

result2 = result1 %>%
  left_join(dfCutoffs)


result = result2 %>% 
  group_by(questionnaireId, scale) %>%
  mutate(max_scale = high_cut == max(high_cut))  %>% 
  filter((low_cut <= value & high_cut > value & max_scale == F) | (low_cut <= value & high_cut >= value & max_scale == T) | (is.na(low_cut) & is.na(high_cut))) %>% 
  select(- max_scale) %>% 
  ungroup


 return(result)

}