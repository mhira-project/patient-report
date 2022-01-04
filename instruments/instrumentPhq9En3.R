#Packages ----



# Function ----
instrumentPhq9En3 = function(mhiraData){ 
  print("instrumentPhq9En3")
  
  # This script works for these questionnaires
  Quest =  c("PHQ-9")
  
  data = mhiraData
  
  if (!any(data$questionnaireName %in%  Quest)){return(NULL)}
  
 
  # Rearrange data ----
  df = data %>% filter(questionnaireName == Quest)
  df = df %>% select(name, questionnaireName, textValue, assessmentName) %>% pivot_wider(id_cols = c(assessmentName, questionnaireName), names_from = name, values_from = textValue)
  df = df %>% discard(~all(is.na(.)))
  df = df %>% mutate_at(.vars = vars(num_range(prefix = "q", range = 1:9)), .funs = as.numeric )
  
  dfS = df %>%  mutate(total = rowSums(across(where(is.numeric)))) %>%
    select(assessmentName, questionnaireName,  total) %>% 
    pivot_longer(cols =  total, names_to = "variable", values_to = "value")
  # Depression Severity: 0-4 none, 5-9 mild, 10-14 moderate, 15-19 moderately severe, 20-27 severe.   
  
  # Scales  ----
  dfS$assessmentDate = NA
  dfS$topic = "Depression"
  
  
   #Cutoffs ----
  cut_offs = tibble(topic = dfS$topic[1],
                    questionnaireName = dfS$questionnaireName[1],
                    level = factor(c('none', 'mild', 'moderate','moderately severe', 'severe'), levels = c('none', 'mild', 'moderate','moderately severe', 'severe')),
                    low_cut = c(0,5,10,15,20), high_cut = c(5,10,15,20,28))
  

sui_template = 'According to PhQ-9, the patient thinks about being better off dead or hurting himself/herself xFx.
                Please make sure the patient is safe or refer him/her to the emergency department or a specialist.'

dfS$sui = factor(df$q9, levels = c(1:3), labels =c("on several days", "on more than half the days", "nearly every day"))

dfS = dfS  %>% rowwise()  %>%  mutate(Warning = ifelse(is.na(sui), NA,  sub("xFx", sui, sui_template)))

 
 # Text
 dfS = dfS %>% 
   filter(variable == "total") %>%
   mutate(severity = ifelse(value < 5, "none", 
                            ifelse(value < 10, "mild", 
                                   ifelse(value < 15, "moderate", 
                                          ifelse(value < 20, "moderately severe", "severe")))))
 
 
text_template =  "According to the PhQ-9 questionnaire, the patient's deperssion level was xSx at xAx (xDx)"
rec_no = "Based on the results, no further intervention seems to be required regarding depressive symptoms."
rec_mild = "Based on the results, evaluation by a mental health professional might be required."
rec_mod_sev = "Based on the results, evaluation by a mental health professional might be required. The patient likely requires a treatment."   


                            
repl = function(severity, assessmentName, assessmentDate){
  x = sub("xSx", severity, text_template )
  x = sub("xAx", assessmentName, x )
 # x = sub("xDx", assessmentDate, x )
  return(x)
}       

dfS = dfS %>% rowwise() %>% mutate(Assessment =  repl(severity, assessmentName),
                    Recommendation = ifelse(value < 5, rec_no, 
                                            ifelse(value < 10, rec_mild, 
                                                   ifelse(value < 15, rec_mod_sev, 
                                                          ifelse(value < 20, rec_mod_sev, rec_mod_sev)))))
                          
                          
 

# Warning
 
 
 return_list = list(values = dfS, cut_offs = cut_offs)
 
 return(return_list)
}





#df = cbind(df, df %>% select(num_range(prefix = "q", range = 1:9)) %>% rename_all( ~sub("q","v",.x))) 