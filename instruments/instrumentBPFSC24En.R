# The Criterion Validity of the Borderline Personality Features Scale for Children in an
# Adolescent Inpatient Setting

instrumentBPFSC24En = function(mhiraData){ 
  print("instrumentLopf97Ger3")

  # This script works for these questionnaires
  Quest =  c("BPFSC-24")
  
  data = mhiraData
  
  if (!any(data$questionnaireName %in% Quest)){return(NULL)}
  
  # Rearragne data ----
  
  df = data %>% select(name, questionnaireName, textValue, assessmentName) %>% pivot_wider(id_cols = c(assessmentName, questionnaireName), names_from = name, values_from = textValue)
  df = df %>% filter(questionnaireName == Quest) %>% discard(~all(is.na(.)))
  df = df %>% mutate_at(.vars = vars(starts_with("BPFSC")), .funs = as.numeric )
  
  # Item patterns ----
  recode<-c("BPFSC1", "BPFSC5", "BPFSC23", "BPFSC24")
  affect_instability <- c("BPFSC1", "BPFSC5", "BPFSC8", "BPFSC14", "BPFSC17", "BPFSC21")
  identity_problems <- c("BPFSC3", "BPFSC9", "BPFSC12", "BPFSC16", "BPFSC18", "BPFSC22")
  negative_relationships <- c("BPFSC2", "BPFSC6", "BPFSC10", "BPFSC13", "BPFSC20", "BPFSC24")
  self_harm <- c("BPFSC4", "BPFSC7", "BPFSC11", "BPFSC15", "BPFSC19", "BPFSC23")
  
  # Scales
  
  df = df %>% select(assessmentName, questionnaireName, starts_with("BPFSC"))
  
  df[,recode] <- 6 - df[,recode]
  
  dfS = data.frame(
    df %>% select(all_of(affect_instability)) %>% summarise(rowSums(.)), 
    df %>% select(all_of(identity_problems)) %>% summarise(rowSums(.)), 
    df %>% select(all_of(negative_relationships)) %>% summarise(rowSums(.)),
    df %>% select(all_of(self_harm)) %>% summarise(rowSums(.)) 
  )
  colnames(dfS) = c('affect_instability', 'identity_problems', 'negative_relationships', 'self_harm')
  
  dfS$affect_instability_t = 4 * dfS$affect_instability
  dfS$identity_problems_t = 4 * dfS$identity_problems
  dfS$negative_relationships_t = 4 * dfS$negative_relationships
  dfS$self_harm_t = 4 * dfS$self_harm
  
  dfS = data.frame(assessmentName = df$assessmentName, questionnaireName = df$questionnaireName , dfS)
  dfS$total_t = dfS %>% select(ends_with('_t')) %>% rowMeans(na.rm= T)
  
  dfS = dfS %>% select(assessmentName,questionnaireName, ends_with("_t")) %>%
    na.omit() %>%
    pivot_longer(values_to = "value", names_to = "variable", cols = ends_with("_t"))
  
  dfS$assessmentDate = NA
  dfS$topic = "Borderline Personality Screening"
  dfS$severity = NA
  dfS$Recommendation = NA
  dfS$Assessment = NA
  dfS$Warning = NA
  
  dfS$Recommendation[dfS$variable == "total_t"] = "If levels of personality functioning are pathological, please consult a mental health care professional." 
  dfS$Assessment[dfS$variable == "total_t"] = "Please consider the figure for level of severity"
  
  
  cut_offs = tibble(topic = dfS$topic[1],
    questionnaireName = dfS$questionnaireName[1],
    level = factor(c('normal', 'mild', 'severe'), levels = c('normal', 'mild', 'severe')),
    low_cut = c(24,66,96), high_cut = c(66,96,120)) #I'm only really sure about the 66 figure
  
  return_list = list(values = dfS, cut_offs =  cut_offs)
  
  return(return_list)
}