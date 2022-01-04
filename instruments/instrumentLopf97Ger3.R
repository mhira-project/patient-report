instrumentLopf97Ger3 = function(mhiraData){ 
print("instrumentLopf97Ger3")
#Packages ----
library(tidyverse)


# Settings ----   
  
# This script works for these questionnaires
Quest =  c("LoPF-Q_12_18")


# LOPFscore = function(df){dfx  = df
# for (i in 1:nrow(dfx)) {
#   missinglimit = ifelse(ncol(dfx) > 20,3,2)   # bei 20 Items in einer Subskala dürfen 3 fehlen, sonst nur 2
#   if(sum(is.na(dfx[i,])) <= missinglimit  ){dfx[i,is.na(dfx[i,])] <- 0}  # Fehlende Werte werden durch 0 ersetzt
# }
# round(rowSums(dfx))
# }




data = mhiraData

if (!any(data$questionnaireName %in%  Quest)){return(NULL)}

#
# Rearragne data ----

df = data %>% select(name, questionnaireName, textValue, assessmentName) %>% pivot_wider(id_cols = c(assessmentName, questionnaireName), names_from = name, values_from = textValue)
df = df %>% filter(questionnaireName == Quest) %>% discard(~all(is.na(.)))
df = df %>% mutate_at(.vars = vars(starts_with("LOPF")), .funs = as.numeric )

print("some date is missing to make the assessment name unique")

# Item patterns ----
recode<-c("LOPF77", "LOPF97","LOPF88", "LOPF76", "LOPF82", "LOPF91", "LOPF81", "LOPF92","LOPF96","LOPF28","LOPF5","LOPF16","LOPF8","LOPF36","LOPF21","LOPF18","LOPF65","LOPF73","LOPF17","LOPF44","LOPF42","LOPF50","LOPF10","LOPF70","LOPF1")
identity<-c("LOPF77", "LOPF97", "LOPF88", "LOPF76", "LOPF82", "LOPF84", "LOPF95", "LOPF91", "LOPF85", "LOPF81", "LOPF92", "LOPF83", "LOPF86", "LOPF79", "LOPF78", "LOPF87", "LOPF96", "LOPF93", "LOPF75", "LOPF90", "LOPF80", "LOPF89", "LOPF94")
#cont<- c("LOPF77", "LOPF97", "LOPF88", "LOPF76", "LOPF82", "LOPF84", "LOPF95", "LOPF91", "LOPF85", "LOPF81", "LOPF92")
#coh<-c("LOPF83", "LOPF86", "LOPF79", "LOPF78", "LOPF87", "LOPF96", "LOPF93", "LOPF75", "LOPF90", "LOPF80", "LOPF89", "LOPF94")
selfDirection<-c("LOPF3", "LOPF26", "LOPF41", "LOPF32", "LOPF61", "LOPF66", "LOPF72", "LOPF27", "LOPF33", "LOPF52", "LOPF71","LOPF62", "LOPF46", "LOPF19", "LOPF7", "LOPF22", "LOPF69", "LOPF11", "LOPF30", "LOPF64", "LOPF28", "LOPF60", "LOPF24", "LOPF35", "LOPF57")
#congruence<-c("LOPF3", "LOPF26", "LOPF41", "LOPF32", "LOPF61", "LOPF66", "LOPF72", "LOPF27", "LOPF33", "LOPF52", "LOPF71") 
#goal<-c("LOPF62", "LOPF46", "LOPF19", "LOPF7", "LOPF22", "LOPF69", "LOPF11", "LOPF30", "LOPF64", "LOPF28", "LOPF60", "LOPF24", "LOPF35", "LOPF57")
empathy<-c("LOPF5", "LOPF16", "LOPF53", "LOPF63", "LOPF58", "LOPF49", "LOPF25", "LOPF56", "LOPF43", "LOPF47", "LOPF8", "LOPF36", "LOPF15", "LOPF21", "LOPF18", "LOPF54", "LOPF51", "LOPF67", "LOPF68", "LOPF40", "LOPF59", "LOPF13", "LOPF34", "LOPF55", "LOPF48", "LOPF29")
#persp<-c("LOPF5", "LOPF16", "LOPF53", "LOPF63", "LOPF58", "LOPF49", "LOPF25", "LOPF56", "LOPF43", "LOPF47")
#prosocial<-c("LOPF8", "LOPF36", "LOPF15", "LOPF21", "LOPF18", "LOPF54", "LOPF51", "LOPF67", "LOPF68", "LOPF40", "LOPF59", "LOPF13", "LOPF34", "LOPF55", "LOPF48", "LOPF29")
intimacy<-c("LOPF2", "LOPF9", "LOPF31", "LOPF6", "LOPF20", "LOPF37", "LOPF4", "LOPF65", "LOPF73","LOPF17", "LOPF44", "LOPF12", "LOPF42", "LOPF23", "LOPF74", "LOPF38", "LOPF14", "LOPF45", "LOPF39", "LOPF50", "LOPF10", "LOPF70", "LOPF1")
#fhgnaehe<-c("LOPF2", "LOPF9", "LOPF31", "LOPF6", "LOPF20", "LOPF37", "LOPF4", "LOPF65", "LOPF73")
#rec<-c("LOPF17", "LOPF44", "LOPF12", "LOPF42", "LOPF23", "LOPF74", "LOPF38", "LOPF14", "LOPF45", "LOPF39", "LOPF50", "LOPF10", "LOPF70", "LOPF1")

# Scales  


df = df %>% select(assessmentName, questionnaireName, starts_with("LOPF"))




df[,recode] <- 4 - df[,recode]

dfS = data.frame(
df %>% select(all_of(identity)) %>% summarise(rowSums(.)), 
df %>% select(all_of(selfDirection)) %>% summarise(rowSums(.)), 
df %>% select(all_of(empathy)) %>% summarise(rowSums(.)),
df %>% select(all_of(intimacy)) %>% summarise(rowSums(.)) 
)
colnames(dfS) = c('identity', 'selfDirection', 'empathy', 'intimacy')

dfS$selfDirection_t = round((( dfS$selfDirection-30.7)/16.9)*10)+50
dfS$empathy_t = round((( dfS$empathy-31.3)/13.4)*10)+50
dfS$intimacy_t = round((( dfS$intimacy-23.5)/11.8)*10)+50
dfS$identity_t = round((( dfS$identity-28.3)/13.6)*10)+50
# LoPF$Sl_Selbstkongruenz_t = round((( LoPF$Sl_Selbstkongruenz-15.6)/8.8)*10)+50
# LoPF$Sl_Zielgerichtetheit_t = round((( LoPF$Sl_Zielgerichtetheit-15.1)/9.2)*10)+50

# LoPF$Emp_Perspektivenw_t = round((( LoPF$Emp_Perspektivenw-12.3)/6.1)*10)+50
# LoPF$Emp_Prosozialität_t = round((( LoPF$Emp_Prosozialität-19.0)/8.6)*10)+50

# LoPF$Int_Bindungsf_t = round((( LoPF$Int_Bindungsf-11.5)/5.9)*10)+50
# LoPF$Int_Reziprozität_t = round((( LoPF$Int_Reziprozität-12.0)/7.3)*10)+50

# LoPF$Id_Kontinuität_t = round((( LoPF$Id_Kontinuität-13.4)/6.8)*10)+50
# LoPF$Id_Kohärenz_t = round((( LoPF$Id_Kohärenz-14.9)/8.4)*10)+50


for (tscale in  grep("_t", colnames(dfS))){
  dfS[dfS[ ,tscale] < 20 & !is.na(dfS[ ,tscale]),tscale] <- 20 
  dfS[dfS[ ,tscale] > 80 & !is.na(dfS[ ,tscale]),tscale] <- 80 
}

dfS = data.frame(assessmentName = df$assessmentName, questionnaireName = df$questionnaireName , dfS)
#dfS = dfS %>% filter(questionnaireName == Quest)
dfS$total_t = dfS %>% select(ends_with('_t')) %>% rowMeans(na.rm= T)


dfS = dfS %>% select(assessmentName,questionnaireName, ends_with("_t")) %>%
                            na.omit() %>%
                            pivot_longer(values_to = "value", names_to = "variable", cols = ends_with("_t"))


# dfS$text = as.character(NA)
dfS$assessmentDate = NA
dfS$topic = "Personality_functioning"
dfS$severity = NA
dfS$Recommendation = NA
dfS$Assessment = NA
dfS$Warning = NA

dfS$Recommendation[dfS$variable == "total_t"] = "If levels of personality functioning are pathological, please consult a mental health care professional." 
dfS$Assessment[dfS$variable == "total_t"] = "Please consider the figure for level of severity"


cut_offs = tibble(topic = dfS$topic[1],
                      questionnaireName = dfS$questionnaireName[1],
                      level = factor(c('normal', 'mild', 'severe'), levels = c('normal', 'mild', 'severe')),
                      low_cut = c(40,60,70), high_cut = c(60,70,80))


return_list = list(values = dfS, cut_offs =  cut_offs)

return(return_list)

}



