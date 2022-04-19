# Input: The parameter given to the function is a data frame with the columns
# representing the item data of a single questionnaire e.g., item1; item2; item3, ...
# The column names should be the variable name of the item i.e., name in the xlsform.
# The data frame has a single row as it only represents data of one questionnaire

# Output:The function should return a data frame strutctured like the example below:
#      variable       value   scaleMin scaleMax mean sd plotGroup
# 1 selfDirection_t    76        0       80      NA  NA      0
# 2       empathy_t    80        0       80      NA  NA      0
# 3      intimacy_t    80        0       80      NA  NA      0
# 4      identity_t    80        0       80      NA  NA      0
# 5         total_t    79        0       80      NA  NA      1

# The data frame has a long format if there are multiple variables 

scales_function = function(dfItmes){
  recode<-c("LOPF77", "LOPF97","LOPF88", "LOPF76", "LOPF82", "LOPF91", "LOPF81", "LOPF92","LOPF96","LOPF28","LOPF5","LOPF16","LOPF8","LOPF36","LOPF21","LOPF18","LOPF65","LOPF73","LOPF17","LOPF44","LOPF42","LOPF50","LOPF10","LOPF70","LOPF1")
  identity<-c("LOPF77", "LOPF97", "LOPF88", "LOPF76", "LOPF82", "LOPF84", "LOPF95", "LOPF91", "LOPF85", "LOPF81", "LOPF92", "LOPF83", "LOPF86", "LOPF79", "LOPF78", "LOPF87", "LOPF96", "LOPF93", "LOPF75", "LOPF90", "LOPF80", "LOPF89", "LOPF94")
  selfDirection<-c("LOPF3", "LOPF26", "LOPF41", "LOPF32", "LOPF61", "LOPF66", "LOPF72", "LOPF27", "LOPF33", "LOPF52", "LOPF71","LOPF62", "LOPF46", "LOPF19", "LOPF7", "LOPF22", "LOPF69", "LOPF11", "LOPF30", "LOPF64", "LOPF28", "LOPF60", "LOPF24", "LOPF35", "LOPF57")
  empathy<-c("LOPF5", "LOPF16", "LOPF53", "LOPF63", "LOPF58", "LOPF49", "LOPF25", "LOPF56", "LOPF43", "LOPF47", "LOPF8", "LOPF36", "LOPF15", "LOPF21", "LOPF18", "LOPF54", "LOPF51", "LOPF67", "LOPF68", "LOPF40", "LOPF59", "LOPF13", "LOPF34", "LOPF55", "LOPF48", "LOPF29")
  intimacy<-c("LOPF2", "LOPF9", "LOPF31", "LOPF6", "LOPF20", "LOPF37", "LOPF4", "LOPF65", "LOPF73","LOPF17", "LOPF44", "LOPF12", "LOPF42", "LOPF23", "LOPF74", "LOPF38", "LOPF14", "LOPF45", "LOPF39", "LOPF50", "LOPF10", "LOPF70", "LOPF1")
 
  dfItems[,recode] <- 4 - dfItems[,recode]
  
  dfS = data.frame(
    dfItems %>% select(all_of(identity)) %>% summarise(rowSums(.)), 
    dfItems %>% select(all_of(selfDirection)) %>% summarise(rowSums(.)), 
    dfItems %>% select(all_of(empathy)) %>% summarise(rowSums(.)),
    dfItems %>% select(all_of(intimacy)) %>% summarise(rowSums(.)) 
  )
  colnames(dfS) = c("identity", "selfDirection", "empathy", "intimacy")
  
  dfS$selfDirection_t = round((( dfS$selfDirection-30.7)/16.9)*10)+50
  dfS$empathy_t = round((( dfS$empathy-31.3)/13.4)*10)+50
  dfS$intimacy_t = round((( dfS$intimacy-23.5)/11.8)*10)+50
  dfS$identity_t = round((( dfS$identity-28.3)/13.6)*10)+50
  
  
  for (tscale in  grep("_t", colnames(dfS))){
    dfS[dfS[ ,tscale] < 20 & !is.na(dfS[ ,tscale]),tscale] <- 20 
    dfS[dfS[ ,tscale] > 80 & !is.na(dfS[ ,tscale]),tscale] <- 80 
  }

  dfS$total_t = dfS %>% select(ends_with("_t")) %>% rowMeans(na.rm= T)
  
  
  dfS = dfS %>% select(ends_with("_t")) %>%
    na.omit() %>%
    pivot_longer(values_to = "value", names_to = "variable", cols = ends_with("_t"))
  
  
 dfS =  data.frame(dfS, scaleMin = 0, scaleMax = 80, mean = NA, sd = NA, plotGroup = c(0,0,0,0,1))

  
return(dfS)
  
}
