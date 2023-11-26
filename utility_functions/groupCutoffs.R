groupCutoffs = function(cutoffs){
  
  g = function(scale, low_cut, high_cut, level){
    
    x = data.frame(scale, low_cut, high_cut, level, group = NA)
    
    for (s in 1:length(unique(x$scale))){
      
      if(x$group[x$scale == unique(x$scale)[s]] %>% is.na() %>% all()){
        x$group[x$scale ==unique(x$scale)[s]] <- s} else
        {next}
      
      if(!any(is.na(x$group))){next} 
    }
    
    return(x$group) 
    
  }
  
 if(cutoffs %>% nrow < 1){
   cutoffs$group <- numeric(0)
   } else {
        cutoffs =  cutoffs %>%
            group_by(questionnaireVersionId) %>%
            mutate(group =   g(scale, low_cut, high_cut, level))
          }
  
 return(cutoffs)
}
