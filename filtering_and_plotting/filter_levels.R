# the user may only want to plot certain levels of those disaggs
# so after running filter_disaggregations() we need to filter for selected levels
# We could turn the following into a loop, but I'm struggling with it and 
# it's not overly important, so leaving as individual statements for now.

filter_levels <- function(dat){

  if(number_of_selections >= 1 & !is.na(levels1)[1]) {
    dat <- dat %>% 
      filter(!!rlang::sym(char1) %in% c(levels1))
  } 
  
  if(number_of_selections >= 2 & !is.na(levels2)[1]) {
    dat <- dat %>% 
      filter(!!rlang::sym(char2) %in% c(levels2))
  } 
  
  if(number_of_selections >= 3 & !is.na(levels3[1])) {
    dat <- dat %>% 
      filter(!!rlang::sym(char3) %in% c(levels3))
  }
  
  if(number_of_selections == 4 & !is.na(levels4)[1]) {
    dat <- dat %>% 
      filter(!!rlang::sym(char4) %in% c(levels4))
    
    return(dat)
  }
}