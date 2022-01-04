filter_series_units <- function(){
  
  if("Units" %in% names(dat)) {
    
    if(length(unique(dat$Units) > 1)){
      dat <- filter(dat, Units == Units_selection)
    }
  }
  if("Series" %in% names(dat)) {
    
    if(length(unique(dat$Series) > 1)){
      dat <- filter(dat, Series = Series_selection) 
    }
  }
  
  return(dat)
}

get_special_characteristics <-function(){
  
  special_characteristics <- NULL
  if("Units" %in% names(dat)) {
    special_characteristics <- c(special_characteristics, "Units")
  }
  if("Series" %in% names(dat)) {
    special_characteristics <- c(special_characteristics, "Series")
  }
  return(special_characteristics)
}