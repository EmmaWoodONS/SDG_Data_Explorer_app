#This function is for facet wrapping. Will only facet by row
test_facet_args <- function(df, facet2, linecolour) {
  
  facet2 <- sym(facet2)
  linecolour <- sym(linecolour)
  
  df <- df %>% 
    mutate(facet2 = !!(enquo(facet2)),
           linecolour = !!(enquo(linecolour)))
  
  ggplot(data = df, 
         aes(Year, Value))+
    geom_line(aes(colour = !!linecolour)) +
    geom_point()+
    labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
         y = "Percentage(%)", x = "") +
    facet_grid(. ~ facet2)
  
}

#This function is for facet gridding. Will facet by row and coloumn
test_facet_args_grid <- function(df, facet1, facet2, linecolour) {
  
  facet1 <- sym(facet1)
  facet2 <- sym(facet2)
  linecolour <- sym(linecolour)
  
  df <- df %>% 
    mutate(facet1 = !!(enquo(facet1)),
           facet2 = !!(enquo(facet2)),
           linecolour = !!(enquo(linecolour)))
  
  ggplot(data = df, 
         aes(Year, Value))+
    geom_line(aes(colour = !!linecolour)) +
    geom_point()+
    labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
         y = "Percentage(%)", x = "") +
    facet_grid(facet1 ~ facet2)
  
}