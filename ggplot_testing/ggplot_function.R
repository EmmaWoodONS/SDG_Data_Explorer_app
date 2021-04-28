library(tidyverse)
library(ggplot2)
library(dplyr)

#change name later

threeaone_function_test <- read.csv("H:\\3-a-1 (1).csv", na.strings=c("","NA")) %>% 
  mutate_if(is.factor, as.character)


test_facet_args <- function(df, facet1, facet2, linecolour) {
  

  df <- df %>% 
    mutate(facet1 = !!(enquo(facet1)),
           facet2 = !!(enquo(facet2)),
           linecolour = !!(enquo(linecolour)))
  
  ggplot(data = df, 
         aes(Year, Value))+
    geom_line(aes(colour = linecolour)) +
    geom_point()+
    labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
         y = "Percentage(%)", x = "") +
    facet_grid(facet1 ~ facet2)
  
}

 test_facet_args(threeaone_function_test,
                 linecolour = Sex, 
                 facet1 = Age, 
                 facet2 = Country)


