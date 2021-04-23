library(tidyverse)
library(ggplot2)
library(dplyr)

threeaone_function_test <- read.csv("H:\\3-a-1 (1).csv", na.strings=c("","NA")) %>% 
  mutate_if(is.factor, as.character)


color_select <- "Sex" # Color
facet1 <- "Age" # Facet wrap
facet2 <- "Country" # Facet wrap


simple_plot <- function(df, color_select, facet1, facet2) {
  
  # test <- ggplot(data = df, 
  #                aes(Year, 
  #                    Value, 
  #                    color = color_select))+
  #   geom_line()+
  #   geom_point(color = "steelblue") +
  #   labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
  #        y = "Percentage(%)", x = "") +
  #   facet_grid(facet1 ~ facet2)
  # return(test)
}

test_facet_args <- function(df, color_select, facet1, facet2) {
  

  df <- df %>% 
    mutate(facet1 = !!(enquo(facet1)),
           facet2 = !!(enquo(facet2)))
  
  ggplot(data = df, 
         aes(Year, Value, color = color_select))+
    geom_line() +
    geom_point(color = "steelblue")+
    labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
         y = "Percentage(%)", x = "") +
    facet_grid(facet1 ~ facet2)
  
}

plot <- test_facet_args(threeaone_function_test, Sex, Age, Country)

test_plot <- simple_plot(threeaone_function_test, color_select = Sex, facet1 = Age, facet2 = Country)




ggplot(data = threeaone_function_test, aes(Year, Value, color = Sex))+
  geom_line()+
  geom_point(color = "steelblue")+
  labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
       y = "Percentage(%)", x = "")+
  facet_grid(Age ~ Country)

