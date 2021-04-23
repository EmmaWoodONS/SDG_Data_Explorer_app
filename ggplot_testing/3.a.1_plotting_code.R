library(tidyverse)
library(ggplot2)
library(dplyr)

threeaone <- read.csv("H:\\3-a-1 (1).csv", na.strings=c("","NA")) %>% 
  mutate_if(is.factor, as.character)


threeaone_plot <- threeaone %>%
  select(-Observation.status, -Unit.multiplier, -Unit.measure, -GeoCode) %>%
  filter(is.na(Socio.economic.classification) & is.na(Ethnicity) & is.na(Country.of.birth)) %>%
  filter((is.na(Sex) | Sex == "Male") & (Country == "England")) %>%
  filter(!is.na(Region)) %>% 
  mutate(Sex = ifelse(is.na(Sex), "Headline", Sex),
         Age = ifelse(is.na(Age), "Headline", Age))
  


ggplot(threeaone_plot, aes(Year, Value, color = Sex))+
  geom_line()+
  geom_point(color = "steelblue")+
  labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
       y = "Percentage(%)", x = "")+
  facet_grid(Region ~ Age)





