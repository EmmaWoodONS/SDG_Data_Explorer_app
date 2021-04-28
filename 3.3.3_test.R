library(ggplot2)
library(dplyr)
library(tidyverse)

three_three_three <- read.csv('H:\\3-3-3 (1).csv', na.strings=c("","NA"))


clean_three_three_three<- three_three_three %>%
  select(-Observation.status, -Unit.multiplier, -Unit.measure, -GeoCode) %>% 
  filter(Units == "Rate per 100,000 population") %>% 
  filter(Sex == "Female") %>% 
  mutate(Age = ifelse(is.na(Age), "Headline", Age)) 
  
ggplot(data = clean_three_three_three, aes(Year, Value)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") + 
  labs(title = "Malaria incidence per 100,000 for females across all ages",
       subtitle = "by UK",
       y = "Rate per 100,000", x = "Year") + 
  facet_wrap(Sex ~ Age) 


  