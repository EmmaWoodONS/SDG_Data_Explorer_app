library(tidyverse)
library(ggplot2)
library(dplyr)

eightfiveone_test1<- read.csv("H:\\8-5-1 (1).csv")



#plotting Full time > England + Wales > Men 
eightfiveone_clean <- eightfiveone_test1 %>% 
  select(-Observation.status, -Unit.multiplier, -Unit.measure, -GeoCode) %>% 
  filter(Working.Pattern == "Full Time") %>%
  filter((Country == "England" | Country == "Wales") & Region == "") %>% 
  filter(Sex == "Men")


ggplot(eightfiveone_clean, aes(Year, Value, color = Country))+
  geom_line()+
  geom_point(color = "steelblue")+
  labs(title = "Average hourly earnings of Full time workers in Wales and England who are men",
       y = "Average hourly earnings in GBP(£)", x = "")
  
