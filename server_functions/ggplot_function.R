library(tidyr)
library(ggplot2)
library(dplyr)

#next things- make this function work regardless of how many characteristics the user has chosen.


setwd("server_functions") 

source("config.R")   

dat <- read.csv(paste0(csv_filepath, indicator), na.strings=c("","NA")) %>% 
  mutate_if(is.factor, as.character)


source("Filtering.R")

filtered_data <- filter_for_selections(dat)

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

 test_facet_args(filtered_data_frame,
                 linecolour = (line_colour), 
                 facet1 = facet_row, 
                 facet2 = )


