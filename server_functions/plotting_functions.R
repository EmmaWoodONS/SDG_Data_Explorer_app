# This function creates a basic plot plotting X against y as a base
basic_plot <- function(df){
  ggplot(data = filtered_data,
                     aes(Year, Value)) +
  geom_point()
}


basic_plot_start <- basic_plot(filtered_data)
basic_plot_start

#This function is for facet wrapping. Will only facet by row
test_facet_args <- function(df, facet2, linecolour) {
  
  facet2 <- sym(facet2)
  linecolour <- sym(linecolour)
  
  df <- df %>%
    mutate(facet2 = !!(enquo(facet2)),
           linecolour = !!(enquo(linecolour)))
  
    facets <- geom_line(aes(colour = !!linecolour)) +
    labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
         y = "Percentage(%)", x = "") +
    facet_grid(. ~ facet2)
    
    return(facets)
}




ggplot(data = filtered_data,
       aes(Year, Value)) +
  geom_point() %+%
  test_facet_args(filtered_data, facet_row, line_colour)

#This function is for facet gridding. Will facet by row and coloumn
test_facet_args_grid <- function(df, facet1, facet2, linecolour) {
  
  facet1 <- sym(facet1)
  facet2 <- sym(facet2)
  linecolour <- sym(linecolour)
  
  df <- df %>% 
    mutate(facet1 = !!(enquo(facet1)),
           facet2 = !!(enquo(facet2)),
           linecolour = !!(enquo(linecolour)))
  
    basic_plot_start +
    geom_line(aes(colour = !!linecolour)) +
    labs(title = "Percentage of Men in all regions of England of all age ranges who are current cigarette smokers aged 18 years and older",
         y = "Percentage(%)", x = "") +
    facet_grid(facet1 ~ facet2)
  
}