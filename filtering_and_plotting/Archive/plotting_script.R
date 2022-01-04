# basic plot
starting_plot <- ggplot(data = filtered_data,
                        aes(Year, Value)) +
  geom_point()



if(is.na(facet_row)||is.na(facet_column)) {
  
  facet_row <- sym(facet_row)
  
  filtered_data <- filtered_data %>% 
    mutate(facet_row = !!(enquo(facet_row)))
  
  faceted_plot <- starting_plot+
    facet_grid(. ~ facet_row)
} else {
  facet_row <- sym(facet_row)
  facet_column <- sym(facet_column)
  
  filtered_data <- filtered_data %>% 
    mutate(facet_row = !!(enquo(facet_row)),
           facet_column = !!(enquo(facet_column)))
  
  faceted_plot <- starting_plot+
    facet_grid(facet_row ~ facet_column)
}


if(!is.na(line_colour)) {
  
  line_colour <- sym(line_colour)

  filtered_data <- filtered_data %>% 
    mutate(line_colour = !!(enquo(line_colour)))
  
  final_plot <- faceted_plot +
    geom_line(aes(color = !!line_colour))

} else {
  
  final_plot <- faceted_plot +
    geom_line()
}
