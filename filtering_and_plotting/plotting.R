
selections <- c(line_colour, line_style, 
                facet_row, facet_column)
number_of_selections <- length(selections[!is.na(selections)])

line_colour_sym <- as.name(line_colour)
line_style_sym <- as.name(line_style)
facet_row_sym <- as.name(facet_row)
facet_column_sym <- as.name(facet_column)

if(!is.na(facet_row)){
df <- filtered_data %>% 
  mutate(facet_row := !!enquo(facet_row_sym))
}
if(!is.na(facet_column)){
  df <- filtered_data %>% 
    mutate(facet_column := !!enquo(facet_column_sym))
}

plot <- ggplot(data = df,
               aes(Year, Value)) +
  geom_point()

if(!is.na(line_colour) & !is.na(line_style)){
  plot <- plot +
    geom_line(aes(colour = !!line_colour_sym,
                  lty = line_style))
} else if(!is.na(line_colour)) {
  plot <- plot +
    geom_line(aes(colour = !!line_colour_sym))
} else if(!is.na(line_style)) {
  plot <- plot +
    geom_line(aes(lty = line_style))
} else {
  plot <- plot +
    geom_line()
}


if(!is.na(facet_row) & !is.na(facet_column)){
  plot <- plot +
    facet_grid(facet_row ~ facet_column)
} else if(!is.na(facet_column)){
  plot <- plot +
    facet_grid(. ~ facet_column)
} else if(!is.na(facet_row)){
  plot <- plot +
    facet_grid(facet_row ~ .)
}

plot <- plot +
  theme_bw()

