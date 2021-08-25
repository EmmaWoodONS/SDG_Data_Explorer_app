library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyr)
library(ggplot2)
library(dplyr)

# data("mpg", package = "ggplot2")

ui <- fluidPage(
  
  tags$img(src = "SDG_logo.png",
           align = "left",
           height = 100, width = 300),
  
  fluidRow(
    
    column(
      width = 10, offset = 1,
      tags$h2("UK Indicator Explorer"),
      tags$h5("Some text to explain what the app does and link to the UK platform"),
      
      panel(
        selectizeGroupUI(
          id = "my-filters",
          params = list(
            variable1 = list(inputId = "variable1", title = "characteristic 1:"),
            variable2 = list(inputId = "variable2", title = "characteristic 2:"),
            variable3 = list(inputId = "variable3", title = "characteristic 3:"),
            variable4 = list(inputId = "variable4", title = "characteristic 4:"))
          ),
        conditionalPanel(
          condition = "input.Select_Indicator == 'All'",
          downloadBttn(
            label = "Download indicator list",
            outputId = "downloadData",
            style = "bordered",
            color = "primary",
            size = "sm")
        ),

        conditionalPanel(
          condition = "input.Select_Indicator != 'All'",
          downloadBttn(
          label = "Download Chart",
          outputId = "downloadChart",
          style = "bordered",
          color = "primary",
          size = "sm")),
        
        uiOutput("Select Indicator"),
        
        status = "primary"),
      

      conditionalPanel(
        condition = "input.Select_Indicator != 'All'",
        # DT::dataTableOutput(outputId = "NA_as_all"))
        # textOutput("selections"))
        plotOutput("plot")),
      conditionalPanel(
        condition = "input.Select_Indicator == 'All'",
        DT::dataTableOutput(outputId = "table"))

    )
  )
)

server <- function(input, output, session) {
  
  control_sheet <- read.csv("https://raw.githubusercontent.com/EmmaWoodONS/SDG_Data_Explorer_app/main/Control_sheet/control_sheet.csv") %>% 
    mutate(across(where(is.factor), as.character)) 
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = control_sheet,
    vars = c("variable1", "variable2", "variable3", "variable4")
  )
  
  output$`Select Indicator` <- renderUI({
    selectInput("Select_Indicator", "Select Indicator", choices = c("All", unique(res_mod()$Indicator)))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(res_mod(), con, row.names = FALSE)
    }
  )
  
  output$downloadChart <- downloadHandler(
    filename = function() {
      paste(input$Select_Indicator, "_", Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot = csv(), device = "png")
    }
  )
  
  
  output$table <- DT::renderDataTable(res_mod(), rownames = FALSE)
  
  #-----------------------------------------------------------------------------
  
  csv <- reactive({
    req(input$Select_Indicator != "All")
    indicator_number <- gsub("\\.", "-", input$Select_Indicator)
    csv_filepath <- paste0("Y:\\Data Collection and Reporting\\Jemalex\\CSV\\indicator_", 
                           indicator_number, ".csv")
    csv <- read.csv(csv_filepath,
             na.strings=c("","NA")) %>%
      mutate_if(is.factor, as.character) 
    
    names(csv) <- tolower(names(csv))
    
    #-----------------------------------------------------------------------------
    # replace NAs with 'All':
    #-----------------------------------------------------------------------------
    `%not_in%` <- Negate(`%in%`)
    
    irrelevant_variables <- c("observation.status", 
                              "unit.multiplier", "unit.measure",
                              "geocode", "value", "year", 
                              "series", "units")
    
    unused_columns <- c("year", "observation.status", "unit.multiplier", "unit.measure", "geocode", "value")
    # all_variables <- remove_unused_columns(csv)
    
    unused_column_numbers <- which(colnames(csv) %in% unused_columns)
    
    all_variables <- csv

    # is_disagg_nested? i.e. does a column require a single value in any other column (not including NAs).
    # Need to know this to know whether an NA should actually be 'All'
    # For example, if
    incompletely_nested_variables <- NULL
    completely_nested_variables <- NULL

    for(i in 1:ncol(all_variables)) {

      if(i %not_in% unused_column_numbers){

        target_column_name <- colnames(all_variables)[i]

        target_column_options <- all_variables %>%
          filter(!is.na(!!as.name(target_column_name)))

        unique_target_column_options <- distinct(target_column_options, !!as.name(target_column_name))

        for(j in 1:ncol(all_variables)) {

          check_column_name <- colnames(all_variables)[j]

          if(target_column_name != check_column_name) {

            unique_check_column_options <- target_column_options %>%
              filter(!is.na(!!as.name(check_column_name))) %>%
              distinct(!!as.name(target_column_name)) %>%
              rename(unique_target_column_options = all_of(target_column_name)) %>%
              .$unique_target_column_options


            if(length(unique_check_column_options) < nrow(unique_target_column_options) &
               length(unique_check_column_options) != 0) {

              number_check_column_options <- length(unique_target_column_options)
              incompletely_nested <- data.frame(nested_variable = rep(check_column_name, times = number_check_column_options),
                                                nested_within = rep(target_column_name, times = number_check_column_options),
                                                values_given_for = unique_check_column_options)
              incompletely_nested_variables <- bind_rows(incompletely_nested_variables, incompletely_nested)

            }

          }
        }
      }
    }

    nested_variables <- as.character(incompletely_nested_variables$nested_variable)
    nested_within <- as.character(incompletely_nested_variables$nested_within)
    values_given_for <- as.character(incompletely_nested_variables$values_given_for)


    # where All does not make sense (e.g. All for Welsh-only Health Boards when England is selected), keep NAs
    incompletely_nested_NAs_to_all <- all_variables
    
    if(!is.null(incompletely_nested_variables)) {
      
      for(i in 1:nrow(incompletely_nested_variables)) {
        
        nested_variable_colname <- sym(nested_variables[i])
        nested_within_colname <- sym(nested_within[i])
        
        incompletely_nested_NAs_to_all <- incompletely_nested_NAs_to_all %>%
          # mutate(Region = ifelse(Country == "England" & is.na(Region), "England", Region)) : this is what the line below is doing as an example from 3.1.2
          mutate(!!nested_variable_colname := ifelse(!!nested_within_colname == values_given_for[i] &
                                                       is.na(!!nested_variable_colname),
                                                     "All", !!nested_variable_colname))
        
      }
      
      final_incompletely_nested_NAs_to_all <- incompletely_nested_NAs_to_all %>%
        select(incompletely_nested_variables$nested_variable)
      
      all_other_NAs_to_all <- all_variables %>%
        select(-incompletely_nested_variables$nested_variable) %>%
        replace(is.na(.) & is.character(.), "All")
      
    } else {
      
      final_incompletely_nested_NAs_to_all <- NULL
      
      
      all_other_NAs_to_all <- all_variables %>%
        replace(is.na(.) & is.character(.), "All")
      
    }


    dat_with_All <- bind_cols(final_incompletely_nested_NAs_to_all, all_other_NAs_to_all)
    
    #-----------------------------------------------------------------------------
    # filter disaggregations
    #-----------------------------------------------------------------------------
    selections <- c(input[["my-filters-variable1"]], input[["my-filters-variable2"]], input[["my-filters-variable3"]], input[[ "my-filters-variable4"]]) # c(char1, char2, char3, char4)
    selections <- selections[!is.na(selections)]
    
    number_of_selections <- length(selections)
    
    unselected_characteristics <- setdiff(names(dat_with_All), selections)
    relevant_unselected_characteristics <- setdiff(unselected_characteristics, irrelevant_variables)
    
    real_values_in_selected <- dat_with_All
   
      for(i in 1:number_of_selections){
        
        selected_var <- as.name(selections[i])
        
        real_values_in_selected <- real_values_in_selected %>%
          filter(!!selected_var != "All" &
                   !is.na(!!selected_var))
      }

      extra_dropdowns <- c()
      only_one <- NULL
      if(length(relevant_unselected_characteristics) > 0){
        for(i in 1:length(relevant_unselected_characteristics)){
          
          unselected_var <- as.name(relevant_unselected_characteristics[i])
          unique_entries <- distinct(real_values_in_selected, !!unselected_var) %>%
            rename(levels = !!unselected_var) %>%
            mutate(disaggregation = relevant_unselected_characteristics[i])
          number_of_unique_entries <- nrow(unique_entries)
          
          if(number_of_unique_entries > 1){
            extra_dropdowns <- c(extra_dropdowns, relevant_unselected_characteristics[i])

          } else {
            only_one <- bind_rows(only_one,
                                  unique_entries)
          }
        }
      } 
      
      # Series and Units also need to be included in the extra dropdown so add them
      # here to keep the original data structure
      if("series" %in% names(dat_with_All)) {
        extra_dropdowns <- c(extra_dropdowns, "series")
      }
      if("units" %in% names(dat_with_All)) {
        extra_dropdowns <- c(extra_dropdowns, "units")
      }
      
      if(length(extra_dropdowns) > 0){
        
        dropdown_column_locations <- which(names(dat_with_All) %in% extra_dropdowns)
        
        # this shouldn't be necessary but units is coming up as an extra dropdown
        # for 11-7-2: that issue needs fixing, but doing this quick fix for now
        number_of_unique_entries <- length(unique(real_values_in_selected[, dropdown_column_locations]))
        
        if(number_of_unique_entries > 1){
          extra_dropdown_levels <- real_values_in_selected[, dropdown_column_locations] %>%
            distinct()
        }
        
      } else { extra_dropdown_levels <- "none" }
      
      if(length(only_one) > 0){
        unselected_var_level <- only_one %>%
          pivot_wider(names_from = disaggregation,
                      values_from = levels)
      } else {unselected_var_level <- "none"}
      
      # real_values_in_selected doesn't include headline ('All') rows,
      # so here we do almost the same filter again, but keep the 'All' rows in
      filtered <- dat_with_All
      for(i in 1:number_of_selections){
        
        selected_var <- as.name(selections[i])
        
        filtered <- filtered %>%
          filter(!is.na(!!selected_var))
      }
      
      # filtered currently contains rows from unselected columns, that we don;t want
      # in the final data, so this right_join gets rid of them.
      if(is.data.frame(unselected_var_level)){
        filtered <- filtered %>%
          right_join(unselected_var_level, by =
                       c(names(unselected_var_level)))
      }
      
      #-------------------------------------------------------------------------
      # plotting
      #-------------------------------------------------------------------------
      
      # Eventually it would be nice to have this as an extra dropdown
      facet_row <- input[["my-filters-variable1"]]
      facet_column <- input[["my-filters-variable2"]]
      line_colour <- input[["my-filters-variable3"]]
      line_style <- input[["my-filters-variable4"]]
      
      plot_options <- c(line_colour, line_style,
                        facet_row, facet_column)
      number_of_selections <- length(plot_options[!is.na(plot_options)])

      if(!is.null(line_colour)) {line_colour_sym <- as.name(line_colour)}
      if(!is.null(line_style)) {line_style_sym <- as.name(line_style)}


      plot <- ggplot(data = filtered,
                     aes(year, value)) +
        geom_point()

      if(!is.null(line_colour) & !is.null(line_style)){
        plot <- plot +
          geom_line(aes(colour = !!line_colour_sym,
                        linetype = !!line_style_sym))
      } else if(!is.null(line_colour)) {
        plot <- plot +
          geom_line(aes(colour = !!line_colour_sym))
      } else if(!is.null(line_style)) {
        plot <- plot +
          geom_line(aes(linetype = !!line_style_sym))
      } else {
        plot <- plot +
          geom_line()
      }


      if(!is.null(facet_row) & !is.null(facet_column)){
        plot <- plot +
          facet_grid(~get(facet_row) ~ ~get(facet_column))
      } else if(!is.null(facet_column)){
        plot <- plot +
          facet_grid(. ~ ~get(facet_column))
      } else if(!is.null(facet_row)){
        plot <- plot +
          facet_grid(~get(facet_row) ~ .)
      }

      plot +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
      
      plot
    
  })

  
  # output$NA_as_all <- DT::renderDataTable(csv(), rownames = FALSE)
  # output$selections <- renderText(csv())
  output$plot <- renderPlot(csv())

}
shinyApp(ui, server)