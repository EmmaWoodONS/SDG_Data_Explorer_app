library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyr)
library(ggplot2)
library(dplyr)
library(shinycssloaders)

# data("mpg", package = "ggplot2")

ui <- fluidPage(
  
  tags$img(src = "SDG_logo.png",
           align = "left",
           height = 100, width = 200),
  
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
        
        downloadBttn(
          label = "Download indicator list",
          outputId = "downloadData",
          style = "bordered",
          color = "primary",
          size = "sm"),
        
        uiOutput("Select Indicator"),
        
        status = "primary"),
      conditionalPanel(
        condition = "input.Select_Indicator != 'All'",
        verbatimTextOutput("extra_variables"),
        # ),
        # 
        # conditionalPanel(
        #   condition = "input.Select_Indicator != 'All'",
        selectizeGroupUI(
          id = "extra-filters",
          params = list(
            variable1 = list(inputId = "series", title = "series:"),
            variable2 = list(inputId = "units", title = "units:"),
            variable3 = list(inputId = "variable1", title = "A:"),
            variable4 = list(inputId = "variable2", title = "B:"),
            variable5 = list(inputId = "variable3", title = "C:"),
            variable6 = list(inputId = "variable4", title = "D:"))
        ),
        actionButton("plot_button", "Plot"),
        # actionButton("reset_button", "reset")
      ),
      
      conditionalPanel(
        condition = "input.Select_Indicator != 'All'",
        # DT::dataTableOutput(outputId = "NA_as_all")),
        # textOutput("selections"))
        plotOutput("plot") %>%
          withSpinner(image = "sdgs-wheel-slow.gif")
      ),
      conditionalPanel(
        condition = "input.Select_Indicator == 'All'",
        DT::dataTableOutput(outputId = "table"))
    )
  )
)

server <- function(input, output, session) {
  
  `%not_in%` <- Negate(`%in%`)
  
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
  
  
  output$table <- DT::renderDataTable(res_mod(), rownames = FALSE)
  
  
  extra_disaggs <- callModule(
    module = selectizeGroupServer,
    id = "extra-filters",
    data = extras()[[1]],
    vars = c("series", "units",
             "variable1", "variable2", "variable3", "variable4")) 
  
  
  
  output$extra_variables <- renderText({
    req(input$Select_Indicator != "All")
    
    text <- "Some indicators have further interacting characteristics. \n Please select \n" 
    
    if(!is.null(extras()[[1]]$series) & extras()[[1]]$series[1] != ""){
      text <- paste0(text, " - Series \n")
    } 
    if(!is.null(extras()[[1]]$units) & extras()[[1]]$units[1] != ""){
      text <- paste0(text, " - Units \n")
    } 
    if(!is.null(extras()[[1]]$variable1) & extras()[[1]]$variable1[1] != ""){
      text <- paste0(text, "- A (", names(extras()[[2]])[[2]][3], ") \n")
    } 
    if(!is.null(extras()[[1]]$variable2) & extras()[[1]]$variable2[1] != ""){
      text <- paste0(text, "- B (", names(extras()[[2]])[[2]][4], ") \n")
    }
    if(!is.null(extras()[[1]]$variable3) & extras()[[1]]$variable3[1] != ""){
      text <- paste0(text, "- C (", names(extras()[[2]])[5], ") \n")
    }
    if(!is.null(extras()[[1]]$variable4) & extras()[[1]]$variable4[1] != ""){
      text <- paste0(text, "- D (", names(extras()[[2]])[6], ") \n")
    }
    text
    
  })
  
  
  # output$NA_as_all <- DT::renderDataTable(plot(), rownames = FALSE)
  # output$selections <- renderText(csv())
  output$plot <- renderPlot({
    input$plot_button
    isolate(plot())
    plot()
  })
  
  
  #-----------------------------------------------------------------------------
  
  csv <- reactive({
    req(input$Select_Indicator != "All")
    indicator_number <- gsub("\\.", "-", input$Select_Indicator)
    csv_filepath <- paste0("Y:\\Data Collection and Reporting\\Jemalex\\CSV\\indicator_", 
                           indicator_number, ".csv")
    csv <- read.csv(csv_filepath,
                    na.strings=c("","NA")) %>%
      mutate_if(is.factor, as.character) %>% 
      # there shouldn't be any blank values but sometimes they sneak in (e.g. 16-1-3 at time of writing)
      filter(!is.na(Value))
    
    names(csv) <- tolower(names(csv))
    
    #-----------------------------------------------------------------------------
    # replace NAs with 'All':
    #-----------------------------------------------------------------------------
    
    
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
        mutate(across(where(is.character), ~replace_na(.x, "All")))
      # following line hashed out as it didn't work for 16-7-1 for some reason
      # replace(is.na(.) & is.character(.), "All")
      
    } else {
      
      final_incompletely_nested_NAs_to_all <- NULL
      
      test <- data.frame("A" = c(NA, "B"),
                         "B" = c("C", NA)) %>% 
        mutate(A = as.character(A)) %>%
        replace(is.na(.) & is.character(.), "All")
      
      all_other_NAs_to_all <- all_variables %>%
        mutate(across(where(is.character), ~replace_na(.x, "All")))
      
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
    } else if("unit.measure" %in% names(dat_with_All)){
      extra_dropdowns <- c(extra_dropdowns, "unit.measure")
    }
    
    if(length(extra_dropdowns) > 0){
      
      dropdown_column_locations <- which(names(dat_with_All) %in% extra_dropdowns)
      
      # this shouldn't be necessary but units is coming up as an extra dropdown
      # for 11-7-2: that issue needs fixing, but doing this quick fix for now
      number_of_unique_entries <- length(unique(real_values_in_selected[, dropdown_column_locations]))
      
      if(number_of_unique_entries > 1){
        extra_dropdown_levels <- as.data.frame(real_values_in_selected[, dropdown_column_locations]) %>%
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
    
    list(filtered, extra_dropdowns)
  })
  
  extras <- reactive(
    observeEvent(input$Select_Indicator, {
    req(input$Select_Indicator != "All")
    
    filtered <- csv()[[1]]
    extra_dropdowns <- csv()[[2]]
    
    # make it so there are always 6 dropdown options (I am assuming it is 
    # unlikely there will ever be more than 6, but this may need to be increased
    # in the future)
    # length(extra_dropdowns) <- 6
    
    # options <- data.frame(matrix(ncol = length(extra_dropdowns), nrow = 0))
    # names(options) <- extra_dropdowns
    
    # get the options for the extra dropdowns, while retaining the original
    # data structure (keeps any nesting intact)
    options <- filtered %>% 
      select(all_of(extra_dropdowns)) %>% 
      distinct()
    
    # make sure units and series are always in extra dropdowns and give them a 
    # value if they don't already have one
    if("units" %not_in% names(filtered) & "unit.measure"%in% names(filtered)){
      options <- rename(options, units = unit.measure)
    }
    
    if("units" %not_in% names(options)){
      options$units <- ""
    }
    
    if("series" %not_in% names(options)){
      options$series <- ""
    } 
    
    # make series and units the first two dropdowns
    options <- options %>% 
      relocate(series, units)
    
    # # I don't know how to make the variable names the names of the dropdowns,
    # # so for now just have them at the top of the list
    # options <- rbind(paste0("characteristic: ", names(options)), options)
    
    # make sure there are always 6 columns - I am SURE there is a better way to do this!
    if(ncol(options) == 2){
      extra_columns <- data.frame("x3" = "",
                                  "x4" = "",
                                  "x5" = "",
                                  "x6" = "")  
    } else if(ncol(options) == 3){
      extra_columns <- data.frame("x4" = "",
                                  "x5" = "",
                                  "x6" = "")  
    } else if(ncol(options) == 4){
      extra_columns <- data.frame("x5" = "",
                                  "x6" = "")  
    } else if(ncol(options) == 5){
      extra_columns <- data.frame("x6" = "")  
    } else {extra_columns <- NULL}
    
    original_names <- bind_cols(options, extra_columns)
    
    options <- original_names
    
    names(options) <- c("series", "units",
                        "variable1", "variable2", "variable3", "variable4")
    list(options, original_names)
    
  }))
  
  plot <- eventReactive(input$plot_button, {
    req(input$Select_Indicator != "All")
    filtered <- csv()[[1]]
    extra_dropdowns <- extras()[[1]]
    
    series_selection <- input[["extra-filters-series"]]
    units_selection <- input[["extra-filters-units"]]
    extra1 <- input[["extra-filters-variable1"]]
    extra2 <- input[["extra-filters-variable2"]]
    extra3 <- input[["extra-filters-variable3"]]
    extra4 <- input[["extra-filters-variable4"]]
    
    # build a dataframe on which we can do the second step of filtering (using
    # any interacting variables that are not standard disaggs e.g. industy sector)
    extras <- c(extra1, extra2, extra3, extra4)
    number_of_extras <- length(extras[!is.null(extras)])
    
    if(number_of_extras > 0) { 
      
      further_selections <- data.frame(series = NA,
                                       units = NA)
      if(!is.null(series_selection)){
        further_selections <- mutate(further_selections,
                                     series = series_selection)
      } else {
        further_selections <- select(further_selections, -series)
      }
      if(!is.null(units_selection)){
        further_selections <- mutate(further_selections,
                                     units = units_selection)
      } else {
        further_selections <- select(further_selections, -units)
      }
      
      if(!is.null(extra1)){
        dropdown_variable <- sym(names(extra_dropdowns)[3])
        further_selections <- mutate(further_selections, !!dropdown_variable := extra1)
      }
      if(!is.null(extra2)){
        dropdown_variable <- sym(names(extra_dropdowns)[4])
        further_selections <- mutate(further_selections, !!dropdown_variable := extra2)
      }
      if(!is.null(extra3)){
        dropdown_variable <- sym(names(extra_dropdowns)[5])
        further_selections <- mutate(further_selections, !!dropdown_variable := extra3)
      }
      if(!is.null(extra4)){
        dropdown_variable <- sym(names(extra_dropdowns)[6])
        further_selections <- mutate(further_selections, !!dropdown_variable := extra4)
      }
      
      # earlier, I renamed unit.measure units in the extra_columns dataframe,
      # so we need to do that here too,
      # as it will still be called unit.measure in the filtered data
      if("units" %not_in% names(filtered) & "unit.measure" %in% names(filtered)){
        filtered <- rename(filtered, units = unit.measure)
      }
      
      for(selection in 1:ncol(further_selections)){
        variable <- sym(names(further_selections)[selection])
        level <- further_selections[1, selection]
        filtered <- filtered %>% 
          filter(!!variable == level)
      }
    }
    # plots don't work properly if year is not numeric (e.g. 2015/16),
    # so need to convert year to numeric if it contains a slash
    slash_present <- ifelse(sum(grepl("/", filtered$year) > 0), TRUE, FALSE)
    
    if(slash_present == TRUE) {
      filtered <- mutate(filtered, year = as.numeric(substr(year, 1, 4)))
    } 
    
    # function for calculating x axis breaks
    int_breaks <- function(x, n = 5){
      round_values <- pretty(x, n)
      round_values[abs(round_values %% 1) < .Machine$double.eps^0.5 ]
    }
    
    # need to make sure labels are correct, given the breaks
    if(slash_present == TRUE) {
      after_slash <- substr(int_breaks(filtered$year) + 1, 3, 4)
      year_labels <- paste0(int_breaks(filtered$year), "/", after_slash)
    } else {
      year_labels <- int_breaks(filtered$year)
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      scale_x_continuous(breaks = int_breaks, labels = year_labels) 
    
    
    plot
    
  })
  
  
  
}

shinyApp(ui, server)