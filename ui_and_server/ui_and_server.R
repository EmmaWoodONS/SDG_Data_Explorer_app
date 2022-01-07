library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyr)
library(ggplot2)
library(shinycssloaders)
library(reactlog)

# data("mpg", package = "ggplot2")

ui <- fluidPage(
  
  tags$img(src = "SDG_logo.png",
           align = "left",
           height = 100, width = 200),
  
  fluidRow(
    
    column(
      width = 10, offset = 1,
      tags$h2("UK Indicator Explorer"),
      HTML("<p>This app allows you to explore indicators with cross-disaggregations. 
              For example, if you want to know which indicators allow you to compare 
              males of a certain age with females of the same age, you might select
              age in <b>characteristic 1</b>, and sex in <b>characteristic 2</b>.
              You can then plot and save the results for each indicator.</p><p>
              </p><p>Characteristics can be searched using the exact names, e.g.
              'Local Authority' (<b>actual characteristics</b>), or using grouped names e.g. 
              'Lower level geography' (<b>grouped characteristics</b>).</p><p>All data 
              come from the <a href='https://sdgdata.gov.uk/'>UK SDG data site</a>, 
              where you will find important notes on the data and a link to the data source.</p>"),
      
      panel(
        radioButtons("group_type", "Choose indicators by:",
                     choices = list(
                       "actual characteristics",
                       "grouped characteristics")),

        selectizeGroupUI(
          id = "my-filters",
          params = list(
            variable1 = list(inputId = "var1", title = "characteristic 1 (panel column):"),
            variable2 = list(inputId = "var2", title = "characteristic 2 (colour):"),
            variable3 = list(inputId = "var3", title = "characteristic 3 (panel row):"),
            variable4 = list(inputId = "var4", title = "characteristic 4 (linetype):"))
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
        
        uiOutput("Num Indicators"),
        
        status = "primary"),
      
      conditionalPanel(
        condition = "input.Select_Indicator != 'All'",

        verbatimTextOutput("extra_variables")
      ),
      
      conditionalPanel(
        condition = "input.Select_Indicator != 'All'",
        
        # actionButton("continue", "continue"),
        
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
        
        actionBttn("plot_button", "Plot")),
      
      conditionalPanel(
        condition = "input.Select_Indicator != 'All'",
        # tableOutput("plot_table")
        # verbatimTextOutput("plot_text")
        plotOutput("plot",
                   height = "600px") %>%
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

  # because callModule isn't really meant to be in a reactive, when this function is called
  # it is referred to as res_mod()(), rather than res_mod()
  res_mod <- reactive({
    
    if(input$group_type == "grouped characteristics") {
      control_sheet <- control_sheet %>%
        mutate(var1 = variable_group_1,
               var2 = variable_group_2,
               var3 = variable_group_3,
               var4 = variable_group_4) 
    } else {
      control_sheet <- control_sheet %>%
        mutate(var1 = variable1,
               var2 = variable2,
               var3 = variable3,
               var4 = variable4) 
    }
    
    callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      data = control_sheet,
      vars = c("var1", "var2", "var3", "var4")
    )
  })
  

  selections <- reactive({
    req(input[["my-filters-var1"]])
    res_mod()() %>% 
      select(indicator_title, variable1, variable2, variable3, variable4)
  })
  
  output$`Num Indicators` <- renderText({
    
    req(input[["my-filters-var2"]])
    if(is.null(input[["my-filters-var3"]])){
      paste0("There are ", length(unique(res_mod()()$Indicator)), " indicators disaggregated by ", input[["my-filters-var1"]], " and ", input[["my-filters-var2"]])
    } else if(is.null(input[["my-filters-var4"]])) {
      paste0("There are ", length(unique(res_mod()()$Indicator)), " indicators disaggregated by ", input[["my-filters-var1"]], ", ", input[["my-filters-var2"]], " and ",  input[["my-filters-var3"]])
    } else {
      paste0("There are ", length(unique(res_mod()()$Indicator)), " indicators disaggregated by ", input[["my-filters-var1"]], ", ", input[["my-filters-var2"]], ", ",  input[["my-filters-var3"]], " and ", input[["my-filters-var4"]] )
    }
  })
  
  output$`Select Indicator` <- renderUI({
    selectInput("Select_Indicator", "Select Indicator", choices = c("All", unique(res_mod()()$Indicator)))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(res_mod()(), con, row.names = FALSE)
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
  
  # output$table <- DT::renderDataTable(res_mod(), rownames = FALSE)
  output$table <- DT::renderDataTable(selections(), rownames = FALSE)

  # output$plot_table <- renderTable(
  #   plot())
  # output$plot_text <- renderText(
  #   plot())
  
  # extra_disaggs <- callModule(
  #   module = selectizeGroupServer,
  #   id = "extra-filters",
  #   data = reactive(extras()[[1]]), # Initially thought this should be `data = extras(),
  #   # but then the options didn't update when the indicator was changed. 
  #   vars = c("series", "units",
  #            "variable1", "variable2", "variable3", "variable4")) 
  extra_disaggs <- reactive({
    callModule(
      module = selectizeGroupServer,
      id = "extra-filters",
      data = reactive(extras()[[1]]), # Initially thought this should be `data = extras(),
      # but then the options didn't update when the indicator was changed. 
      vars = c("series", "units",
               "variable1", "variable2", "variable3", "variable4")) 
  })
  
  output$extra_variables <- renderText({
    req(input$Select_Indicator != "All")
    
    text <- "Some indicators have further interacting characteristics. \n Please select \n" 
    
    # extras <- extras()[[1]]
    extras <- extra_disaggs()()
    # remove blanks so we can find the number of real options in each selection 
    # just by using length
    series_not_null <- extras$series[!is.null(extras$series) & extras$series != ""]
    units_not_null <- extras$units[!is.null(extras$units) & extras$units != ""]
    var1_not_null <- extras$variable1[!is.null(extras$variable1) & extras$variable1 != ""]
    var2_not_null <- extras$variable2[!is.null(extras$variable2) & extras$variable2 != ""]
    var3_not_null <- extras$variable3[!is.null(extras$variable3) & extras$variable3 != ""]
    var4_not_null <- extras$variable4[!is.null(extras$variable4) & extras$variable4 != ""]
    
    real_names <- extras()[[2]]
    
    if(length(series_not_null) > 0){
      text <- paste0(text, " - Series \n")
    } 
    if(length(units_not_null) > 0){
      text <- paste0(text, " - Units \n")
    } 
    if(length(var1_not_null) > 0){
      text <- paste0(text, "- A (", names(real_names)[3], ") \n")
    } 
    if(length(var2_not_null) > 0){
      text <- paste0(text, "- B (", names(real_names)[4], ") \n")
    }
    if(length(var3_not_null) > 0){
      text <- paste0(text, "- C (", names(real_names)[5], ") \n")
    }
    if(length(var4_not_null) > 0){
      text <- paste0(text, "- D (", names(real_names)[6], ") \n")
    }
    text
    
  })
  
  
  
  # output$extra_disaggs <- DT::renderDataTable(extra_disaggs(), rownames = FALSE)

  
  #-----------------------------------------------------------------------------
  
  csv <- reactive({
    req(input$Select_Indicator != "All")
    indicator_number <- gsub("\\.", "-", input$Select_Indicator)
    
    csv_filepath <- paste0('https://raw.githubusercontent.com/ONSdigital/sdg-data/master/data/indicator_',
                           indicator_number, '.csv')
    # csv_filepath <- paste0("Y:\\Data Collection and Reporting\\Jemalex\\CSV\\indicator_", 
    #                        indicator_number, ".csv")
    csv <- read.csv(csv_filepath,
                    na.strings=c("","NA")) %>%
      mutate_if(is.factor, as.character) %>% 
      # there shouldn't be any blank values but sometimes they sneak in (e.g. 16-1-3 at time of writing)
      filter(!is.na(Value))
    
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
    selections <- c(input[["my-filters-var1"]], input[["my-filters-var2"]], 
                    input[["my-filters-var3"]], input[[ "my-filters-var4"]]) 
    selections <- selections[!is.na(selections)]
    
    # because of the way the data are read in, spaces in csv column headings 
    # have been converted to '.'
    selections <- gsub(" ", ".", selections)  
    
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
      
      if(number_of_unique_entries > 0){
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
    
    # filtered currently contains rows from un-selected columns that we don't want
    # in the final data, so this right_join gets rid of them.
    if(is.data.frame(unselected_var_level)){
      filtered <- filtered %>%
        right_join(unselected_var_level, by =
                     c(names(unselected_var_level)))
    }
    
    list(filtered, extra_dropdowns)
  })
  
  
  extras <- reactive({
    req(input$Select_Indicator != "All")
    
    filtered <- csv()[[1]] # 
    
    extra_dropdowns <- csv()[[2]] # extra_dropdowns <- csv[[2]]
    
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
    
    options <- bind_cols(options, extra_columns)
    options_real_names <- options
    
    names(options) <- c("series", "units",
                        "variable1", "variable2", "variable3", "variable4")
    
    list(options, options_real_names)
  })
  # if running outside of app: extras <- list(options, options_real_names)
  
  plot <- reactive({
    req(input$Select_Indicator != "All")
    filtered <- csv()[[1]] # filtered <- csv[[1]]
    # get the extra dropdowns from extra_disaggs rather than from extras()[[1]]
    # so that it is reactive
    extra_dropdowns <- extra_disaggs()()# extra_dropdowns <- extras[[1]]
    real_names <- extras()[[2]] # real_names <- extras[[2]]

    series_selection <- input[["extra-filters-series"]]
    units_selection <- input[["extra-filters-units"]]
    extra1 <- input[["extra-filters-variable1"]]
    extra2 <- input[["extra-filters-variable2"]]
    extra3 <- input[["extra-filters-variable3"]]
    extra4 <- input[["extra-filters-variable4"]]

    # if running outside of app (assumes the first option is selected):
    # series_selection <- extra_dropdowns[1,"series"]
    # units_selection <-  extra_dropdowns[1,"units"]
    # extra1 <- extra_dropdowns[1,"variable1"]
    # extra2 <- extra_dropdowns[1,"variable2"]
    # extra3 <- extra_dropdowns[1,"variable3"]
    # extra4 <- extra_dropdowns[1,"variable4"]

    extra_selections <- extra_dropdowns

    if(length(series_selection == 1)) {
      extra_selections <- filter(extra_selections, series == series_selection)
    }
    if(length(units_selection) == 1) {
      extra_selections <- filter(extra_selections, units == units_selection)
    }
    if(length(extra1) == 1) {
      extra_selections <- filter(extra_selections, variable1 == extra1)
    }
    if(length(extra2) == 1) {
      extra_selections <- filter(extra_selections, variable2 == extra2)
    }
    if(length(extra3) == 1) {
      extra_selections <- filter(extra_selections, variable3 == extra3)
    }
    if(length(extra4) == 1) {
      extra_selections <- filter(extra_selections, variable4 == extra4)
    }

    # in order to join to the data and thus filter it, we need to make the
    # column names match those in the data
    names(extra_selections) <- names(real_names)

    # we don't want to join to the main data by series/units, if the
    # selection for them is blank. This isn't a problem for the others
    # as they will only get a column name if they are selected
    # this section causes the error attempt to set an attribute on NULL
    if(length(series_selection) == 0) {
      extra_selections <- select(extra_selections, -series)
    }
    if(length(units_selection) == 0) {
      extra_selections <- select(extra_selections, -units)
    }
    # end of section that causes error: attempt to set an attribute on NULL
    

    # earlier, I renamed unit.measure units in the extra_columns dataframe,
    # so we need to do that here too,
    # as it will still be called unit.measure in the filtered data
    if("units" %not_in% names(filtered) & "unit.measure" %in% names(filtered)){
      filtered <- rename(filtered, units = unit.measure)
    }

    filtered <- filtered %>%
      right_join(extra_selections)
  
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
    facet_column <- input[["my-filters-var1"]]
    line_colour <- input[["my-filters-var2"]]
    facet_row <- input[["my-filters-var3"]]
    line_style <- input[["my-filters-var4"]]

    # # for running outside of app:
    # facet_column <- "highest qualification"
    # line_colour <- "sex"
    # facet_row <- NULL
    # line_style <- NULL
    
    facet_column <- if(!is.null(facet_column)) {gsub(" ", ".", facet_column)}
    line_colour <- if(!is.null(line_colour)) {gsub(" ", ".", line_colour)}
    facet_row <- if(!is.null(facet_row)) {gsub(" ", ".", facet_row)}
    line_style <- if(!is.null(line_style)) {gsub(" ", ".", line_style)}
    
    plot_options <- c(facet_column, facet_row, line_colour, line_style)
    number_of_selections <- length(plot_options[!is.null(plot_options)])

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



    title <- unique(control_sheet$indicator_title[control_sheet$Indicator == input$Select_Indicator])
    # title <- unique(control_sheet$indicator_title[control_sheet$Indicator == "16.7.1"])

    int_breaks <- function(x,n=5){
      l <- pretty(x,n)
      l[abs(l %% 1) < .Machine$double.eps^0.5 ]
    }


    plot <- plot +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      scale_x_continuous(breaks = int_breaks) +
      ggtitle(title)


    plot
    
  })
  
  # # reset selections for extra disaggregations:
  # updateSelectizeInput(session, "inSelect",
  #                   label = paste("Select input label", length(x)),
  #                   choices = x,
  #                   selected = tail(x, 1)
  # )
  
  
  # output$NA_as_all <- DT::renderDataTable(plot(), rownames = FALSE)
  # output$selections <- renderText(csv())
  
  output$plot <- renderPlot({
    input$plot_button
    isolate(plot())
  })
  
}



shinyApp(ui, server)