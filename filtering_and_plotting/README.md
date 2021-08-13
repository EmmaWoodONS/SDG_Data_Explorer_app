
## Filtering and plotting code

### Introduction
    
Once the indicator is selected by the user, the data needs to be filtered before it is plotted.
This is because if the dataset includes rows for disaggregations that are not accounted for in the ggplot code
ggplot will still try to plot them. For example, imagine an indicator where the user wants to plot Country by Sex.
The data also includes Age but the user isn't interested in that. If we didn't filter the dataset to account for Age
we would get the following plot:    
    
![incorrect_plot](https://user-images.githubusercontent.com/52452377/129395602-958e86d8-18a4-40bd-809c-0e5692c9690a.jpeg)
We would expect 3 points for each year, with 3 lines between each year (one for each sex: M, F, and All).
There are, however, far more than 3 points for each year, and only 3 lines between each year. This is because there is a point 
for every age, for every sex category for each year.    

Once we run the filter_disaggregations code, and thereby account for Age, we get the correct plot:    
    
![correct_plot](https://user-images.githubusercontent.com/52452377/129395982-02a4c5c2-4066-454f-9754-4105a5f54e5c.jpeg)



### Current code
Until we have the input from the user, the filtering and plotting is run using fake inputs.    
    
To run the code:
1) In R, open user_selections.R and edit them for the indicator and variables you want to plot. Note
that the variables you select must be from a single row in the control_sheet csv. Make sure that the 
chart disaggregations (eg `facet_row`) contain all and only the disaggregations entered as `char` variables.
2) open control_script.R and change the working directory (setwd) to your local coding repo.
3) run control_script up to `extra_dropdowns <- filtered_data_and_extra_dropdowns[[2]]`
4) Look at the extra_dropdowns dataframe and choos which row of selections you want
5) change `extra_dropdown_row` to that row number
6) run the rest of the script. The plot will appear in the plot pane

Files:
- user_selections.R - where you enter user selections until they can be selected through the app
- config.R - currently just the names of irrelevant variables (these may change with SDMX work)
- control_script.R - where all other scripts are run from
- correctly_replace_NA_with_All - originally written for creating the control script, but reused here. Called by control_script.R
- filter_disaggregations.R - function to filter disaggs according to user selections. Does not filter series, units, or levels of disaggregations.
- filter_levels.R - not yet tested or in the control_script, but would be called by the control_script. Will require extra dropdowns to appear.
- plotting.R - not a function but run from the control_script. Plots the data regardless of the number of initially selected disaggregations (up to 4).  
  
Extra dropdowns will lead to a second round of user selections. These will be based on the extra_dropdowns output created by filter_disaggregations()
in the control_script. For testing purposes, there is a section of the control_script that allows selection of these prior to plotting, which may
form the basis of how we do it in the app.    
  
