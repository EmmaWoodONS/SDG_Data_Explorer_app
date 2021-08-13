## Filtering and plotting code
    
Once the indicator is selected by the user, the data needs to be filtered before it is plotted.
This is because if the dataset includes rows for disaggregations that are not accounted for in the ggplot code
ggplot will still try to plot them.

Until we have the input from the user, the filtering and plotting is run using fake inputs

Files:
- user_selections.R - where you enter user selections until they can be selected through the app
- config.R - currently just the names of irrelevant variables (these may change with SDMX work)
- control_script.R - where all other scripts are run from
- correctly_replace_NA_with_All - originally written for creating the control script, but reused here. Called by control_script.R
- filter_disaggregations.R - function to filter disaggs according to user selections. Does not filter series, units, or levels of disaggregations.
- filter_levels.R - not yet tested or in the control_script, but would be called by the control_script. Will require extra dropdowns to appear.
- plotting.R - not a function but run from the control_script. Plots the data regardless of the number of initially selected disaggregations (up to 4).  
  
Extra dropdowns will lead to a second round of user selections. These will be based on the extra_dropdowns output created by filter_disaggregations()
in the control_script. For testing purposes, there is a section of the control_script that allows selection of these prior to plotting.    
    
