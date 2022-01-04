# Control sheet

The control sheet (`control_sheet.csv`) contains the data behind the initial user selections.   
Once the app is in use, this will need to be rerun every time data on the platform are updated. To run:  
1. Open `Control_sheet.Rproj`
2. Run `control_sheet_simple.R`
3. The new control sheet overwrite the old control_sheet.csv
4. If necessary during app development merge the change into any active branches
 
### Main points
- The control sheet contains all indicators that have two or more interacting disaggregations (a.k.a variables, or characteristics). This means that only variables that interact with at least one other variable in at least one indicator will be available to be selected by the user.
- Only the key disaggregations are included (e.g. age, sex, geography, disability, ethnicity etc). These are mapped to the disaggregation names in each indicator in the file `Disaggregation_group_lookup.csv`
- If the user selects e.g. 'age' as the first characteristic, the second dropdown will only contain characteristics that interact with age in at least one indicator and so on.
- Interacting characteristics are given in every possible order, so that it doesn't matter which one the user selects first.
- The way the control sheet is set up allows users to choose to either select characteristic groups (e.g. Geography: low-level), using the 'variable_group..' columns; or specific characteristics (e.g. Local Authority/ Local Waste Authority/ County etc), using the 'variable..' columns.
- The code used to create the control sheet is `control_sheet_simple.R` in the `control_sheet.Rproj` project
- `control_sheet_simple.R` gets the indicator numbers and titles from the `indicators.csv` file. These were pulled from <https://unstats.un.org/sdgs/files/Tier%20Classification%20of%20SDG%20Indicators_28%20Dec%202020_web.xlsx>

NOTE: we will need to add to `Disaggregation_group_lookup.csv` when new disaggs that fall into a key disaggregation group are added to the platform  (e.g. Waste Disposal Authority is a low level geography but is only in 1 or 2 indicators).  

### Control sheet snapshot
  
![image](https://user-images.githubusercontent.com/52452377/120819852-e29b9c00-c54b-11eb-9d91-dae75ed095ca.png)
  
### Mock-up of potential selection layout
  
![image](https://user-images.githubusercontent.com/52452377/120816583-ca764d80-c548-11eb-91e9-45875f1b245b.png)

### A high level explanation of how `control_sheet_simple` works
1. The code loops through the indicator csv files.  
2. For each file disaggregation levels (e.g. 'Male') are replaced with the name of the column (in this case, 'Sex') and duplicated rows are dropped.  
3. Any disaggregations that are not key disaggregations found in `Disaggregation_group_lookup.csv` are dropped - At this point we can see what all the possible interactions are, but we need them in all possible permutations.  
4. All possible permutations of the disaggregations are calculated (e.g. for 'Sex, Age', it would return 'Sex, Age', 'Age, Sex', 'Sex, NA', 'Age, NA', 'NA, Sex', 'NA, Age'), and the control sheet is constructed based on this information.  
5. Unwanted rows are removed (e.g. we want to keep 'Sex, Age, NA' but not 'Sex, NA, Age').  
6. 'variable_group..' columns that correspond to the 'variable..' columns are added.  

### A bit of history
Initially, there was going to be the following selection setup:

![image](https://user-images.githubusercontent.com/52452377/120816875-15906080-c549-11eb-9f19-7a27aba04aa2.png)
  
The script now titled `control_sheet_including_levels.R` was the first script written for creation of the control sheet, and was written with the above mockup in mind. 
This script is incomplete, but may come in useful later particularly for filling in NAs with the apropiate word (Total/ England, etc).
  
While writing `control_sheet_including_levels.R` I realised that for the purposes of identifying and exploring interactions, the data team
would be focussing on the key disaggregations, not those that will only ever appear in one or two indictators. In addition, 
there is little requirement to select the levels at this stage, as they can be selected after the indicator. 
  
As inclusion of level selection (as shown in the mockup above) has the potential to cause speed issues, and would complicate 
the script this is no longer included, and a second script was written: 'control_sheet_simple.R'. This is the current code we should use.
  
`indicator_selection_flowchart.pptx` is out of date at time of writing


 
