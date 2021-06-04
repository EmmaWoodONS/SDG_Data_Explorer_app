# Control sheet

The control sheet contains the data behind the initial user selections.  
Initially, there was going to be the following selection setup:

![image](https://user-images.githubusercontent.com/52452377/120800419-d3aaee80-c537-11eb-8bfa-55e6f75f0974.png)
  
The script now titled 'control_sheet_including_levels.R' was the first script written for creation of the control sheet. 
This script is incomplete, but may come in useful later particularly for filling in NAs with the apropiate word (Total/ England, etc).
  
While writing 'control_sheet_including_levels.R' I realised that for the purposes of identifying and exploring interactions, the data team
would be focussing on the key disaggregations, not those that will only ever appear in one or two indictators. In addition, 
there is little requirement to select the levels at this stage, as they can be selected after the indicator. 
  
As inclusion of level selection (as shown in the mockup above) has the potential to cause speed issues, and would complicate 
the script this is no longer included, and a second script was written: 'control_sheet_simple.R'. This is the current code we should use.

 
