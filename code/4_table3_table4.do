
********************************************
// 4_table3_table4: Multiple Group Test
// Author: Marshall A. Taylor
********************************************

version 16.0
clear all
macro drop _all
set more off  


**************************
***COMMANDS BEGIN HERE ***
**************************

//Load in data
import delimited "data/sem_data.csv", clear


//SEMs, Table 3
sem white_pole man_pole good_pole influential_pole young_pole, ///
	group(group)
est store DIFF

sem ( <- white_pole man_pole good_pole influential_pole young_pole), ///
	group(group) ginvariant(covex)
est store SAME

est stats DIFF SAME
lrtest SAME DIFF

//SEMs, Table 4
sem white_pole man_pole good_pole influential_pole young_pole, ///
	group(group2)
est store DIFF2

sem white_pole man_pole good_pole influential_pole young_pole, ///
	group(group3)
est store DIFF3

sem white_pole man_pole good_pole influential_pole young_pole, ///
	group(group4)
est store DIFF4

est stats DIFF DIFF2 DIFF3 DIFF4

**************************
*** COMMANDS END HERE  ***
**************************
log close
exit
