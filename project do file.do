*******************************************
// Market Research Project 1
// Written By: Group 5
*******************************************
//Cali 06 fip  Texas 48 fip

clear all
cap log close

cd "C:\Users\spike\OneDrive\Documents\Qamo4700\Research Project 1"
log using "Research_Project_1.smcl", replace
use cps_project1.dta, clear

ssc install outreg2

// Age Distribution Histogram
histogram age if statefip == 6 | statefip == 48, fcolor(%75) percent xline(54) xline(20) title(Age Distribution in Cali/Texas)
graph export totalAgeDistribution.png, as(png) replace


keep if age >=20 &age <=54
// Prime age adults are those who would be taking a course like this
keep if edu >=111 & edu<500
// Edu of 111 is a bachelors degree. I would consider having a bachelors to be highly educated.
drop if pcrwrt >= 96 | pphoto >= 96 | ppaint >= 96 | pweave >= 96
// Drops all values of individuals who did not answer if they were creative in the survey

gen creative = (pcrwrt == 2 | ppaint == 2 | pphoto == 2 | pweave == 2)

gen white = (race == 100)

gen female = .
replace female = 0 if sex == 1
replace female = 1 if sex == 2

gen texas = .
replace texas = 0 if statefip == 6
replace texas = 1 if statefip == 48

gen laborforce = .
replace laborforce = 1 if labforce == 2
replace laborforce = 0 if labforce == 1

*****************!!!FINAL REGRESSION!!!*******************       
   
regress creative white female nchild laborforce texas [pw=wtfinl], robust
outreg2 using Research_Project_1, excel replace ///
ctitle("Final Regression") addt(Robust, Yes)

*****************!!!FINAL REGRESSION!!!*******************  




*******************************
// Material not in final regression but part of research
*******************************

gen married = (marst == 1 | marst == 2)

regress creative white female nchild laborforce texas married [pw=wtfinl], robust
outreg2 using Research_Project_1, excel append ///
ctitle("Regression w/ Married") addt(Robust, Yes)


gen fulltime = .
replace fulltime = 1 if wkstat >= 10 & wkstat <= 15
replace fulltime = 0 if wkstat >= 20 & wkstat <= 42

regress creative white female nchild laborforce texas fulltime [pw=wtfinl], robust
outreg2 using Research_Project_1, excel append ///
ctitle("Regression w/ Fulltime") addt(Robust, Yes)


gen youngchildren = (nchlt5 >= 1)

regress creative white female nchild laborforce texas youngchildren [pw=wtfinl], robust
outreg2 using Research_Project_1, excel append ///
ctitle("Regression w/ Young Children") addt(Robust, Yes)


gen unemp = (wkstat == 50 | wkstat == 60)

regress creative white female nchild laborforce texas unemp [pw=wtfinl], robust
outreg2 using Research_Project_1, excel append ///
ctitle("Regression w/ Unemployment") addt(Robust, Yes)


gen black = (race == 200)
gen asian = (race == 650 | race == 651)
gen other = (race > 651)
// Used for Pie Charts

// Pie Chart on Race Distribution
graph pie white black asian other if statefip == 6 | statefip == 48, title(Racial Distribution in Cali/Texas) legend(on) plabel(1 percent, size(Large)) 
graph export raceDistribution.png, as(png) replace



