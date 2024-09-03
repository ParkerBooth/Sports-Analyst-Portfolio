**************************************************************
// Market Research Project 2
// By: Parker Booth, Adam Martinez, Daniel Paulson-Luna
**************************************************************

// Setting up Workspace
clear all
cap log close
cd "FilePath"
log using "MarketResearch2.smcl", replace

numlabel, add

// Merging Data sets
import delimited state_cigarette_rates_6.csv
save state_cigarette_rates_6.dta, replace
use state_cigarette_rates_6.dta
drop v4-v9
statastates, name(state) nogenerate
rename state_fips statefip
merge 1:m statefip using finalCPSdata

// Setting up parameters and Dummy Variables
keep if year == 2019
keep if age >= 21
keep if tcigday <= 40

gen activeSmoker = .
replace activeSmoker = 0 if tamsmk == 1
replace activeSmoker = 1 if tamsmk == 2 | tamsmk == 3

gen youngKids = (nchlt5 >= 1)
gen bachelors = (educ >= 111)
gen employed = (empstat == 10 | empstat == 12)
gen northEast = (region == 11 | region == 12)
gen female = (sex == 2)
gen white = (race == 100)
gen veteran = (vetstat == 2)
gen married = (marst == 1 | marst == 2)


// Regression

reg tcigday taxrate youngKids age female white married [w=wtfinl], robust
outreg2 using MarketResearch2, excel replace ///
ctitle("OLS") addt(Robust, Yes)

reg tcigday taxrate youngKids age female white married northEast [w=wtfinl], robust
outreg2 using MarketResearch2, excel append ///
ctitle("OLS") addt(Robust, Yes)

reg tcigday taxrate youngKids age female white married activeSmoker [w=wtfinl], robust
outreg2 using MarketResearch2, excel append ///
ctitle("OLS") addt(Robust, Yes)


// Graphs

twoway scatter tcigday taxrate || lfit tcigday taxrate, legend(off) ///
lcolor(red) ///
xtitle("Excise Tax") ///
ytitle("Cigarettes Smoked per Day") ///
title("Cigarettes Smoked per Day vs Excise Tax")
graph export "CigarettesSmokedvsExciseTax.png", as(png) replace

graph bar (mean) taxrate, over(northEast, relabel(1 "Other Regions" 2 "North East")) ///
ytitle("Mean Excise Tax") ///
title("Other Regions Excise Tax vs North East Excise Tax")
graph export "ExciseTaxRegional.png", as(png) replace

