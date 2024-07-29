cd "C:\Users\spike\Box\QAMO4800\Research Project"
use HousingPriceEventStudies.dta, clear

// Keep only Mountain West States
keep if statefip == 49 | statefip == 56 | statefip == 4 | statefip == 35 | ///
		statefip == 32 | statefip == 30 | statefip == 16 | statefip == 8

// 2000 - 2021
// AZ = 4 & 2002
// CO = 8 & 2019
// ID = 16 & 2019
// MT = 30 & 2005
// NV = 32 & 2019
// NM = 35 & 2009
// UT = 49 & 2008
// WY = 56 & 2014

// Drop unobserved rent variables
keep if rentgrs > 0

// Collapses on Mean values by year over each state
collapse (mean) rentgrs [aw = hhwt], by(year statefip)

// Create Variable for when each state was treated
gen _nfd = .
replace _nfd = 2019 if statefip == 8 | statefip == 16 | statefip == 32
replace _nfd = 2002 if statefip == 4
replace _nfd = 2005 if statefip == 30
replace _nfd = 2009 if statefip == 35
replace _nfd = 2008 if statefip == 49
replace _nfd = 2014 if statefip == 56

// Summary Graph
preserve
collapse (mean) rentgrs, by(year)
twoway line rentgrs year, xline(2002 2005 2008 2009 2014 2019, lwidth(.1) lstyle(foreground)) color(red) ///
xtitle(Year) ytitle(Average Rent Price Across Mountain West) ///
title(Rent Prices have been Increasing in the Mountain West)
restore

* create the lag/lead for treated states
* fill in control obs with 0
* This allows for the interaction between `treat` and `time_to_treat` to occur for each state.
* Otherwise, there may be some NAs and the estimations will be off.
g time_to_treat = year - _nfd
replace time_to_treat = -1 if missing(_nfd)
* this will determine the difference
* btw controls and treated states
g treat = !missing(_nfd)

* Stata won't allow factors with negative values, so let's shift
* time-to-treat to start at 0, keeping track of where the true -1 is
summ time_to_treat
g shifted_ttt = time_to_treat - r(min)
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* Regress on our interaction terms with FEs for group and year,
* clustering at the group (state) level
* use ib# to specify our reference group
reghdfe rentgrs ib`true_neg1'.shifted_ttt, a(statefip year) vce(cluster statefip)
// reg rentgrs ib`true_neg1'.shifted_ttt i.statefip i.year i.statefip#c.year, cluster(statefip)

* Now we can plot.
preserve

	// Keeps only parts where observations >= 4
	keep if inrange(time_to_treat,-8,12)==1

	* Pull out the coefficients and SEs
	g coef = .
	g se = .
	levelsof shifted_ttt, l(times)
	foreach t in `times' {
		replace coef = _b[`t'.shifted_ttt] if shifted_ttt == `t'
		replace se = _se[`t'.shifted_ttt] if shifted_ttt == `t'
	}

	* Make confidence intervals
	g ci_top = coef+1.96*se
	g ci_bottom = coef - 1.96*se

	* Limit ourselves to one observation per quarter
	* now switch back to time_to_treat to get original timing
	keep time_to_treat coef se ci_*
	duplicates drop

	sort time_to_treat

	* Create connected scatterplot of coefficients
	* with CIs included with rcap
	* and a line at 0 both horizontally and vertically
	summ ci_top
	local top_range = r(max)
	summ ci_bottom
	local bottom_range = r(min)

	twoway (sc coef time_to_treat, connect(line)) ///
		(rcap ci_top ci_bottom time_to_treat)	///
		(function y = 0, range(time_to_treat)) ///
		(function y = 0, range(`bottom_range' `top_range') horiz), ///
		xtitle("Time to Treatment") ///
		ytitle("Change in Rental Prices") ///
		xlabel(-8(4)12) ///
		title("Rent Prices in Mountain West Fall Post Housing Affordibility Policies") ///
		subtitle("Centered on Year before Policy Enactment") ///
		legend(label(1 "Coeff") label(2 "95% Confidence Interval") label(3 "X-Axis") label(4 "Y-Axis")  position(bottom) col(2))
		
restore

// *Balance over event times -5 through 20
// preserve
// 	keep if inrange(time_to_treat,-5,10)==1
// 	bys statefip: egen min_treat = min(time_to_treat)
// 	bys statefip: egen max_treat = max(time_to_treat)
// 	drop if min_treat > -5 | max_treat < 10
//	
// 	summ shifted_ttt if time_to_treat == -1
// 	local true_neg1 = r(mean)
// 	reg rentgrs ib`true_neg1'.shifted_ttt i.statefip i.year, cluster(statefip)
//
// 	* Now we can plot.
//
// 	* Pull out the coefficients and SEs
// 	g coef = .
// 	g se = .
// 	levelsof shifted_ttt, l(times)
// 	foreach t in `times' {
// 		replace coef = _b[`t'.shifted_ttt] if shifted_ttt == `t'
// 		replace se = _se[`t'.shifted_ttt] if shifted_ttt == `t'
// 	}
//
// 	* Make confidence intervals
// 	g ci_top = coef+1.96*se
// 	g ci_bottom = coef - 1.96*se
//
// 	* Limit ourselves to one observation per quarter
// 	* now switch back to time_to_treat to get original timing
// 	keep time_to_treat coef se ci_*
// 	duplicates drop
//
// 	sort time_to_treat
//
// 	* Create connected scatterplot of coefficients
// 	* with CIs included with rcap
// 	* and a line at 0 both horizontally and vertically
// 	summ ci_top
// 	local top_range = r(max)
// 	summ ci_bottom
// 	local bottom_range = r(min)
//
// 	twoway (sc coef time_to_treat, connect(line)) ///
// 		(rcap ci_top ci_bottom time_to_treat)	///
// 		(function y = 0, range(time_to_treat)) ///
// 		(function y = 0, range(`bottom_range' `top_range') horiz), ///
// 		xtitle("Time to Treatment") caption("95% Confidence Intervals Shown")
// restore
	
ssc install bacondecomp, replace
//
// *Apply the DD decomposition theorem in Goodman-Bacon (2018) to the two-way
// *    fixed effects DD model.

// keep if inrange(time_to_treat,-5,10)==1

// gen post = (time_to_treat >= 0)

// xtset statefip year
// bacondecomp rentgrs post, stub(Bacon_) robust ddetail
 
// bacondecomp rentgrs post, ddetail



*********LP-DID***********
*** LP-DiD ESTIMATES
drop treat
gen treat = inrange(time_to_treat,0,.)==1 //treatment is only on in post periods
cap drop *lpdid
cap drop D*y
gen b_lpdid = .
gen se_lpdid = .
local post_window 12
local pre_window 8
xtset statefip year


* Gen forward changes in outcome, to be used on the left side of the LP-DiD regressions	
forval j = 0/`post_window' {
	gen D`j'rentgrs = F`j'.rentgrs - L.rentgrs
}

forval j = 2/`pre_window' {
	gen Dm`j'rentgrs = L`j'.rentgrs - L.rentgrs
}

* Run LP-DiD regressions
forval j = 0/`post_window' {
	reghdfe D`j'rentgrs 	    							  ///
			D.treat    							  	 ///   /* treatment indicator */
	if 		D.treat==1 | F`j'.treat==0,				 ///   /* clean control condition */
			absorb(year) vce(cluster statefip)				   /* year indicators */
		
		replace b_lpdid = _b[D.treat] if time_to_treat ==`j'
		replace se_lpdid = _se[D.treat] if time_to_treat ==`j'
	
		if `j'>1 & `j'<=`pre_window' {
			reghdfe Dm`j'rentgrs  							///
					D.treat 							///   /* treatment indicator */
			if 		D.treat==1 | treat==0,				///   /* clean controls condition */
					absorb(year) vce(cluster statefip)			  /* year dummies */
			
			replace b_lpdid = _b[D.treat] if time_to_treat ==-`j'
			replace se_lpdid = _se[D.treat] if time_to_treat ==-`j'
		}
}

replace b_lpdid = 0 if time_to_treat ==-1 //set reference year to zero
replace se_lpdid = 0 if time_to_treat ==-1

gen max95_lpdid = b_lpdid + 1.96*se_lpdid //make 95% CI
gen min95_lpdid = b_lpdid - 1.96*se_lpdid

drop if time_to_treat > 12 | time_to_treat < -8

******************************
***  ESTIMATES GRAPH       ***
******************************
twoway (connected b_lpdid time_to_treat, sort) ///
	   (rcap max95_lpdid min95_lpdid time_to_treat), ///
	   xlabel(-8(4)12) ///
	   xtitle("Time to Treatment") ///
	   ytitle("Change in Rental Prices") ///
	   xline(0) yline(0) ///
	   title("Solving Negative Weighting Biases Removes Perceived Causality") ///
	   subtitle("Centered on Year before Policy Enactment") ///
	   legend(label(1 "Coeff") label(2 "95% Confidence Interval") label(3 "X-Axis") label(4 "Y-Axis")  position(bottom) col(2))
	
twoway (connected b_lpdid time_to_treat, sort color(green)) ///
	   (rcap max95_lpdid min95_lpdid time_to_treat, color(yellow)), ///
	   xlabel(-8(4)12) ///
	   ylabel(-150(50)100) /// 
	   xtitle("") legend(off) 