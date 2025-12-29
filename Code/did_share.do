cd "C:\Users\RMachadoParente\OneDrive - International Monetary Fund (PRD)\Personal\MinWageIneqInf\PNAD"

*///////////////////////////////////////////////////////////////////////////////
* Generate sample
*///////////////////////////////////////////////////////////////////////////////

* use main data
use data/PNAD_1996_2015.dta, clear

*
* sample
*

* keep years of analysis
keep if year<=2012

*
* demographics
*

* population counter
gen pop=1

* age, race and education composition
gen young=(age<30)*employed
gen white=(race==2)*employed
gen highskill=((educ==3)+(educ==4))*employed
replace female=female*employed

*
* employment
*

* generate earning variables MISSING IF NOT IN WORKER SAMPLE
gen earn=earnings if formal | informal
gen earn_form=earnings if formal
gen earn_inf=earnings if informal
gen learn=log(earnings) if formal | informal
gen learn_form=learn if formal
gen learn_inf=learn if informal

* generate formal workers at minimum wage
gen atminw=formal*inrange(earnings,0.975*minw,1.025*minw)

*
* generate data set at state-year
*

* collapse data at state year level
collapse (sum) pop young white highskill female ///
			(sum) active occupied formal informal atminw ///
			(sd) v_all=learn v_form=learn_form v_inf=learn_inf ///
			(mean) mean_all=learn mean_form=learn_form mean_inf=learn_inf ///
			(p50) p50_form=earn_form [aw=weight], by(year state minw)

* adjust variance of log earnings
foreach var in v_all v_form v_inf {
replace `var'=`var'^2
}

* generate population shares
gen frac_young=young/(formal+informal) // fraction young
gen frac_white=white/(formal+informal)  // white
gen frac_highskill=highskill/(formal+informal)  // high skill
gen frac_female=female/(formal+informal)  // female

* generate employment variables
gen empl=formal+informal // total employment within sample
gen unempl=(active-occupied)/active // unemployment
gen frac_inf=informal/empl // informal fraction
gen frac_minw=atminw/formal // fraction at minimum wage
gen kaitz=log(minw/p50_form) // kaitz index
gen ratio_inf=informal/formal // relative size of informal sector

* generate within and between components of overall inequality
gen within=(1-frac_inf)*v_form+frac_inf*v_inf
gen between=(1-frac_inf)*(mean_form-mean_all)^2+frac_inf*(mean_inf-mean_all)^2
gen frac_within=within/(within+between)
gen frac_between=between/(within+between)

* generate post 1999 dummy
gen post=(year>1999)

* generate treatment
local measure frac_minw // select measure
bys state: egen bind1999 = mean(`measure') if year <= 1999 // calculate mean in pre-period
egen quintile = xtile(bind1999) if year <= 1999, nq(9) // split states in groups of 3
bys state: egen treated = max(quintile) // replace missing observations

* calculate average unemployment in pre-period
bys state: egen aux = mean(unempl) if year<=1999
bys state: egen pre_unempl = max(aux)

* save main regression data set
save data/regdata.dta, replace

*///////////////////////////////////////////////////////////////////////////////
* RELATIVE EVOLUTION OF OUTCOMES STUDY ANALYSIS
*///////////////////////////////////////////////////////////////////////////////

* use regression data
use data/regdata.dta, clear

* plot evolution of outcomes for groups 1 and 9, 1996 = 1
collapse (mean) v_all v_form v_inf frac_inf [aw=empl], by(treated year) // collapse by treatment group
foreach var in v_all v_inf v_form frac_inf { // normalize variables for each treatment
bys treated: gen aux=`var'[1]
gen n_`var'=`var'/aux
drop aux
}
bys treated: gen netinf=frac_inf-frac_inf[4]
sort treated year
				
* 
* FIGURE 4
*
graph twoway (line n_frac_inf year if treated==9, color(black) lpat(solid) lwidth(medthick)) ///
				(line n_frac_inf year if treated==1, color(gs12) lpat(solid) lwidth(medthick)), ///
				legend(order(1 "Most exposed" 2 "Least exposed") row(1) ring(0) pos(7)) ///
				ytitle("Informal share of labor (1996=1)") xtitle("") ///
				xlabel(1996(2)2012) ///
				graphregion(color(white)) name(raw_n_frac, replace)
				
*
* FIGURE 3 
*			
graph twoway (line n_v_all year if treated==9, color(black) lpat(solid)) ///
				(line n_v_form year if treated==9, color(black) lpat(longdash)) ///
				(line n_v_inf year if treated==9, color(black) lpat(shortdash)), ///
				legend(order(1 "Aggregate" 2 "Formal" 3 "Informal") row(3) ring(0) position(8)) ///
				ytitle("Variance of log earnings (1996=1)") xtitle("") ///
				xlabel(1996(2)2012) ///
				title("3 most binding states") ///
				ylabel(0.4(0.2)1.4) ///
				graphregion(color(white)) name(raw_v_restr, replace)			
graph twoway (line n_v_all year if treated==1, color(black) lpat(solid)) ///
				(line n_v_form year if treated==1, color(black) lpat(longdash)) ///
				(line n_v_inf year if treated==1, color(black) lpat(shortdash)), ///
				legend(order(1 "Aggregate" 2 "Formal" 3 "Informal") row(3) ring(0) position(8)) ///
				ytitle("Variance of log earnings (1996=1)") xtitle("") ///
				xlabel(1996(2)2012) ///
				title("3 least binding states") ///
				ylabel(0.4(0.2)1.4) ///
				graphregion(color(white)) name(raw_v_unrestr, replace)

*///////////////////////////////////////////////////////////////////////////////
* EVENT STUDY ANALYSIS (GROUP 9 VS GROUP 1)
*///////////////////////////////////////////////////////////////////////////////

* use regression data
use data/regdata.dta, clear

* take logs of regression variables
foreach var in v_all v_form v_inf frac_inf between {
replace `var'=log(`var')
}

* main DiD regressions
eststo clear
foreach outcome in v_all v_form v_inf frac_inf between {

* run DiD regressions
local controls frac_highskill frac_young frac_white frac_female unempl // set controls
eststo: reghdfe `outcome' ib1.treat##c.post `controls', abs(state year) vce(cluster state) // post interaction -> mean effect
qui reghdfe `outcome' ib1.treat##ib1999.year `controls', abs(state year) vce(cluster state) // event study specification
qui parmest, label list(parm label estimate min* max* p) saving("data/`outcome'.dta", replace) // save parameters

}

*
* TABLE 2
*
esttab using "figures\TableDiD.tex", ///
               se label replace b(3) star(* 0.10 ** 0.05 *** 0.01) staraux nonotes nogaps nodepvars r2 nomtitles

*
* plot event studies 9 versus 1
* 

* input data
use "data/v_all.dta", clear 
gen regression=1
append using "data/v_form.dta"
replace regression=2 if regression==.
append using "data/v_inf.dta"
replace regression=3 if regression==.
append using "data/frac_inf.dta"
replace regression=4 if regression==.
append using "data/between.dta"
replace regression=5 if regression==.

* organize data set
keep if inrange(_n,40,159) | inrange(_n,205,324) | inrange(_n,370,489) | ///
		inrange(_n,535,654) | inrange(_n,700,819)
g group = substr(parm, 1, 1)
destring group, replace
bys regr group: g time = _n
replace time = time + 1995
replace time = time + 1 if time>1999
replace time = time + 1 if time>2009
keep estimate min95 max95 time group regression
reshape wide estimate min95 max95, i(time regression) j(group)
sort time

* set colors
local c1 "black"
local c2 "cranberry" 
local c3 "midblue" 
local c4 "orange"
local c5 "midgreen"

* plot measures of variance
gen time1=time-0.2
gen time2=time
gen time3=time+0.2

*
* FIGURE 5
*
twoway 	(rcap min959 max959 time1 if regression==2, lc(cranberry) lw(medthin)) ///
		(scatter estimate9 time1 if regression==2, msize(medlarge) m(circle) mlw(medium) mlc(cranberry) mfc(white)) ///
		(rcap min959 max959 time2 if regression==3, lc(midblue) lw(medthin)) ///
		(scatter estimate9 time2 if regression==3, msize(medlarge) m(X) mlw(large) mlc(midblue) mfc(white)) ///
		(rcap min959 max959 time3 if regression==1, lc(black) lw(medthin)) ///
		(scatter estimate9 time3 if regression==1, msize(medlarge) m(triangle) mlw(large) mlc(black) mfc(white)), ///
		ylabel(-0.6(0.2)0.6,labsize(small)) ///
		legend(order(2 "Formal ({&beta}=-.253***)" 4 "Informal ({&beta}=.316***)" 6 "Overall ({&beta}=.200**)") row(1) 	ring(0) pos(6)) ///
		xtitle("") xlabel(1996(2)2012) yline(0, lpattern(dash) lcolor(gs12)) graphregion(fcolor(white))	name(did_v,replace)
twoway 	(rcap min959 max959 time2 if regression==4, lc(midblue) lw(medthin)) ///
		(scatter estimate9 time2 if regression==4, msize(medlarge) m(X) mlw(medthick) mlc(midblue) mfc(white)), ///
		ylabel(-0.6(0.2)0.6,labsize(small)) ///
		legend(order(2 "Fraction informal ({&beta}=.073**)") row(1) ring(0) pos(6)) ///
		xtitle("") xlabel(1996(2)2012) yline(0, lpattern(dash) lcolor(gs12)) graphregion(fcolor(white))	name(did_inf,replace)		

*///////////////////////////////////////////////////////////////////////////////
* EVENT STUDY ANALYSIS (ALL GROUPS)
*///////////////////////////////////////////////////////////////////////////////

* use regression data
use data/regdata.dta, clear

* take logs of regression variables
foreach var in v_all v_form v_inf frac_inf between {
replace `var'=log(`var')
}

* main DiD regressions
foreach outcome in v_all v_form v_inf frac_inf {
* run DiD regressions
local controls frac_highskill frac_young frac_white frac_female unempl // set controls
reghdfe `outcome' ib1.treat##c.post `controls', abs(state year) vce(cluster state) // post interaction -> mean effect
qui parmest, label list(parm label estimate min* max* p) saving("data/`outcome'.dta", replace) // save parameters
}

*
* plot event studies (all groups)
*

* input data
use "data/v_all.dta", clear 
gen regression=1
append using "data/v_form.dta"
replace regression=2 if regression==.
append using "data/v_inf.dta"
replace regression=3 if regression==.
append using "data/frac_inf.dta"
replace regression=4 if regression==.

* organize data set
keep if inrange(_n,12,19) | inrange(_n,37,44) | inrange(_n,62,69) | ///
		inrange(_n,87,94)
g group = substr(parm, 1, 1)
destring group, replace
keep estimate min95 max95 group regression
gen g1=group-0.2
gen g2=group
gen g3=group+0.2

* set colors
local c1 "black"
local c2 "cranberry" 
local c3 "midblue" 
local c4 "orange"
local c5 "midgreen"

*
* FIGURE 6
*
* plot measures of variance
twoway 	(rcap min95 max95 g2 if regression==2, lc(cranberry) lw(medthin)) ///
		(scatter estimate g2 if regression==2, msize(medlarge) m(circle) mlw(medium) mlc(cranberry) mfc(white)) ///
		(rcap min95 max95 g3 if regression==3, lc(midblue) lw(medthin)) ///
		(scatter estimate g3 if regression==3, msize(medlarge) m(X) mlw(medium) mlc(midblue) mfc(white)) ///
		(rcap min95 max95 g1 if regression==1, lc(black) lw(medthin)) ///
		(scatter estimate g1 if regression==1, msize(medlarge) m(triangle) mlw(medium) mlc(black) mfc(white)), ///
		ytitle("Average treatment effect") ylabel(,labsize(small)) ///
		legend(order(2 "Formal" 4 "Informal" 6 "Overall") row(1) ring(0) pos(6)) ///
		ylabel(-0.5(0.1)0.5) ///
		xtitle("Treatment group") xlabel(2(1)9) yline(0, lpattern(dash) lcolor(gs12)) graphregion(fcolor(white)) name(did_v_across,replace)
twoway 	(rcap min95 max95 group if regression==4, lc(midblue) lw(medthin)) ///
		(scatter estimate group if regression==4, msize(medlarge) m(X) mlw(medium) mlc(midblue) mfc(white)), ///
		ytitle("Average treatment effect") ylabel(,labsize(small)) ///
		legend(order(2 "Fraction informal") row(1) ring(0) pos(6)) ///
		ylabel(-0.5(0.1)0.5) ///
		xtitle("Treatment group") xlabel(2(1)9) yline(0, lpattern(dash) lcolor(gs12)) graphregion(fcolor(white)) name(did_inf_across,replace)		
		