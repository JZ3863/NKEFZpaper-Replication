
cd "output directory" 

use an, clear

**# import city-level outcome data
merge m:1 city year using city, gen(merge1)  // prepare city-level data. The author has no permission to share the data
keep if merge1 == 3
drop merge1

**# panel setting
* relative time
recode year (2011 = 1) (2013 = 2) (2015 = 3) (2018 = 4) (2020 = 5), gen(t)
encode ID, gen(id)
tsset id t

**# variables
**## sex
recode sex (1 = 0 "Female") (2 = 1 "Male"), gen(sex_)
drop sex
ren sex_ sex
**## uban
recode urban (1 = 0 "Rural") (2 = 1 "Urban"), gen(urban_)
drop urban
ren urban_ urban
**## marital
recode maritalc (3/6 = 0 "Single") ( 1/2= 1 "Married/cohabiting"), gen(marital)

**## education
recode educ (1 = 0 "Illiterate") (2/5 = 1 "Elementary/middle school") (6/11 = 2 "High school and above"), gen(edu3)

**## individual income
g pcincome = hhincome / hhsize

**## ADL
recode ADL (0 = 0 "No") (1/6 = 1 "Yes"), gen(dADL)

**## depress
recode depress (0/9 = 0 "No depressed") (10/30 = 1 "depressed"), g(depress1) //https://doi.org/10.1016/j.eclinm.2024.102767

**### loneliness
la de lone 0 "Rarely or none of the time" 1 "Some or a little of the time" ///
	2 "Occasionally or a moderate amount of the time" 3 "Most or all of the time", a
**### sleep quality
la de sleepquality 0 "Rarely or none of the time" 1 "Some or a little of the time" ///
	2 "Occasionally or a moderate amount of the time" 3 "Most or all of the time", a 
la var sleepquality "My sleep was restless"
**### sleep duration (https://doi.org/10.1016/j.jad.2025.119543)
g napsleep1 = napsleep / 60 // minutes to hours
egen sleephour = rowtotal(nightsleep napsleep1), m
g sleep6 = 0 if sleephour < 6
replace sleep6 = 1 if sleephour >= 6 & !mi(sleephour)
la de sleep6 0 "Insufficient sleep" 1 "Sufficient sleep", a

**## cognition
recode cognition (0/5 = 0 "Low cog") (6/31 = 1 "High cog"), gen(dcognition)
egen fluid = rowtotal(derecall imrecall serial), m
egen crystal = rowtotal(orient), m

egen episodic = rowtotal(derecall imrecall), m
alpha derecall imrecall, s // Cronbach's alpha = .7448
egen intact = rowtotal(serial draw orient), m
alpha serial draw orient, s //Cronbach's alpha = .59;
bys id: egen cognition2011 = max(cond(year==2011, cognition, .))
bys id: egen episodic2011 = max(cond(year==2011, episodic, .))
bys id: egen cognition2015 = max(cond(year == 2015, cognition, .))

**## social cohesion (https://doi.org/10.1016/j.jeoa.2019.100235)
replace socialact = socialact - 1
la de socialact 0 "No" 1 "Yes", modify

la de socialfreq 1 "Never" 2 "Not regularly" 3 "Almost every week" 4 "Almost daily"
foreach i of var socialcharity socialclub socialcom socialedu socialfriend ///
	socialhelp socialmajong socialother socialsick {
		replace `i'_f = 0 if `i' == 0 & mi(`i'_f)
		recode `i'_f (1 = 4) (2 = 3) (3 = 2) (0 = 1)
		label val `i'_f socialfreq
}

egen maxrecreation = rowmax(socialmajong_f socialclub_f socialcom_f)
alpha socialmajong_f socialclub_f socialcom_f, c //.26
recode maxrecreation (1 = 0 "No") (2/4 = 1 "Yes"), gen(drecreation)
egen maxaltruistic = rowmax(socialhelp_f socialsick_f socialcharity_f)
alpha socialhelp_f socialsick_f socialcharity_f, c // .336
alpha socialhelp_f socialsick_f, c s //.3228
recode maxaltruistic (1 = 0 "No") (2/4 = 1 "Yes"), gen(daltruistic)
egen dcohesion = rowmax(drecreation daltruistic)
egen ccohesion = rowmax(maxrecreation maxaltruistic)
bys id: egen dcohesion2011 = max(cond(year==2011, dcohesion, .))

**### contact with children
foreach i of var childcontactface childcontactvirtual {
	replace `i' = 10 - `i'
}
egen childcontact = rowmax(childcontactface childcontactvirtual)
la def contactfreq_rev ///
    9 "Almost every day" ///
    8 "2–3 times a week" ///
    7 "Once a week" ///
    6 "Every two weeks" ///
    5 "Once a month" ///
    4 "Once every three months" ///
    3 "Once every six months" ///
    2 "Once a year" ///
    1 "Almost never"
la val childcontactface contactfreq_rev
la val childcontactvirtual contactfreq_rev
la val childcontact contactfreq_rev

**## drink & smoke
g smokedrink = 1 if smoke == 1 | drinkl == 1
replace smokedrink = 0 if mi(smokedrink) & (smoke == 0 | drinkl == 0)
la var smokedrink "If the respondent reported smoking or drinking"

**## fuel
recode heatfuel (1 3/5 8 = 1 "Yes") (else = 0 "No") , gen(cleanheatfuel)
recode cookfuel (2/5 7 = 1 "Yes") (else  = 0 "No") , gen(cleancookfuel)
g cleanfuel = 1 if cleanheatfuel == 1 | cleancookfuel == 1
replace cleanfuel = 0 if mi(cleanfuel) & (cleanheatfuel == 0 | cleancookfuel == 0)

g cleanfuel2 = 1 if cookfuel == 5 | heatfuel == 5 // electricity
replace cleanfuel2 = 2 if mi(cleanfuel2) & (inlist(cookfuel, 2, 3, 4) | inlist(heatfuel, 3, 4)) // gas
replace cleanfuel2 = 3 if mi(cleanfuel2) & (cookfuel == 7 | heatfuel == 1) // solar
replace cleanfuel2 = 4 if mi(cleanfuel2) & (heatfuel == 8) // central heating
replace cleanfuel2 = 5 if mi(cleanfuel2) & (cookfuel == 1 | heatfuel == 2) //coal
replace cleanfuel2 = 6 if mi(cleanfuel2) & (cookfuel == 6 | heatfuel == 6) // cop/wood
replace cleanfuel2 = 7 if mi(cleanfuel2) & cleanfuel == 0 //others

lab def cleanfuel2_lab ///
    1 "Electricity" ///
    2 "Gas" ///
    3 "Solar" ///
    4 "Central heating" ///
    5 "Coal" ///
    6 "Crop/Wood" ///
    7 "Other"
lab val cleanfuel2 cleanfuel2_lab


bys id: egen cleanfuel2011 = max(cond(year==2011, cleanfuel, .))
bys id: egen cleancookfuel2011 = max(cond(year==2011, cleancookfuel, .))
bys id: egen cleancookfuel2013 = max(cond(year==2013, cleancookfuel, .))

**## city-level covariates
destring green*, force replace
replace green1 = green1 / 1000
la var green1 "绿地面积 (1000 公顷) / Area of Green Land (1000 hectare)" 
foreach i of var so2 soot {
    replace `i' = `i' / 100000
    local oldlabel : var label `i'
    la var `i' "`oldlabel' /100,000"
}
g lnpop = log(pop)

bys city t (ID): egen age_temp = mean(age >= 75) if age >= 60
bys city t: egen age75_prop = max(age_temp)
drop age_temp
bys city t: egen male_prop = mean(sex)
bys city t: egen literate_prop = mean(inlist(edu3, 1, 2))
bys city t: egen married_prop = mean(marital)
bys city t: egen hhsize_prop = mean(hhsize > 2 & !mi(hhsize))
bys city t: egen urban_prop = mean(urban == 1)
bys city t: egen dADL_prop = mean(dADL == 1)
bys city t: egen selfhealth_prop = mean(selfhealth == 4 | selfhealth == 5)
bys city t: egen chronic_prop = mean(chronic >= 2 & !mi(chronic))
bys city t: egen smokedrink_prop = mean(smokedrink == 1)

g carbon = (year >= lowcarbon)
la de carbonlab 0 "No" 1 "Yes"
la val carbon carbonlab

g treat = (NKEFZ == 2016 | NKEFZ == 2011)

*create group variable
g first_treat = 2 if NKEFZ == 2011 
replace first_treat = 4 if mi(first_treat) & NKEFZ == 2016
replace first_treat = 0 if mi(first_treat) 
distinct ID if first_treat == 2
**proportion of treated counties
bys ID: egen proportion = max(Proportion)
replace proportion = 0 if mi(proportion)
g ctreat = round(proportion * 100)
replace ctreat = 0 if year < 2016

**# Sample selection
distinct ID //25882; 129410
**## Step 0: remove not interviewed
distinct ID if inw == "No" // 14489; 31997
drop if inw == "No"
distinct ID 

**### Step 1: delete those age < 60
bys ID: egen min_age = min(age)
distinct ID if min_age < 60 | age > 200 //16464; 61391
drop if min_age < 60 | age > 200 

**## Step 2: delete IDs that present one time
bys ID: g id_count = _N
g single_obs = (id_count == 1)
distinct ID if single_obs == 1 // n = 922
drop if single_obs == 1
drop id_count single_obs
distinct ID

**## Step 3: always treated
g treated2011 = year == 2011 & inw == "Yes" & NKEFZ == 2011 & (r1iwy == 2012 | (r1iwm >= 6 & r1iwy == 2011))
bys id (year): egen always_treated = max(treated2011)
distinct ID if always_treated ==1 //4509; 19808
drop if always_treated ==1
drop always_treated treated2011
distinct ID

**## Step 4: only keep IDs who responded waves 2011-2015 & (2018 or 2020)
distinct ID if inw1 == "Yes" & inw2 == "Yes" & inw3 == "Yes" & (inw4 == "Yes" | inw5 == "Yes")
keep if inw1 == "Yes" & inw2 == "Yes" & inw3 == "Yes" & (inw4 == "Yes" | inw5 == "Yes")

**# CSDID
gl C0 age i.sex i.educl i.marital hhsize i.urban dADL i.selfhealth chronic i.smokedrink lnpop i.carbon

qui {
cap drop xtpcconsum*
qui csdid cognition $C0, i(id) t(t) gvar(first_treat) agg(event) long2
egen xtpcconsume_ = xtile(pcconsumption) if e(sample), n(4) by(year)
recode xtpcconsume_ (. = 5 "Missing") if e(sample), gen(xtpcconsume)
gl C $C0 i.xtpcconsume
bys city t: egen xtpcconsume_prop = mean(xtpcconsume == 3 | xtpcconsume == 4)
}

qui teffects ipw (cognition) (treat $C) if e(sample) & year == 2015, atet vce(cl city)
tebalance summarize
mat balance = r(table)
coefplot (mat(balance[,1]), offset(-.01) m(Oh) mc(black)) ///
	(mat(balance[,2]), m(O) mc(black)), ///
	xline(0 -.1 .1, lp(dash) lw(thin)) xti("Standardized difference") ///
	yline(1/20, lw(vthin) lc(gs12)) ///
	leg(order(1 "Raw" 2 "Weighted") pos(6) ring(1) col(2)) noci ///
	xlab(-.2 "-0.2" -.1 "-0.1"  0 "0" .1 "0.1" .2 "0.2" .4 "0.4", nogrid) ///
	ylab( ///
        1 "Age" ///
        2 "Gender" ///
		3 "Education: upper secondary/vocational training" ///
		4 "Education: tertiary education" ///
        5 "Marital status" ///
		6 "Household size" ///
		7 "Residence" ///
		8 "ADLs" ///
		9 "Health: poor" ///
        10 "Health: fair" ///
        11 "Health: good" ///
        12 "Health: very good" ///
		13 "Chronic diseases" ///
		14 "Smoking/drinking" ///
        15 "Population size" ///
        16 "Low-carbon City" ///
        17 "Expenditure: 2nd quartile" ///
        18 "Expenditure: 3rd quartile" ///
        19 "Expenditure: 4th quartile" ///
        20 "Expenditure: missing", ang(h)) grid(none)
		
csdid cognition $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
g sample = e(sample)

mat b_cog = e(b)[1, "Tm3".."Tp1"]
mat V_cog = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store cog

csdid episodic $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_ep = e(b)[1, "Tm3".."Tp1"]
mat V_ep = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store ep
csdid intact $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_in = e(b)[1, "Tm3".."Tp1"]
mat V_in = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store intact

**## event plot
coefplot (cog, m(O) mc(blue) ciop(recast(rcap) lc(blue) lp(solid)) lw(thin)) ///
    (ep, m(T) mc(dkgreen) ciop(recast(rcap) lc(dkgreen) lp(solid)) lw(thin)) ///
    (intact, m(S) mc(red) ciop(recast(rcap) lc(red) lp(solid)) lw(thin)), ///
    keep(Tm3 Tm2 Tp0 Tp1) vert graphregion(col(white)) ///
    yline(0, lc(black) lw(thin) lp(dash)) ///
    xline(2.5, lc(black) lw(thin) lp(dash)) ///
    coefl(Tm3 = "2011" Tm2 = "2013" Tp0 = "2018" Tp1 = "2020") ///
    leg(order(2 "Cognitive function" 4 "Episodic memory" 6 "Mental intactness") ///
           pos(6) symx(*0.5) keyg(*0.5) rows(1)) ///
    yti("ATT", s(medsmall) mar(r=-1)) ///
    ylab(-1 "-1" -.5 "-0.5" 0 "0" .5 "0.5" 1 "1", labs(medsmall) nogrid) ///
    text(-1.12 2.5 "2016", place(s) s(medsmall)) ///
    text(-1.12 4.4 "Year", place(s) s(medsmall) just(right))


**## Mechanisms
* environmental containments
csdid cleanfuel $C cleanfuel2011, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_clean = e(b)[1, "Tm3".."Tp1"]
mat V_clean = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store clean
preserve
gl City age75_prop male_prop literate_prop married_prop hhsize_prop urban_prop ///
	dADL_prop selfhealth_prop chronic_prop smokedrink_pro i.carbon lnpop xtpcconsume_prop
bys city year: keep if _n == 1
csdid soot $City, i(city) t(t) gvar(first_treat) cl(city) agg(event) long2 //good
mat b_soot = e(b)[1, "Tm3".."Tp1"]
mat V_soot = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store soot
csdid so2 $City, i(city) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_so2 = e(b)[1, "Tm3".."Tp1"]
mat V_so2 = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
restore

* social cohesion
csdid dcohesion $C dcohesion2011, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_dcoh = e(b)[1, "Tm3".."Tp1"]
mat V_dcoh = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store dcoh
csdid ccohesion $C if e(sample), i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_ccoh = e(b)[1, "Tm3".."Tp1"]
mat V_ccoh = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store ccoh

* physical activty
csdid physicallt $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_lt = e(b)[1, "Tm3".."Tp1"]
mat V_lt = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store lt
csdid physicalmd $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_md = e(b)[1, "Tm3".."Tp1"]
mat V_md = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store md
csdid physicalvg $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_vg = e(b)[1, "Tm3".."Tp1"]
mat V_vg = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store vg

* depress
csdid depress $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat b_depress = e(b)[1, "Tm3".."Tp1"]
mat V_depress = e(V)["Tm3".."Tp1", "Tm3".."Tp1"]
est store depress

**## event plot
coefplot (soot, m(O) mc(blue) ciop(recast(rcap) lc(blue) lp(solid)) lw(thin)) ///
    (so2, m(T) mc(dkgreen) ciop(recast(rcap) lc(dkgreen) lp(solid)) lw(thin)) ///
    (clean, m(S) mc(red) ciop(recast(rcap) lc(red) lp(solid)) lw(thin)), ///
    keep(Tm3 Tm2 Tp0 Tp1) vert graphregion(col(white)) ///
    yline(0, lc(black) lw(thin) lp(dash)) ///
    xline(2.5, lc(black) lw(thin) lp(dash)) ///
    coefl(Tm3 = "2011" Tm2 = "2013" Tp0 = "2018" Tp1 = "2020") ///
    leg(order(2 "Smoke and dust" 4 "SO{sub:2}" 6 "Clean fuel") ///
           pos(6) symx(*0.5) keyg(*0.5) rows(1)) ///
    yti("ATT", s(medsmall) mar(r=-1)) ///
    ylab(-.4 "-0.4" -.2 "-0.2" 0 "0" .2 "0.2" .4 "0.4", labs(medsmall) nogrid) ///
    text(-.525 2.5 "2016", place(s) s(medsmall)) ///
    text(-.525 4.4 "Year", place(s) s(medsmall) just(right)) 
	
graph save FigS3_a, replace

coefplot (dcoh, m(O) mc(blue) ciop(recast(rcap) lc(blue) lp(solid)) lw(thin)) ///
    (ccoh, m(T) mc(dkgreen) ciop(recast(rcap) lc(dkgreen) lp(solid)) lw(thin)) ///
    (lt, m(S) mc(red) ciop(recast(rcap) lc(red) lp(solid)) lw(thin)), ///
    keep(Tm3 Tm2 Tp0 Tp1) vert graphregion(col(white)) ///
    yline(0, lc(black) lw(thin) lp(dash)) ///
    xline(2.5, lc(black) lw(thin) lp(dash)) ///
    coefl(Tm3 = "2011" Tm2 = "2013" Tp0 = "2018" Tp1 = "2020") ///
    leg(order(2 "Social cohesion (dichotomous)" 4 "Social cohesion (continuous)" 6 "Mild physical activity") ///
           pos(6) symx(*0.5) keyg(*0.5) rows(1)) ///
    yti("ATT", s(medsmall) mar(r=-1)) ///
    ylab(-.2 "-0.2" 0 "0" .2 "0.2", labs(medsmall) nogrid) ///
    text(-.225 2.5 "2016", place(s) s(medsmall)) ///
    text(-.225 4.4 "Year", place(s) s(medsmall) just(right)) 
	
graph save FigS3_b, replace

coefplot (md, m(O) mc(blue) ciop(recast(rcap) lc(blue) lp(solid)) lw(thin)) ///
    (vg, m(T) mc(dkgreen) ciop(recast(rcap) lc(dkgreen) lp(solid)) lw(thin)) ///
    (depress, m(S) mc(red) ciop(recast(rcap) lc(red) lp(solid)) lw(thin)), ///
    keep(Tm3 Tm2 Tp0 Tp1) vert graphregion(col(white)) ///
    yline(0, lc(black) lw(thin) lp(dash)) ///
    xline(2.5, lc(black) lw(thin) lp(dash)) ///
    coefl(Tm3 = "2011" Tm2 = "2013" Tp0 = "2018" Tp1 = "2020") ///
    leg(order(2 "Moderate physical activity" 4 "Vigorous physical activity" 6 "Depressive symptoms") ///
           pos(6) symx(*0.5) keyg(*0.5) rows(1)) ///
    yti("ATT", s(medsmall) mar(r=-1)) ///
    ylab(-.5 "-0.5" 0 "0" .5 "0.5" 1 "1", labs(medsmall) nogrid) ///
    text(-.67 2.5 "2016", place(s) s(medsmall)) ///
    text(-.67 4.4 "Year", place(s) s(medsmall) just(right)) 
	
graph save FigS3_c, replace

graph combine "FigS3_a" "FigS3_b" "FigS3_c", rows(3)

**# Subgroup analysis
est clear
mat drop _all
**## sex
gl sex age i.educl i.marital hhsize i.urban dADL i.selfhealth chronic i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $sex if sex == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Female
mat sex1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $sex if sex == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Male; Good
mat sex2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $sex if sex == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Female
mat sex3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $sex episodic2011 if sex == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Male; good
mat sex4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

**## urban
gl urban age i.sex i.educl i.marital hhsize dADL i.selfhealth chronic i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $urban cognition2011 if urban == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Rural; good
mat urban1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $urban if urban == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Urban; good
mat urban2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $urban if urban == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Rural
mat urban3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $urban if urban == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Urban; good
mat urban4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

* economic capital
**## expenditure
gl consume age i.sex i.educl i.marital hhsize i.urban dADL i.selfhealth chronic i.smokedrink i.carbon lnpop
csdid cognition $consume if inlist(xtpcconsume, 1, 2), i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat exp1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $consume if inlist(xtpcconsume, 3, 4), i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat exp2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $consume if inlist(xtpcconsume, 1, 2), i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //
mat exp3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $consume if inlist(xtpcconsume, 3, 4), i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat exp4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

* social capital
**## marital
gl marital age i.sex i.educl hhsize i.urban dADL i.selfhealth chronic i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $marital if marital == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Single
mat marital1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $marital cognition2011 if marital == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Married/cohabiting
mat marital2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $marital if marital == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Single
mat marital3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $marital if marital == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Married/cohabiting
mat marital4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

**## household size
gl hhsize age i.sex i.educl i.marital i.urban dADL i.selfhealth chronic i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $hhsize if hhsize < 3, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat hhsize1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $hhsize if age >= 3, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat hhsize2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $hhsize if hhsize < 3, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat hhsize3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $hhsize if age >= 3, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat hhsize4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

* cultural capital
**## Edu--good
gl edu age i.sex i.marital hhsize i.urban dADL i.selfhealth chronic i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $edu if edu3 == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Illiterate
mat edu1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $edu if edu3 == 1 | edu3 == 2, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Literate
mat edu2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $edu if edu3 == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Illiterate
mat edu3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $edu if edu3 == 1 | edu3 == 2, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // Literate
mat edu4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

* health capital
**## age-non-sig
gl age sex i.educl i.marital hhsize i.urban dADL i.selfhealth chronic i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $age if age < 70, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat age1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $age if age >= 70, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat age2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $age if age < 70, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat age3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $age if age >= 70, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat age4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

**## ADL
gl ADL age i.sex i.educl i.marital hhsize i.urban i.selfhealth chronic i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $ADL cognition2011 if dADL == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // No ADL
mat ADL1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $ADL if dADL == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // ADL
mat ADL2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $ADL if dADL == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // No ADL
mat ADL3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $ADL if dADL == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // ADL
mat ADL4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

**## self-rated health
gl health age i.sex i.educl i.marital hhsize i.urban dADL chronic i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $health if selfhealth < 3, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat health1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $health if selfhealth >= 3, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat health2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $health if selfhealth < 3, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat health3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $health if selfhealth >= 3, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat health4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

**## chronic diseases
gl chronic age i.sex i.educl i.marital hhsize i.urban dADL i.selfhealth i.smokedrink i.carbon lnpop i.xtpcconsume
csdid cognition $chronic cognition2015 if chronic < 2, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat chronic1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $chronic if chronic >= 2, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat chronic2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $chronic if chronic < 2, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat chronic3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $chronic if chronic >= 2, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
mat chronic4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

**## smoke & drink
gl smoke age i.sex i.educl i.marital hhsize i.urban dADL i.selfhealth chronic i.carbon lnpop i.xtpcconsume
csdid cognition $smoke if smokedrink == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //
mat smoke1 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid cognition $smoke if smokedrink == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat smoke2 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $smoke if smokedrink == 0, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //
mat smoke3 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])
csdid episodic $smoke if smokedrink == 1, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
mat smoke4 = (r(table)[1,"Post_avg"] \ r(table)[5,"Post_avg"] \ r(table)[6,"Post_avg"])

**## plot coefficients
local cogmats age1 age2 sex1 sex2 urban1 urban2 exp1 exp2 edu1 edu2 marital1 marital2 hhsize1 hhsize2 ADL1 ADL2 health1 health2 chronic1 chronic2 smoke1 smoke2
local cogjoin: subinstr local cogmats " " " , ", all
mat cog = `cogjoin'
mat coln cog = `cogmats'
mat rown cog = b ll95 ul95
mat list cog
mat b_cog  = cog["b", .]
mat list b_cog
mat CI_cog = cog[2..3, .]
mat list CI_cog

local epimats age3 age4 sex3 sex4 urban3 urban4 exp3 exp4 edu3 edu4 marital3 marital4 hhsize3 hhsize4 ADL3 ADL4 health3 health4 chronic3 chronic4 smoke3 smoke4
local epijoin: subinstr local epimats " " " , ", all
mat epi = `epijoin'
mat coln epi = `cogmats'
mat rown epi = b ll95 ul95
mat list epi
mat b_epi  = epi["b", .]
mat list b_epi
mat CI_epi = epi[2..3, .]
mat list CI_epi

coefplot mat(b_cog), ci(CI_cog) mc(black) lc(black) bylabel(Cognitive function) || ///
         mat(b_epi), ci(CI_epi) mc(black) lc(black) bylabel(Episodic memory) ///
		 subtitle(, size(small) margin(zero)) ysize(8.5) xsize(10.5)  ///
		  xline(0, lp(dash) lw(thin)) xlab(-1 "-1" 0 "0" 1 "1", labs(vsmall) nogrid) leg(off) ///
         headings(age1 = "{bf:Age}" ///
				sex1 = "{bf:Gender}" ///
				urban1 = "{bf:Residence}" ///
				exp1 = "{bf:Expenditure}" ///
				edu1 = "{bf:Education level}" ///
				marital1 = "{bf:Marital status}" ///
				hhsize1 = "{bf:Household size}" ///
				ADL1 = "{bf:ADLs}" ///
				health1 = "{bf:Self-rated health}" ///
				chronic1 = "{bf:Chronic diseases}" ///
				smoke1 = "{bf:Smoke/drink}", labs(small)) ///
		 coeflabels(age1 = "<70" age2 = "≥70" ///
				sex1 = "Female" sex2 = "Male" ///
				urban1 = "Rural" urban2 = "Urban" ///
				exp1 = "1{sup:st} and 2{sup:nd} quartiles" exp2 = "3{sup:rd} and 4{sup:th} quartiles" ///
				edu1 = "Illiterate" edu2 = "Literate" ///
				marital1 = "Single" marital2 = "Married/cohabiting" ///
				hhsize1 = "<3" hhsize2 = "≥3" ///
				ADL1 = "No" ADL2 = "Yes" ///
				health1 = "Poor/fair" health2 = "Good" ///
				chronic1 = "<2" chronic2 = "≥2" ///
				smoke1 = "No" smoke2 = "Yes", labs(vsmall)) ///
         m(o) mc(black) mlc(black) ciopts(lc(black)) ///
		 graphregion(margin(zero) lstyle(none)) plotregion(margin(zero) style(none))


**# Sensitivity analysis
**## Alternative outcomes
set cformat %9.3f
* cognition
csdid dcognition $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //good
csdid fluid $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 // good


* air quality
csdid cleancookfuel $C cleancookfuel2011 cleancookfuel2013, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 
display %12.11f r(table)[4, 3] // good

* social cohesion
csdid socialact $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2
csdid drecreation $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //NS
csdid daltruistic $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //NS

* physical activty
csdid physicalltday $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //NS
csdid physicalmdday $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //NS
csdid physicalvgday $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //NS

* stress
csdid lone $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //NS
csdid sleepquality $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //NS
set pformat %5.4f
csdid sleep6 $C, i(id) t(t) gvar(first_treat) cl(city) agg(event) long2 //NS
set pformat %5.3f

**## placebo
**## In-time: - 1
g first_treat_1 = first_treat - 1 
replace first_treat_1 = 0 if first_treat_1 == -1

csdid cognition $C, i(id) t(t) gvar(first_treat_1) cl(city) agg(event) long2 
csdid episodic $C, i(id) t(t) gvar(first_treat_1) cl(city) agg(event) long2 
csdid cleanfuel $C cleanfuel2011, i(id) t(t) gvar(first_treat_1) cl(city) agg(event) long2 
preserve
gl City age75_prop male_prop literate_prop married_prop hhsize_prop urban_prop ///
	dADL_prop selfhealth_prop chronic_prop smokedrink_pro i.carbon lnpop xtpcconsume_prop
bys city year: keep if _n == 1
csdid soot $City, i(city) t(t) gvar(first_treat_1) cl(city) agg(event) long2
restore

**## In-place
save temp, replace
* individual-level
use temp, clear

cap program drop InSpacePlaceboTest
program define InSpacePlaceboTest, rclass
    syntax varname
    preserve
    bys city (t): keep if _n==1
    xtshuffle first_treat, id(city) time(t) gen(first_treat_new)
    bys city: keep if _n==1
    tempfile shuffled_city
    keep city first_treat_new
    save `shuffled_city', replace
    use temp, clear
    merge m:1 city using `shuffled_city', nogen
    qui csdid `varlist' $C, i(id) t(t) gvar(first_treat_new) cl(city) agg(simple) long2
	scalar b_att = _b[ATT]
	scalar se_att = _se[ATT]
	scalar z_att = b_att / se_att
	scalar p_att = 2 * (1 - normal(abs(z_att)))
    return scalar pbo_eff = _b[ATT]
	return scalar pbo_z   = z_att
	return scalar pbo_p   = p_att
end
foreach y in cognition episodic cleanfuel {
    use temp, clear
    simulate pbo_eff_`y' = r(pbo_eff) ///
             pbo_z_`y'   = r(pbo_z) ///
             pbo_p_`y'   = r(pbo_p), ///
             reps(500) seed(1234) ///
             saving(pbo_`y', replace): InSpacePlaceboTest `y'
}

* interpret results
** cognition
use pbo_cognition, clear
g extreme_abs = (abs(pbo_eff)>= .467)
su extreme_abs
twoway ///
    (scatter pbo_p_cognition pbo_eff_cognition, ///
        msymbol(oh) mcolor(black) ///
        xlabel(-1 "-1" -.5 "-0.5" 0 "0" .5 "0.5" 1 "1", nogrid) ///
        ylabel(0 "0" .2 "0.2" .4 "0.4" .6 "0.6" .8 "0.8" 1 "1", nogrid) ///
        ytitle("P value") xtitle("ATT") leg(off)), ///
    xline(0 .4672139, lc(black) lp(dash)) ///
	yline(.008, lc(black) lp(dash)) title("Cognitive function", size(medium))
graph save F31, replace

** episodic memory
use pbo_episodic, clear
g extreme_abs = (abs(pbo_eff)>= .3715866)
su extreme_abs
twoway ///
    (scatter pbo_p_episodic pbo_eff_episodic, ///
        msymbol(oh) mcolor(black) ///
        xlabel(-1 "-1" -.5 "-0.5" 0 "0" .5 "0.5" 1 "1", nogrid) ///
        ylabel(0 "0" .2 "0.2" .4 "0.4" .6 "0.6" .8 "0.8" 1 "1", nogrid) ///
        ytitle("P value") xtitle("ATT") leg(off)), ///
    xline(0 .3715866, lc(black) lp(dash)) ///
	yline(.009, lc(black) lp(dash)) title("Episodic memory", size(medium))
graph save F32, replace

** clean fule
use pbo_cleanfuel, clear
g extreme_abs = (abs(pbo_eff)>= .0443955)
su extreme_abs
twoway ///
    (scatter pbo_p pbo_eff, ///
        msymbol(oh) mcolor(black) ///
        xlabel(-.1 "-0.1" -.05 "-0.05" 0 "0" .05 "0.05" .1 "0.1", nogrid) ///
        ylabel(0 "0" .2 "0.2" .4 "0.4" .6 "0.6" .8 "0.8" 1 "1", nogrid) ///
        ytitle("P value") xtitle("ATT") leg(off)), ///
    xline(0 .0443955, lc(black) lp(dash)) ///
	yline(.028, lc(black) lp(dash)) title("Clean fuel", size(medium))
graph save F33, replace

graph combine "F31" "F32" "F33"

**## continous treatment
foreach i of var educl selfhealth xtpcconsume {
	tab `i', gen(`i'_)
}

gl Cc age sex marital educl_1 educl_2 educl_3 urban hhsize dADL selfhealth_1 ///
	selfhealth_2 selfhealth_3 selfhealth_4 selfhealth_5 ///
	chronic smokedrink lnpop carbon xtpcconsume_1 xtpcconsume_2 xtpcconsume_3 ///
	xtpcconsume_4 xtpcconsume_5
preserve
did_multiplegt_dyn cognition id t ctreat if sample == 1, effects(2) placebo(2) cluster(id) ///
	only_never_switchers controls($Cc) ///
	continuous(1) bootstrap(50,1) save_results(temp1)
g p = 2 * normal(-abs(point_estimate / se_point_estimate)) //p value
restore

preserve
did_multiplegt_dyn episodic id t ctreat if sample == 1, effects(2) placebo(2) cluster(id) ///
	only_never_switchers controls($Cc) ///
	continuous(1) bootstrap(50,1) save_results(temp2)
g p = 2 * normal(-abs(point_estimate / se_point_estimate)) //p value
restore
	
preserve
did_multiplegt_dyn cleanfuel id t ctreat if sample == 1, effects(2) placebo(2) cluster(id) ///
	only_never_switchers controls($Cc) ///
	continuous(1) bootstrap(50,1) save_results(temp3)
g p = 2 * normal(-abs(point_estimate / se_point_estimate)) //p value
restore

**# Descriptive
gl des cognition episodic intact cleanfuel soot so2 dcohesion ccohesion physicallt ///
	physicalmd physicalvg depress ///
	age i.sex i.educl i.marital hhsize i.urban i.xtpcconsume i.dADL i.selfhealth ///
	chronic i.smokedrink pop i.carbon

dtable $des if sample == 1, by(treat, tests) fact(, stat(fvfreq fvperc)) ///
    nformat("%9.0fc"fvfreq) nformat("%9.2f" fvperc) nformat("%9.2f" mean) nformat("%9.2f" sd) ///
	export(tab1120.xlsx, replace)

tab1 cleanfuel2 if sample == 1
tab cleanfuel2 urban if sample == 1, cell


**## attrition

* Step 1: Create individual-level dropout indicators for each wave
bys id: egen dropout_ind4 = max(inw4 == "No") if sample==1
bys id: egen dropout_ind5 = max(inw5 == "No") if sample==1

* Step 2: Check number of distinct individuals who dropped out
distinct id if dropout_ind4==1 & sample==1
distinct id if dropout_ind5==1 & sample==1

* Step 3: Tabulate percent of individuals who dropped out
preserve
keep if sample==1
bys id: keep if _n==1  // keep one row per individual

tab dropout_ind4
tab dropout_ind5
tab dropout_ind4 first_treat
tab dropout_ind5 first_treat

restore


**##  Prop of rural residents
preserve
keep if sample == 1
bys ID: keep if _n == 1
g rural = (urban == 0)
bys cityname: egen total_residents = count(rural)
bys cityname: egen rural_residents = total(rural)
g prop_rural = 100 * (rural_residents / total_residents)
bys cityname: keep if _n == 1
format prop_rural %6.1f
sort provincename
list cityname prop_rural first_treat, noobs
su rural
bys first_treat: su rural
restore








