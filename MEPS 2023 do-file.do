**MEPS 2023 Data Set**

/*
	Research Question
	1. How are socioeconomic factors (race/ethnicity and poverty) associated with Medicaid 
	coverage and the prevalence of select chronic and behavioral health conditions (mental health,
	diabetes, hypertension)?
	2. Controlling for socioeconomic factors and coverage, how do these health burdens relate to 
	reliance on non-hospital care (e.g., typplsick, obtotvis)?
*/


**Upload and convert higher-level MEPS do-files**
clear
cd /Users/tobi/Data
do meps_00004.do
save "meps2023.dta", replace

*Extract and Build person-level condition flags from hierchical condition file*
clear
do /Users/tobi/Data/meps_00005.do
save /Users/tobi/Data/meps_00005.dta, replace
use "meps_00005.dta", clear

*Fill parent MEPSID downward through hierarchical rows
gen mepsid_fill = mepsid
forvalues i = 1/10 {
    replace mepsid_fill = mepsid_fill[_n-1] if mepsid_fill == ""
}

*Check that condition rows now have person IDs
count if ccsr1 != "" & mepsid_fill != ""
count if diabeticev < . & mepsid_fill != ""
count if hypertenev < . & mepsid_fill != ""

*Dichotimize Variables*
*Mental health row flag from selected CCSR1 groups
gen mh_row = 0
replace mh_row = 1 if inlist(ccsr1, ///
    "MBD002", ///
    "MBD003", ///
    "MBD005", ///
    "MBD007", ///
    "MBD013", ///
    "MBD025")
label variable mh_row "Row flag mental health conditions"

*Diabetes row flag
gen diabetes_row = .
replace diabetes_row = 1 if diabeticev == 2
replace diabetes_row = 0 if diabeticev == 1
label variable diabetes_row "Row flag diabetes"

*Hypertension row flag
gen hyperten_row = .
replace hyperten_row = 1 if hypertenev == 2
replace hyperten_row = 0 if hypertenev == 1
label variable hyperten_row "Row flag hypertension"

*Collapse to one row per person
bysort mepsid_fill: egen any_mh = max(mh_row)
label variable any_mh "Person indicator mental health"

bysort mepsid_fill: egen any_diabetes = max(diabetes_row)
label variable any_diabetes "Person indicator diabetes"

bysort mepsid_fill: egen any_hyperten = max(hyperten_row)
label variable any_hyperten "Person indicator hypertension"

replace any_hyperten = 0 if missing(any_hyperten)
replace any_diabetes = 0 if missing(any_diabetes)

bysort mepsid_fill: keep if _n == 1

*Keep only merge key + final flags
keep mepsid_fill any_mh any_diabetes any_hyperten
rename mepsid_fill mepsid

save "condition_flags.dta", replace


**Clean Data**
*Merge person-level condition flags into clean person file*
use "/Users/tobi/Data/meps2023.dta", clear
merge 1:1 mepsid using "/Users/tobi/Data/condition_flags.dta"
tab _merge

describe
summarize

*Medicaid indicator
gen medicaid = .
replace medicaid = 0 if himachip == 1
replace medicaid = 1 if himachip == 2
label variable medicaid "Medicaid Indicator"

*Poverty indicator
gen poverty = .
replace poverty = 1 if povlev < 100 & povlev >= 0
replace poverty = 0 if povlev >= 100
label variable poverty "Poverty Indicator"

*Race Indicator & Group Multiple Race
gen race_grp = .

replace race_grp = 1 if racea == 100
replace race_grp = 2 if racea == 200
replace race_grp = 3 if racea == 310
replace race_grp = 4 if racea == 410
replace race_grp = 5 if racea == 420
replace race_grp = 6 if inrange(racea, 610, 617)
replace race_grp = 7 if racea == 900

label define race_grp_lbl ///
1 "White" ///
2 "Black" ///
3 "AI/AN" ///
4 "Asian" ///
5 "Pacific Islander" ///
6 "Multiple race" ///
7 "Unknown"

label values race_grp race_grp_lbl
label variable race_grp "Race Indicator"

*Convert MEPS Code 996 to missing variables
replace age = . if age == 996
summarize age
misstable summarize

save "meps2023_analysis.dta", replace


**Data Check**

*Bivariate Analysis*
svyset psuann [pweight=perweight], strata(stratann)
svy: tab hispyn any_hyperten, row

tab poverty medicaid, row
tab race_grp medicaid, row
tab hispyn medicaid, row
tab sex medicaid, row

tab any_mh medicaid, row 
tab any_diabetes medicaid, row
tab any_hyperten medicaid, row 

tab sex any_mh, row
tab sex any_diabetes, row
tab sex any_hyperten, row

tab race_grp any_mh, row
tab race_grp any_diabetes, row
tab race_grp any_hyperten, row

tab hispyn any_mh, row
tab hispyn any_diabetes, row
tab hispyn any_hyperten, row

tab poverty any_mh, row
tab poverty any_diabetes, row
tab poverty any_hyperten, row

gen lobtotvis = ln(obtotvis) if obtotvis > 0
label variable lobtotvis "Obtotvis Log"
histogram lobtotvis, percent title("Distribution of Office-Based Visits")

tabstat exptot if regionmeps != 0, by(regionmeps) stat(mean sd n)
graph box exptot if regionmeps != 0, over(regionmeps) title("MEPS Expenditures by Region")

graph bar (mean) medicaid, over(race_grp) title("Medicaid Coverage by Race")
graph bar (mean) medicaid, over(hispyn) title("Medicaid Coverage by Ethnicity")
graph bar (mean) medicaid, over(poverty) title("Medicaid Coverage by Poverty Status")

tabstat obtotvis, by(medicaid) stat(mean sd n)
tabstat obtotvis, by(any_mh) stat(mean sd n)
tabstat obtotvis, by(any_diabetes) stat(mean sd n)
tabstat obtotvis, by(any_hyperten) stat(mean sd n)
tabstat obtotvis, by(ladl) stat(mean sd n)
tabstat obtotvis, by(lmtphys) stat(mean sd n)
tabstat obtotvis, by(sex) stat(mean sd n)

ttest obtotvis, by(medicaid)
ttest obtotvis, by(any_mh)
ttest obtotvis, by(any_diabetes)
ttest obtotvis, by(any_hyperten)

*Multivariate Analysis*
regress obtotvis i.medicaid
regress obtotvis i.medicaid i.race_grp
regress obtotvis i.medicaid i.race_grp i.hispyn 
regress obtotvis i.medicaid i.race_grp i.hispyn povlev 
regress obtotvis i.medicaid i.race_grp i.hispyn povlev age 
regress obtotvis i.medicaid i.race_grp i.hispyn povlev age i.sex 
regress obtotvis i.medicaid i.race_grp i.hispyn povlev age i.sex i.any_mh i.any_diabetes i.any_hyperten
poisson obtotvis i.medicaid i.race_grp i.hispyn povlev age i.sex i.any_mh i.any_diabetes i.any_hyperten
poisson obtotvis i.medicaid i.race_grp i.hispyn povlev age i.sex i.any_mh i.any_diabetes i.any_hyperten, irr

vif











