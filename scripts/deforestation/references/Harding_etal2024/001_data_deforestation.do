***************************************************
* Deforestation data tiddying and donations merge *
***************************************************


** Overall cleaning

import delimited using "$rasters/Deforestation/output_data/Hansen_2000_2018.csv", clear delimit(";")

// depending on the SO R may deliver data differently
cap replace cover = subinstr(cover, ",", ".",.)
destring cover , replace

replace cover = cover / 10000 // Rescale to Ha2

drop v1 aoi loss

order x muni_code year cover
rename cover coverage

sort muni_code year
gen cover2000 = coverage if year == 2000
gen cover2011 = coverage if year == 2011
gen cover2007 = coverage if year == 2007
bys muni_code : egen coverage2000 = max(cover2000)
bys muni_code : egen coverage2011 = max(cover2011)
bys muni_code : egen coverage2007 = max(cover2007)
drop cover2000 cover2011 cover2007
sort muni_code year

gen ratio2011 = coverage / coverage2011
gen ratio2007 = coverage / coverage2007


forvalues i = 1/4{
bys muni_code : gen def_pre`i' = coverage[_n-`i'] - coverage[_n]
bys muni_code : gen def_post`i' = coverage[_n] - coverage[_n+`i']
gen def_ratio_pre`i'  = def_pre`i' / coverage2000 * 100
gen def_ratio_post`i' = def_post`i' / coverage2000 * 100
gen def_ratio2011_pre`i'  = def_pre`i' / coverage2007 * 100
gen def_ratio2011_post`i' = def_post`i' / coverage2011 * 100
gen ratio2011_post`i' = ratio2011[_n+`i']
gen ratio2011_pre`i' = ratio2007[_n-`i']
lab var def_pre`i' 		 "Deforested area ~ `i'y time window pre"
lab var def_post`i' 	 "Deforested area ~ `i'y time window post"
lab var def_ratio_pre`i' "Ratio: Deforested area/coverage2000 ~ `i'y time window pre"
lab var def_ratio_post`i' "Ratio: Deforested area/coverage2000 ~ `i'y time window post"
lab var def_ratio2011_pre`i' "Ratio: Deforested area/coverage2000 ~ `i'y time window pre"
lab var def_ratio2011_post`i' "Ratio: Deforested area/coverage2000 ~ `i'y time window post"
lab var ratio2011_post`i' "Ratio: Coverage / coverage 2011 ~ `i'y time window post "
lab var ratio2011_pre`i' "Ratio: Coverage / coverage 2007 ~ `i'y time window pre "
gen inv_ratio2011_post`i' = 1/ratio2011_post`i'
gen inv_ratio2011_pre`i' = 1/ratio2011_pre`i'

// yearly def
bys muni_code : gen def_prey`i' = coverage[_n-`i'] - coverage[_n-`=`i'-1']
gen def_ratio_prey`i'  = def_prey`i' / coverage2007 * 100
lab var def_ratio_prey`i' "Ratio: Deforested area/coverage2000 ~ `i'y pre"
bys muni_code : gen def_posty`i' = coverage[_n+`=`i'-1'] - coverage[_n+`i']
gen def_ratio_posty`i' = def_posty`i' / coverage2011 * 100
lab var def_ratio_posty`i' "Ratio: Deforested area/coverage2000 ~ `i'y pst"
}


** keep relevant variables 
keep muni_code year coverage2011 def_ratio2011_pre1 def_ratio2011_post1 def_ratio_posty1 def_ratio2011_pre2 def_ratio2011_post2 def_ratio_posty2 def_ratio2011_pre3 def_ratio2011_post3 def_ratio_posty3 def_ratio_pre4 def_ratio2011_pre4 def_ratio2011_post4 def_ratio_posty4

******* Adjust deforestation data for easy merge:

keep if year == 2012 // first year of goverment
replace year = 2011 // Elecction year (so it merges correctly)

/*
***** Merge with donations data
/* This data has personally identifiable information, sensitive and not included for privacy reasons */
merge 1:m muni_code year using "$data/MASTER_AUG21_6"
gsort muni_code year - votes 
keep if _merge == 3
drop _merge
*/

*** save dataset 
cap mkdir "$datacons/output_datasets"
save "$datacons/output_datasets/deforestation_2011.dta", replace



