clear all
set more off

global data "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/"
*ag land census
clear
import delimited "$data/raw_data_nass/ag_land_census.csv", encoding(utf8) 

keep year state stateansi county countyansi value
drop if countyansi==.

generate fips = 1000*stateansi + countyansi

rename value cropland
drop if cropland==" (D)"
destring cropland, replace 

label var cropland "Cropland in acres"
save "$data/raw_data_nass/ag_land_census.dta", replace

*ag land census - irrigated
clear
import delimited "$data/raw_data_nass/ag_land_census_harvested_irrigated.csv", encoding(utf8) 

keep year state stateansi county countyansi value
drop if countyansi==.

generate fips = 1000*stateansi + countyansi

rename value cropland_irri
drop if cropland_irri==" (D)"
destring cropland_irri, replace 

label var cropland_irri "Harvested, Irrigated Cropland in acres"
save "$data/raw_data_nass/ag_land_census_harvested_irrigated.dta", replace

*non-irrigated cash rent
clear
import delimited "$data/raw_data_nass/nonirrigated_cash_rent.csv", encoding(utf8) 

drop program period weekending geolevel zipcode region watershed_code watershed watershed ///
commodity dataitem domain domaincategory cv
drop if countyansi==.

generate fips = 1000*stateansi + countyansi

rename value non_irri_rent
label var non_irri_rent "RENT, CASH, CROPLAND, NON-IRRIGATED - EXPENSE, MEASURED IN $ / ACRE"
save "$data/raw_data_nass/non_irrigated_2008_2017.dta", replace

*irrigated cash rent
clear
import delimited "$data/raw_data_nass/irrigated_cash_rent.csv", encoding(utf8) 

drop program period weekending geolevel zipcode region watershed_code watershed watershed ///
commodity dataitem domain domaincategory cv
drop if countyansi==.

generate fips = 1000*stateansi + countyansi

rename value irri_rent
label var irri_rent "RENT, CASH, CROPLAND, IRRIGATED - EXPENSE, MEASURED IN $ / ACRE"
save "$data/raw_data_nass/irrigated_2008_2017.dta", replace


*clean planted acreage data (barley corn oats rice sorghum soybeans cotton wheat)
foreach x in barley corn oats rice sorghum soybeans ucotton dwheat swheat wwheat{
import delimited "$data/raw_data_nass/`x'_planted_acres.csv", encoding(utf8) clear

destring year, replace
destring value, replace

keep year state stateansi county countyansi value
drop if countyansi==.

generate fips = 1000*stateansi + countyansi

rename value `x'_planted_acres
label var `x'_planted_acres "`x', planted acres"
save "$data/raw_data_nass/`x'_planted_acres.dta", replace
}

*clean irrigated planted acreage data (corn sorghum soybeans cotton wheat)
foreach x in corn sorghum soybeans ucotton swheat wwheat{
import delimited "$data/raw_data_nass/`x'_planted_acres_irri.csv", encoding(utf8) clear

destring year, replace
destring value, replace

keep year state stateansi county countyansi value
drop if countyansi==.

generate fips = 1000*stateansi + countyansi

rename value `x'_planted_acres_irri
label var `x'_planted_acres_irri "`x', planted acres, irrigated"
save "$data/raw_data_nass/`x'_planted_acres_irri.dta", replace
}

*clean yield data (barley corn oats rice sorghum soybeans cotton wheat)
foreach x in barley corn oats rice sorghum soybeans ucotton dwheat swheat wwheat{

import delimited "$data/raw_data_nass/`x'_yield.csv", encoding(utf8) clear

destring year, replace
destring value, replace

keep year state stateansi county countyansi value
drop if countyansi==.

generate fips = 1000*stateansi + countyansi

rename value `x'_yield
label var `x'_yield "`x', planted acres"
save "$data/raw_data_nass/`x'_yield.dta", replace
}

*clean price data (barley corn oats rice sorghum soybeans cotton wheat)
foreach x in barley corn oats rice sorghum soybeans ucotton wheat{

import delimited "$data/raw_data_nass/`x'_price.csv", encoding(utf8) clear

destring year, replace
destring value, replace

keep year value

rename value `x'_price
label var `x'_price "`x', price"
save "$data/raw_data_nass/`x'_price.dta", replace
}

*clean prod value data (barley corn oats rice sorghum soybeans cotton wheat)
foreach x in barley corn oats rice sorghum soybeans ucotton wheat{

import delimited "$data/raw_data_nass/`x'_production_value.csv", encoding(utf8) clear

destring year, replace
destring value, replace

keep year value

rename value `x'_production_value
label var `x'_production_value "`x', production value"
save "$data/raw_data_nass/`x'_production_value.dta", replace
}

*keep fips with non_irri_rent data with geocode
use "$data/raw_data_nass/non_irrigated_2008_2017.dta", clear 
collapse (mean) non_irri_rent, by(fips)
rename non_irri_rent avg_non_irri_rent
merge 1:1 fips using "$data/us_county_shape/cb_2017_us_county_20m.dta"
keep if _merge==3
drop _merge

expand 19
bys fips: gen year=_n
replace year=year+1999
drop STATEFP COUNTYFP COUNTYNS AFFGEOID GEOID NAME LSAD
save "$data/raw_data_nass/non_irri_rent_fips.dta", replace


*merge
use "$data/raw_data_nass/non_irri_rent_fips.dta", clear
merge 1:1 fips year using "$data/raw_data_nass/non_irrigated_2008_2017.dta"
drop _merge

merge 1:1 fips year using "$data/raw_data_nass/irrigated_2008_2017.dta"
drop if _merge==2
drop _merge

foreach x in barley corn oats rice sorghum soybeans ucotton dwheat swheat wwheat{

merge 1:1 fips year using "$data/raw_data_nass/`x'_planted_acres.dta"
drop if _merge==2
drop _merge

merge 1:1 fips year using "$data/raw_data_nass/`x'_yield.dta"
drop if _merge==2
drop _merge
}

foreach x in corn sorghum soybeans ucotton swheat wwheat{

merge 1:1 fips year using "$data/raw_data_nass/`x'_planted_acres_irri.dta"
drop if _merge==2
drop _merge
}

merge 1:1 fips year using "$data/raw_data_nass/ag_land_census.dta"
drop if _merge==2
drop _merge

merge 1:1 fips year using "$data/raw_data_nass/ag_land_census_harvested_irrigated.dta"
drop if _merge==2
drop _merge

foreach x in barley corn oats rice sorghum soybeans ucotton wheat{

merge m:1 year using "$data/raw_data_nass/`x'_price.dta"
drop _merge

merge m:1 year using  "$data/raw_data_nass/`x'_production_value.dta"
drop _merge
}

save "$data/non_irrigated_rent_acre_yield_2008_2017.dta", replace
