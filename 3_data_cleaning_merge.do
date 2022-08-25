set more off
cd "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/"

global data "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/"

*PPI data
clear
import delimited "$data/ppi_allcommodities.txt", clear 
keep if year>1988
keep if period=="M13" | period=="M01"
drop if period=="M01" & year<2019
rename value ppi
keep year ppi
tempfile ppi
save `ppi', replace

*NASS data
use non_irrigated_rent_acre_yield_2008_2017.dta, clear
*merge with ppi
merge m:1 year using `ppi'
drop _merge
*fill in missing stateansi using fips
bys fips: egen state_code=max(stateansi)
replace stateansi=state_code if stateansi==.
*drop 2019
drop if year>2017
*drop with missing geo info
drop if _CX==.
tab state

*Cash rent, and price: convert to 2017 real dollars (1982 = 100, 2017=193.5, BLS ppi)
gen non_irri_rent_r=(non_irri_rent/ppi)*193.5 
label var non_irri_rent_r "Cash rent, non-irrigated, $/acre, PPI adjusted (2017=100)"
*price
foreach x in barley oats corn sorghum soybeans ucotton wheat{
gen `x'_price_r= (`x'_price/ppi)*193.5 
gen ln_`x'_price_r=ln(`x'_price_r)
}

*acres missing with zeros
foreach x in barley corn oats rice sorghum soybeans ucotton dwheat swheat wwheat{
replace `x'_planted_acres=0 if `x'_planted_acres==.
}
foreach x in corn sorghum soybeans ucotton swheat wwheat{
replace `x'_planted_acres_irri=0 if `x'_planted_acres_irri==.
}

*2003 - 2007 5-year average planted acres
egen total_field=rowtotal(*_planted_acres)
egen total_field_irri=rowtotal(*_planted_acres_irri)
gen total_field_nonirri=total_field-total_field_irri
*nass discrepency? One county (OK) is total_field_nonirri<0 & total_field_nonirri!=.
replace total_field_nonirri=0 if total_field_nonirri<0
*moving average
xtset fips year
tssmooth ma avg5_total_field_nonirri=total_field_nonirri, window(4 1 0)
*aggregate wheat
gen wheat_planted_acres=dwheat_planted_acres+swheat_planted_acres+wwheat_planted_acres
gen wheat_planted_acres_irri=swheat_planted_acres_irri+wwheat_planted_acres_irri
*by crop
foreach x in barley corn oats rice sorghum soybeans ucotton dwheat swheat wwheat wheat{
tssmooth ma avg5_`x'_planted_acres=`x'_planted_acres, window(4 1 0)
}
foreach x in corn sorghum soybeans ucotton swheat wwheat wheat{
tssmooth ma avg5_`x'_planted_acres_irri=`x'_planted_acres_irri, window(4 1 0)
}
foreach x in corn sorghum soybeans ucotton swheat wwheat wheat{
gen `x'_planted_acres_nirri=`x'_planted_acres-`x'_planted_acres_irri
tssmooth ma avg5_`x'_planted_nirri=`x'_planted_acres_nirri, window(4 1 0)
}

foreach x in barley corn oats rice sorghum soybeans ucotton dwheat swheat wwheat wheat{
tssmooth ma avg3_`x'_planted_acres=`x'_planted_acres, window(2 1 0)
}
foreach x in corn sorghum soybeans ucotton swheat wwheat wheat{
tssmooth ma avg3_`x'_planted_acres_irri=`x'_planted_acres_irri, window(2 1 0)
}
foreach x in corn sorghum soybeans ucotton swheat wwheat wheat{
tssmooth ma avg3_`x'_planted_nirri=`x'_planted_acres_nirri, window(2 1 0)
}

foreach x in barley corn oats rice sorghum soybeans ucotton dwheat swheat wwheat wheat{
tssmooth ma avg7_`x'_planted_acres=`x'_planted_acres, window(6 1 0)
}
foreach x in corn sorghum soybeans ucotton swheat wwheat wheat{
tssmooth ma avg7_`x'_planted_acres_irri=`x'_planted_acres_irri, window(6 1 0)
}
foreach x in corn sorghum soybeans ucotton swheat wwheat wheat{
tssmooth ma avg7_`x'_planted_nirri=`x'_planted_acres_nirri, window(6 1 0)
}


*2007 & 2012 croplands
gen year2007=0
replace year2007=1 if year==2007
gen year2012=0
replace year2012=1 if year==2012
gen year2007_cropland=year2007*cropland
bys fips: egen cropland_2007=max(year2007_cropland)
gen year2012_cropland=year2012*cropland
bys fips: egen cropland_2012=max(year2012_cropland)
*gen non-irrigated cropland
replace cropland_irri=0 if cropland_irri==.
gen cropland_nonirri=cropland-cropland_irri
gen year2007_cropland_nonirrigated=year2007*cropland_nonirri
bys fips: egen cropland_2007_nonirrigated=max(year2007_cropland_nonirrigated)
gen year2012_cropland_nonirrigated=year2012*cropland_nonirri
bys fips: egen cropland_2012_nonirrigated=max(year2012_cropland_nonirrigated)
*gen non-irrigated field crops planted
gen year2007_planted_nonirrigated=year2007*avg5_total_field_nonirri
bys fips: egen planted_2007_nonirrigated=max(year2007_planted_nonirrigated)
gen year2012_planted_nonirrigated=year2012*avg5_total_field_nonirri
bys fips: egen planted_2012_nonirrigated=max(year2012_planted_nonirrigated)
*trend
gen trend=year-2007
gen trend2=trend^2

*initial share exclude irrigated
foreach x in barley oats {
gen year2007xavg5_`x'_excl_irri=year2007*(avg5_`x'_planted_acres/cropland_2007_nonirrigated)
bys fips: egen initial_share_`x'_excl_irri=max(year2007xavg5_`x'_excl_irri)
label var initial_share_`x'_excl_irri "Share of `x' (2003 - 2007 average)"
}
foreach x in corn sorghum soybeans ucotton wheat{
gen year2007xavg5_`x'_excl_irri=year2007*(avg5_`x'_planted_nirri/cropland_2007_nonirrigated)
bys fips: egen initial_share_`x'_excl_irri=max(year2007xavg5_`x'_excl_irri)
label var initial_share_`x'_excl_irri "Share of `x' (2003 - 2007 average)"
}
*initial share exclude irrigated: ma3
foreach x in barley oats {
gen year2007xavg3_`x'_excl_irri=year2007*(avg3_`x'_planted_acres/cropland_2007_nonirrigated)
bys fips: egen initial_alt_`x'_excl_irri=max(year2007xavg3_`x'_excl_irri)
label var initial_alt_`x'_excl_irri "Share of `x' (2005 - 2007 average)"
}
foreach x in corn sorghum soybeans ucotton wheat{
gen year2007xavg3_`x'_excl_irri=year2007*(avg3_`x'_planted_nirri/cropland_2007_nonirrigated)
bys fips: egen initial_alt_`x'_excl_irri=max(year2007xavg3_`x'_excl_irri)
label var initial_alt_`x'_excl_irri "Share of `x' (2005 - 2007 average)"
}
*initial share exclude irrigated: ma7
foreach x in barley oats {
gen year2007xavg7_`x'_excl_irri=year2007*(avg7_`x'_planted_acres/cropland_2007_nonirrigated)
bys fips: egen initial_alt2_`x'_excl_irri=max(year2007xavg7_`x'_excl_irri)
label var initial_alt2_`x'_excl_irri "Share of `x' (2001 - 2007 average)"
}
foreach x in corn sorghum soybeans ucotton wheat{
gen year2007xavg7_`x'_excl_irri=year2007*(avg7_`x'_planted_nirri/cropland_2007_nonirrigated)
bys fips: egen initial_alt2_`x'_excl_irri=max(year2007xavg7_`x'_excl_irri)
label var initial_alt2_`x'_excl_irri "Share of `x' (2001 - 2007 average)"
}

*merge with prism
merge 1:1 fips year using "$data/PRISM_1981_2017_apr_sep.dta"
drop _merge

*Tariff and trade 
merge m:1 year using "$data/trains_tariff_nomcont.dta"
drop _merge
merge m:1 year using "$data/trains_tariff_nominit.dta"
drop _merge
merge m:1 year using "$data/trains_tariff_nominit_ma3.dta"
drop _merge
merge m:1 year using "$data/trains_tariff_nominit_ma7.dta"
drop _merge
merge m:1 year using "$data/trains_tariff_relcont.dta"
drop _merge
merge m:1 year using "$data/trains_tariff_relinit.dta"
drop _merge
merge m:1 year using "$data/weights_2011.dta"
drop _merge

*save final
save "$data/us_full.dta", replace

*placebo
use "$data/trains_tariff_nomcont.dta", clear
merge m:1 year using "$data/trains_tariff_nominit.dta"
drop _merge
merge m:1 year using "$data/trains_tariff_relcont.dta"
drop _merge
merge m:1 year using "$data/trains_tariff_relinit.dta"
drop _merge
keep year weightedaverage_* ss_weightedaverage_* rweightedaverage_* ss_rweightedaverage_* totalimport_* ss_totalimport_*
save "$data/trains_placebo.dta", replace



