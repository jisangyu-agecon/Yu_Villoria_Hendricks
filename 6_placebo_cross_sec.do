set more off
global data "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/"
global figure "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/draft_tariff_rent/"

use "$data/us_full.dta", clear

*2007 crops
gen share2007=planted_2007_nonirrigated/cropland_2007_nonirrigated
summ share2007 if non_irri_rent_r!=., detail

*use 2008 - 2017
drop if year>2017

*year2011 dummy
gen year2011=0
replace year2011=1 if year==2011

*drop counties with only one year of observation
gen cashrent_report=0
replace cashrent_report=1 if non_irri_rent!=. & year>2007
bys fips: egen ever_report_cashrent=sum(cashrent_report)
drop if ever_report_cashrent<2

*initial export share
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
gen export_share_`x'=1000*imports_`x'/`x'_production_value
replace export_share_`x'=1 if export_share_`x'>1

tssmooth ma avg_export_share_`x'=export_share_`x', window(4,1,0)

gen year2007xavg_eshare_`x'=year2007*avg_export_share_`x'
bys fips: egen initial_export_share_`x'=max(year2007xavg_eshare_`x')

gen year2011xavg_eshare_`x'=year2011*avg_export_share_`x'
bys fips: egen export_share_2011_`x'=max(year2011xavg_eshare_`x')
}

*initial crop share replace values greater than 1 to 1
foreach x in barley oats corn sorghum soybeans ucotton wheat{
replace initial_share_`x'_excl_irri=1 if initial_share_`x'_excl_irri>1 & initial_share_`x'_excl_irri!=.
}

*weather data
gen gdd=dday10C-dday30C
gen hdd=dday30C
gen prec=prec_apr_sep
gen prec2=prec^2

*gen state-by-year 
egen stateyear=group(stateansi year)

*label cash rent var "Cash rent, non-irrigated (USD/acre, PPI adjusted, 1982=100)"
label var non_irri_rent_r "Real Cash Rent"

*crop shares...
*exclude irrigated crops
foreach x in barley oats {
gen cont_share_`x'=(`x'_planted_acres/cropland_2007_nonirrigated)  
replace cont_share_`x'=(`x'_planted_acres/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
}

foreach x in corn sorghum soybeans ucotton wheat {
gen cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2007_nonirrigated)  
replace cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
}

*save data for placebo
preserve
keep fips year initial_share_*_excl_irri
collapse initial_share_*_excl_irri, by(fips)
save "$data/placebo_initial_shares.dta", replace
restore

drop initial_share_*_excl_irri
save "$data/us_full_placebo_basedata_cross_sec.dta", replace

*placebo simulation
clear all

program placebo_cross
use "$data/placebo_initial_shares.dta", clear
sort fips
gen tempid=_n
preserve
*Create a shuffled version of your variable
keep initial_share_*_excl_irri tempid
gen double rand = runiform()
sort rand
replace tempid = _n
drop rand
tempfile temp
save `temp'
restore
keep fips tempid
merge 1:1 tempid using `temp'
drop _merge
tempfile temp2
save `temp2' 

*Merge shuffled variable onto original
use "$data/us_full_placebo_basedata_cross_sec.dta", clear
merge m:1 fips using `temp2'

*localized tariff exposure
foreach x in barley oats corn sorghum soybeans ucotton wheat {
gen ssd_ssd_tariff_w_`x'=initial_share_`x'_excl_irri*ss_weightedaverage_`x'*initial_export_share_`x'
}
*localized tariff
egen ssd_ssd_tariff_w=rowtotal(ssd_ssd_tariff_w_*)

reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
end

simulate _b[ssd_ssd_tariff_w] _se[ssd_ssd_tariff_w], reps(1000) seed(123123): placebo_cross
rename _sim_1 beta
rename _sim_2 stderror
gen tstat=beta/stderror


label var beta "Coefficient"

sum, detail

save "$data/placebo_sim_data_cross_sec.dta", replace








