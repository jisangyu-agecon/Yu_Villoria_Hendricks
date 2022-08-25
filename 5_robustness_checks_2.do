set more off
global data "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/"
global figure "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/draft_tariff_rent/"

use "$data/us_full.dta", clear

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

*weather data
gen gdd=dday10C-dday30C
gen hdd=dday30C
gen prec=prec_apr_sep
gen prec2=prec^2

*generate alternative weather variables
foreach x in gdd dday30C prec prec2{
	tssmooth ma avg_`x'=`x', window(14,1,0) 
}
foreach x in gdd dday30C prec prec2{
	gen dev_`x'=`x'-avg_`x' 
}

*gen state-by-year 
egen stateyear=group(stateansi year)

*label cash rent var "Cash rent, non-irrigated (USD/acre, PPI adjusted, 2017 USD)"
label var non_irri_rent_r "Real Cash Rent"
*logs or levels?
gen lnnon_irri_rent_r=ln(non_irri_rent_r)
label var lnnon_irri_rent_r "ln(Real Cash Rent)"

preserve
*initial production value, export volume, export share over total production value
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
*production value
tssmooth ma avg_prod_`x'=`x'_production_value, window(4,1,0)
*export volume
tssmooth ma avg_export_`x'=imports_`x', window(4,1,0)
*2007 export volume
gen year2007xexport_`x'=year2007*avg_export_`x'*1000
bys fips: egen export2007_`x'=max(year2007xexport_`x')
*export share
gen export_share_`x'=1000*imports_`x'/`x'_production_value
replace export_share_`x'=1 if export_share_`x'>1
tssmooth ma avg_export_share_`x'=export_share_`x', window(4,1,0)
*2007 export share
gen year2007xavg_eshare_`x'=year2007*avg_export_share_`x'
bys fips: egen initial_export_share_`x'=max(year2007xavg_eshare_`x')
*2011 export share
gen year2011xavg_eshare_`x'=year2011*avg_export_share_`x'
bys fips: egen export_share_2011_`x'=max(year2011xavg_eshare_`x')
}

*initial crop share replace values greater than 1 to 1
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
replace initial_share_`x'_excl_irri=1 if initial_share_`x'_excl_irri>1 & initial_share_`x'_excl_irri!=.
}
*tempfile
tempfile temp
save `temp', replace
restore

*Alternative length for the base period: MA3
*initial production value, export volume, export share over total production value
preserve
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
*production value
tssmooth ma avg_prod_`x'=`x'_production_value, window(2,1,0)
*export volume
tssmooth ma avg_export_`x'=imports_`x', window(2,1,0)
*2007 export volume
gen year2007xexport_`x'=year2007*avg_export_`x'*1000
bys fips: egen export2007_`x'=max(year2007xexport_`x')
*export share
gen export_share_`x'=1000*imports_`x'/`x'_production_value
replace export_share_`x'=1 if export_share_`x'>1
tssmooth ma avg_export_share_`x'=export_share_`x', window(2,1,0)
*2007 export share
gen year2007xavg_eshare_`x'=year2007*avg_export_share_`x'
bys fips: egen initial_export_share_`x'=max(year2007xavg_eshare_`x')
}
*initial crop share replace values greater than 1 to 1
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
replace initial_alt_`x'_excl_irri=1 if initial_alt_`x'_excl_irri>1 & initial_alt_`x'_excl_irri!=.
}
****nominal tariff****
*shift-share design
*total acre
foreach x in barley oats {
bys year: egen totalacre_`x'=sum(`x'_planted_acres)
gen year2007xtotalacre_`x'=year2007*totalacre_`x'
bys fips: egen totalacre2007_`x'=max(year2007xtotalacre_`x')
}
foreach x in corn sorghum soybeans ucotton wheat{
bys year: egen totalacresum_`x'=sum(`x'_planted_acres)
bys year: egen totalacreirri_`x'=sum(`x'_planted_acres_irri)
gen totalacre_`x'=totalacresum_`x'-totalacreirri_`x'
replace totalacre_`x'=0 if totalacre_`x'==. | totalacre_`x'<0
gen year2007xtotalacre_`x'=year2007*totalacre_`x'
bys fips: egen totalacre2007_`x'=max(year2007xtotalacre_`x')
}
*barley oats
foreach x in barley oats {
gen cont_share_`x'=(`x'_planted_acres/cropland_2007_nonirrigated)  
replace cont_share_`x'=(`x'_planted_acres/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
replace cont_share_`x'=0 if cont_share_`x'<0 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*weightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_alt_`x'_excl_irri*weightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_alt_`x'_excl_irri*ss_alt_weightedaverage_`x'*initial_export_share_`x'

gen localizedexport_`x'=cont_share_`x'*(193.5*1000*imports_`x'/ppi)/totalacre_`x'
}
*other crops
foreach x in corn sorghum soybeans ucotton wheat{
gen cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2007_nonirrigated)  
replace cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
replace cont_share_`x'=0 if cont_share_`x'<0 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*weightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_alt_`x'_excl_irri*weightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_alt_`x'_excl_irri*ss_alt_weightedaverage_`x'*initial_export_share_`x'

gen localizedexport_`x'=cont_share_`x'*(193.5*1000*imports_`x'/ppi)/totalacre_`x'
}
*localized tariff
egen cont_tariff_w=rowtotal(cont_tariff_w_*) 
label var cont_tariff_w "Contemp. shares, $ {LT_{it}}$"
egen ssd_tariff_w=rowtotal(ssd_tariff_w_*)
label var ssd_tariff_w "Contemp. export and init. crop shares, $\tilde{LT}_{it}$"
egen ssd_ssd_tariff_w=rowtotal(ssd_ssd_tariff_w_*)
label var ssd_ssd_tariff_w "Init. shares, $\bar{LT}_{it}$"
*Robustness check: Alternative time length (MA3)
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_ma3.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_ma3.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LRT}$)") addtext(First stage F, 1747.64, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_ma3.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LRT}$)") addtext(First stage F, 34.80, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_ma3.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_ma3.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma3.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma3.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LRT}$)") addtext(First stage F, 1747.64, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma3.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LRT}$)") addtext(First stage F, 34.80, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma3.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma3.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
restore

*Alternative length for the base period: MA7
*initial production value, export volume, export share over total production value
preserve
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
*production value
tssmooth ma avg_prod_`x'=`x'_production_value, window(6,1,0)
*export volume
tssmooth ma avg_export_`x'=imports_`x', window(6,1,0)
*2007 export volume
gen year2007xexport_`x'=year2007*avg_export_`x'*1000
bys fips: egen export2007_`x'=max(year2007xexport_`x')
*export share
gen export_share_`x'=1000*imports_`x'/`x'_production_value
replace export_share_`x'=1 if export_share_`x'>1
tssmooth ma avg_export_share_`x'=export_share_`x', window(6,1,0)
*2007 export share
gen year2007xavg_eshare_`x'=year2007*avg_export_share_`x'
bys fips: egen initial_export_share_`x'=max(year2007xavg_eshare_`x')
}
*initial crop share replace values greater than 1 to 1
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
replace initial_alt2_`x'_excl_irri=1 if initial_alt2_`x'_excl_irri>1 & initial_alt2_`x'_excl_irri!=.
}
****nominal tariff****
*shift-share design
*total acre
foreach x in barley oats {
bys year: egen totalacre_`x'=sum(`x'_planted_acres)
gen year2007xtotalacre_`x'=year2007*totalacre_`x'
bys fips: egen totalacre2007_`x'=max(year2007xtotalacre_`x')
}
foreach x in corn sorghum soybeans ucotton wheat{
bys year: egen totalacresum_`x'=sum(`x'_planted_acres)
bys year: egen totalacreirri_`x'=sum(`x'_planted_acres_irri)
gen totalacre_`x'=totalacresum_`x'-totalacreirri_`x'
replace totalacre_`x'=0 if totalacre_`x'==. | totalacre_`x'<0
gen year2007xtotalacre_`x'=year2007*totalacre_`x'
bys fips: egen totalacre2007_`x'=max(year2007xtotalacre_`x')
}
*barley oats
foreach x in barley oats {
gen cont_share_`x'=(`x'_planted_acres/cropland_2007_nonirrigated)  
replace cont_share_`x'=(`x'_planted_acres/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
replace cont_share_`x'=0 if cont_share_`x'<0 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*weightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_alt2_`x'_excl_irri*weightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_alt2_`x'_excl_irri*ss_alt2_weightedaverage_`x'*initial_export_share_`x'

gen localizedexport_`x'=cont_share_`x'*(193.5*1000*imports_`x'/ppi)/totalacre_`x'
}
*other crops
foreach x in corn sorghum soybeans ucotton wheat{
gen cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2007_nonirrigated)  
replace cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
replace cont_share_`x'=0 if cont_share_`x'<0 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*weightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_alt2_`x'_excl_irri*weightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_alt2_`x'_excl_irri*ss_alt2_weightedaverage_`x'*initial_export_share_`x'

gen localizedexport_`x'=cont_share_`x'*(193.5*1000*imports_`x'/ppi)/totalacre_`x'
}
*localized tariff
egen cont_tariff_w=rowtotal(cont_tariff_w_*) 
label var cont_tariff_w "Contemp. shares, $ {LT_{it}}$"
egen ssd_tariff_w=rowtotal(ssd_tariff_w_*)
label var ssd_tariff_w "Contemp. export and init. crop shares, $\tilde{LT}_{it}$"
egen ssd_ssd_tariff_w=rowtotal(ssd_ssd_tariff_w_*)
label var ssd_ssd_tariff_w "Init. shares, $\bar{LT}_{it}$"
*Robustness check: Alternative time length (MA7)
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_ma7.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_ma7.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LRT}$)") addtext(First stage F, 1243.14, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_ma7.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LRT}$)") addtext(First stage F, 34.60, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_ma7.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_ma7.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma7.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma7.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LRT}$)") addtext(First stage F, 1243.14, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma7.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LRT}$)") addtext(First stage F, 34.60, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma7.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_ma7.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
restore






*Robustness check: Relative tariff
use `temp', clear
*shift-share design
*total acre
foreach x in barley oats {
bys year: egen totalacre_`x'=sum(`x'_planted_acres)
gen year2007xtotalacre_`x'=year2007*totalacre_`x'
bys fips: egen totalacre2007_`x'=max(year2007xtotalacre_`x')
}
foreach x in corn sorghum soybeans ucotton wheat{
bys year: egen totalacresum_`x'=sum(`x'_planted_acres)
bys year: egen totalacreirri_`x'=sum(`x'_planted_acres_irri)
gen totalacre_`x'=totalacresum_`x'-totalacreirri_`x'
replace totalacre_`x'=0 if totalacre_`x'==. | totalacre_`x'<0
gen year2007xtotalacre_`x'=year2007*totalacre_`x'
bys fips: egen totalacre2007_`x'=max(year2007xtotalacre_`x')
}
*barley oats
foreach x in barley oats {
gen cont_share_`x'=(`x'_planted_acres/cropland_2007_nonirrigated)  
replace cont_share_`x'=(`x'_planted_acres/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
replace cont_share_`x'=0 if cont_share_`x'<0 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*rweightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_share_`x'_excl_irri*rweightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_share_`x'_excl_irri*ss_rweightedaverage_`x'*initial_export_share_`x'

gen localizedexport_`x'=cont_share_`x'*(193.5*1000*imports_`x'/ppi)/totalacre_`x'
}
*other crops
foreach x in corn sorghum soybeans ucotton wheat{
gen cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2007_nonirrigated)  
replace cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
replace cont_share_`x'=0 if cont_share_`x'<0 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*rweightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_share_`x'_excl_irri*rweightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_share_`x'_excl_irri*ss_rweightedaverage_`x'*initial_export_share_`x'

gen localizedexport_`x'=cont_share_`x'*(193.5*1000*imports_`x'/ppi)/totalacre_`x'
}
*localized tariff
egen cont_tariff_w=rowtotal(cont_tariff_w_*) 
label var cont_tariff_w "Contemp. shares, $ {LRT_{it}}$"
egen ssd_tariff_w=rowtotal(ssd_tariff_w_*)
label var ssd_tariff_w "Contemp. export and init. crop shares, $\tilde{LRT}_{it}$"
egen ssd_ssd_tariff_w=rowtotal(ssd_ssd_tariff_w_*)
label var ssd_ssd_tariff_w "Init. shares, $\bar{LRT}_{it}$"

*figure of tariffs by year
preserve
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi)
keep if e(sample)==1
keep year non_irri_rent_r cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w
drop if non_irri_rent_r==. 

graph box cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w, over(year) nooutsides ytitle("Localized Relative Tariff (%)", size(small)) ///
legend(label(1 "Contemp. export and" "crop shares") label(2 "Contemp. export and" "intial crop shares") label(3 "Initial export and" "crop shares") size(vsmall)) note("") scale(1.2) graphregion(color(white))
graph export "$figure/rel_tariff_boxplot.png", replace
restore

*Robustness check: Relative tariff
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_relative.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_relative.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LRT}$)") addtext(First stage F, 77.58, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_relative.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LRT}$)") addtext(First stage F, 1.52, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_relative.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_relative.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_relative.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_relative.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LRT}$)") addtext(First stage F, 77.58, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_relative.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LRT}$)") addtext(First stage F, 1.52, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_relative.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_relative.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append


*Robustness check: No imputation
use "$data/us_full_noimpute.dta", clear

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

*weather data
gen gdd=dday10C-dday30C
gen hdd=dday30C
gen prec=prec_apr_sep
gen prec2=prec^2

*gen state-by-year 
egen stateyear=group(stateansi year)

*label cash rent var "Cash rent, non-irrigated (USD/acre, PPI adjusted, 2017 USD)"
label var non_irri_rent_r "Real Cash Rent"
*logs or levels?
gen lnnon_irri_rent_r=ln(non_irri_rent_r)
label var lnnon_irri_rent_r "ln(Real Cash Rent)"

*initial production value, export volume, export share over total production value
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
*production value
tssmooth ma avg_prod_`x'=`x'_production_value, window(4,1,0)
*export volume
tssmooth ma avg_export_`x'=imports_`x', window(4,1,0)
*2007 export volume
gen year2007xexport_`x'=year2007*avg_export_`x'*1000
bys fips: egen export2007_`x'=max(year2007xexport_`x')
*export share
gen export_share_`x'=1000*imports_`x'/`x'_production_value
replace export_share_`x'=1 if export_share_`x'>1
tssmooth ma avg_export_share_`x'=export_share_`x', window(4,1,0)
*2007 export share
gen year2007xavg_eshare_`x'=year2007*avg_export_share_`x'
bys fips: egen initial_export_share_`x'=max(year2007xavg_eshare_`x')
*2011 export share
gen year2011xavg_eshare_`x'=year2011*avg_export_share_`x'
bys fips: egen export_share_2011_`x'=max(year2011xavg_eshare_`x')
}

*initial crop share replace values greater than 1 to 1
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
replace initial_share_`x'_excl_irri=1 if initial_share_`x'_excl_irri>1 & initial_share_`x'_excl_irri!=.
}
*tempfile
tempfile temp
save `temp', replace

****nominal tariff****
*shift-share design
*total acre
foreach x in barley oats {
bys year: egen totalacre_`x'=sum(`x'_planted_acres)
gen year2007xtotalacre_`x'=year2007*totalacre_`x'
bys fips: egen totalacre2007_`x'=max(year2007xtotalacre_`x')
}
foreach x in corn sorghum soybeans ucotton wheat{
bys year: egen totalacresum_`x'=sum(`x'_planted_acres)
bys year: egen totalacreirri_`x'=sum(`x'_planted_acres_irri)
gen totalacre_`x'=totalacresum_`x'-totalacreirri_`x'
replace totalacre_`x'=0 if totalacre_`x'==. | totalacre_`x'<0
gen year2007xtotalacre_`x'=year2007*totalacre_`x'
bys fips: egen totalacre2007_`x'=max(year2007xtotalacre_`x')
}
*barley oats
foreach x in barley oats {
gen cont_share_`x'=(`x'_planted_acres/cropland_2007_nonirrigated)  
replace cont_share_`x'=(`x'_planted_acres/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
replace cont_share_`x'=0 if cont_share_`x'<0 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*weightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_share_`x'_excl_irri*weightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_share_`x'_excl_irri*ss_weightedaverage_`x'*initial_export_share_`x'

gen localizedexport_`x'=cont_share_`x'*(193.5*1000*imports_`x'/ppi)/totalacre_`x'
}
*other crops
foreach x in corn sorghum soybeans ucotton wheat{
gen cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2007_nonirrigated)  
replace cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
replace cont_share_`x'=0 if cont_share_`x'<0 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*weightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_share_`x'_excl_irri*weightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_share_`x'_excl_irri*ss_weightedaverage_`x'*initial_export_share_`x'

gen localizedexport_`x'=cont_share_`x'*(193.5*1000*imports_`x'/ppi)/totalacre_`x'
}
*localized tariff
egen cont_tariff_w=rowtotal(cont_tariff_w_*) 
label var cont_tariff_w "Contemp. shares, $ {LT_{it}}$"
egen ssd_tariff_w=rowtotal(ssd_tariff_w_*)
label var ssd_tariff_w "Contemp. export and init. crop shares, $\tilde{LT}_{it}$"
egen ssd_ssd_tariff_w=rowtotal(ssd_ssd_tariff_w_*)
label var ssd_ssd_tariff_w "Init. shares, $\bar{LT}_{it}$"

*localized export
egen localizedexport=rowtotal(localizedexport_*)
label var localizedexport "Localized Export Exposure"

*main regression I - nominal tariff
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
keep if e(sample)==1
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_noimpute.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_noimpute.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1865.97, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_noimpute.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 87.03, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_noimpute.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_noimpute.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append


reghdfe lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_noimpute.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_noimpute.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1865.97, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_noimpute.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 87.03, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_noimpute.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi)
outreg2 using "$figure/preliminary_ln_nominal_noimpute.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

