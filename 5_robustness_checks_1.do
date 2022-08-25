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
xtset fips year
foreach x in gdd dday30C prec prec2{
	tssmooth ma avg_`x'=`x', window(14,1,0) 
}
foreach x in gdd dday30C prec prec2{
	gen dev_`x'=l.`x'-avg_`x' 
}

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

*Robustness check: Include the lagged dep. var
xtset fips year
xi: xtabond non_irri_rent_r cont_tariff_w gdd dday30C prec prec2 i.stateyear, vce(robust)
outreg2 using "$figure/preliminary_nominal_xtabond.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
xi: xtabond non_irri_rent_r gdd dday30C prec prec2 i.stateyear, inst(ssd_tariff_w) end(cont_tariff_w) vce(robust)
outreg2 using "$figure/preliminary_nominal_xtabond.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append 
xi: xtabond non_irri_rent_r gdd dday30C prec prec2 i.stateyear, inst(ssd_ssd_tariff_w) end(cont_tariff_w) vce(robust)
outreg2 using "$figure/preliminary_nominal_xtabond.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append 
xi: xtabond non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2 i.stateyear, vce(robust)
outreg2 using "$figure/preliminary_nominal_xtabond.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append 
xi: xtabond non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2 i.stateyear, vce(robust)
outreg2 using "$figure/preliminary_nominal_xtabond.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append 

xi: xtabond lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2 i.stateyear, vce(robust)
outreg2 using "$figure/preliminary_ln_nominal_xtabond.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
xi: xtabond lnnon_irri_rent_r gdd dday30C prec prec2 i.stateyear, inst(ssd_tariff_w) end(cont_tariff_w) vce(robust)
outreg2 using "$figure/preliminary_ln_nominal_xtabond.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append 
xi: xtabond lnnon_irri_rent_r gdd dday30C prec prec2 i.stateyear, inst(ssd_ssd_tariff_w) end(cont_tariff_w) vce(robust)
outreg2 using "$figure/preliminary_ln_nominal_xtabond.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append 
xi: xtabond lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2 i.stateyear, vce(robust)
outreg2 using "$figure/preliminary_ln_nominal_xtabond.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append 
xi: xtabond lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2 i.stateyear, vce(robust)
outreg2 using "$figure/preliminary_ln_nominal_xtabond.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append 

*Robustnes check: Lagged independent variable
preserve
xtset fips year
gen lagcont_tariff_w=l.cont_tariff_w
gen lagssd_tariff_w=l.ssd_tariff_w
gen lagssd_ssd_tariff_w=l.ssd_ssd_tariff_w
replace cont_tariff_w=lagcont_tariff_w
replace ssd_tariff_w=lagssd_tariff_w
replace ssd_ssd_tariff_w=lagssd_ssd_tariff_w
drop if year<2009
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
keep if e(sample)==1
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_lagnominal.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_lagnominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1410.21, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_lagnominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 40.91, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_lagnominal.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_lagnominal.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_lagnominal.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_lagnominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1410.21, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_lagnominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 40.91, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_lagnominal.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_lagnominal.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
restore

*Robustness check: State-specific trend
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(stateyear fips) vce(cluster stateansi year)
keep if e(sample)==1
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_nostateyearfe.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_nostateyearfe.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F,1833.71, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_nostateyearfe.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 2.92, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_nostateyearfe.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_nostateyearfe.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_nostateyearfe.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_nostateyearfe.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1833.71, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_nostateyearfe.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 2.92, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_nostateyearfe.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2 i.stateansi#c.trend i.stateansi#c.trend2, absorb(fips) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_nostateyearfe.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, No) nonotes nor2 label tex(frag) append

*Robustness check: Nominal cash rent
label var non_irri_rent "Nominal Cash Rent"
*logs
gen lnnon_irri_rent=ln(non_irri_rent)
label var lnnon_irri_rent "ln(Nominal Cash Rent)"
reghdfe non_irri_rent cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1410.21, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 40.91, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_nominal.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_nominal.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_lnnominal_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_lnnominal_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1410.21, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_lnnominal_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 40.91, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_lnnominal_nominal.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_lnnominal_nominal.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

*Robustness check: Different weather variables
reghdfe non_irri_rent_r cont_tariff_w dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_alt_weather.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_alt_weather.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1421.91, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_alt_weather.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 41.16, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_alt_weather.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_alt_weather.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent_r cont_tariff_w dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_alt_weather.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_alt_weather.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1421.91, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_alt_weather.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 41.16, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_alt_weather.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w dev_gdd dev_dday30C dev_prec dev_prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_alt_weather.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

*Robustness check: Exclude 2008
preserve
keep if year>2008
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
keep if e(sample)==1
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_no08.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_no08.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 836.88, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal_no08.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 38.41, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_no08.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal_no08.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_no08.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_no08.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 836.88, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_no08.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 38.41, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_no08.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal_no08.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
restore

*Robustness check: LD
xtset fips year
tssmooth ma avg5_non_irri_rent_r=non_irri_rent_r, window(4 1 0)
tssmooth ma avg5_cont_tariff_w=cont_tariff_w, window(4 1 0)
tssmooth ma avg5_ssd_tariff_w=ssd_tariff_w, window(4 1 0)
tssmooth ma avg5_ssd_ssd_tariff_w=ssd_ssd_tariff_w, window(4 1 0)
tssmooth ma avg5_gdd=gdd, window(4 1 0)
tssmooth ma avg5_dday30C=dday30C, window(4 1 0)
tssmooth ma avg5_prec=prec, window(4 1 0)
tssmooth ma avg5_prec2=prec2, window(4 1 0)
*share times prices
foreach x in barley oats corn sorghum soybeans ucotton wheat {
tssmooth ma avg5_price_`x'=`x'_price_r, window(4 1 0)
tssmooth ma avg5_cshare_`x'=cont_share_`x', window(4 1 0)
}
foreach x in barley oats corn sorghum soybeans ucotton wheat {
gen avg5_csharexprice_`x'=cont_share_`x'*avg5_price_`x'
}

preserve
keep if year==2012 | year==2017
gen t=1
replace t=2 if year==2017

xtset fips t
gen d_non_irri_rent_r=d.avg5_non_irri_rent_r
gen d_cont_tariff_w=d.avg5_cont_tariff_w
gen d_ssd_tariff_w=d.avg5_ssd_tariff_w
gen d_ssd_ssd_tariff_w=d.avg5_ssd_ssd_tariff_w
gen d_gdd=d.avg5_gdd
gen d_dday30C=d.avg5_dday30C
gen d_prec=d.avg5_prec
gen d_prec2=d.avg5_prec2

foreach x in barley oats corn sorghum soybeans ucotton wheat {
gen d_cont_sharexprice_`x'=d.avg5_csharexprice_`x'
}
label var d_non_irri_rent_r  "Real cash rent"
label var d_cont_tariff_w "Contemp. shares, $ {LT_{it}}$"
label var d_ssd_tariff_w "Contemp. export and init. crop shares, $\tilde{LT}_{it}$"
label var d_ssd_ssd_tariff_w "Init. shares, $\bar{LT}_{it}$"
*LD
reghdfe d_non_irri_rent_r d_cont_tariff_w d_gdd d_dday30C d_prec d_prec2 if year==2017, absorb(stateansi) cluster(stateansi)
outreg2 using "$figure/preliminary_LD.tex", replace nocon ctitle("FE") keep(d_cont_tariff_w) addtext(First stage F, NA, Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
ivreghdfe d_non_irri_rent_r (d_cont_tariff_w = d_ssd_tariff_w) d_gdd d_dday30C d_prec d_prec2 if year==2017, absorb(stateansi) cluster(stateansi)
outreg2 using "$figure/preliminary_LD.tex", append nocon ctitle("FE-IV ($\tilde{LT}$)") keep(d_cont_tariff_w) addtext(First stage F, 2028.51, Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
ivreghdfe d_non_irri_rent_r (d_cont_tariff_w = d_ssd_ssd_tariff_w) d_gdd d_dday30C d_prec d_prec2 if year==2017, absorb(stateansi) cluster(stateansi)
outreg2 using "$figure/preliminary_LD.tex", append nocon ctitle("FE-IV ($\bar{LT}$)") keep(d_cont_tariff_w) addtext(First stage F, 523.65, Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
reghdfe d_non_irri_rent_r d_ssd_tariff_w d_gdd d_dday30C d_prec d_prec2 if year==2017, absorb(stateansi) cluster(stateansi)
outreg2 using "$figure/preliminary_LD.tex", append nocon ctitle("FE") keep(d_ssd_tariff_w) addtext(First stage F, NA, Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
reghdfe d_non_irri_rent_r d_ssd_ssd_tariff_w d_gdd d_dday30C d_prec d_prec2 if year==2017, absorb(stateansi) cluster(stateansi)
outreg2 using "$figure/preliminary_LD.tex", append nocon ctitle("FE") keep(d_ssd_ssd_tariff_w) addtext(First stage F, NA, Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
restore

*Robustnes check: Shift-share approach
*generate differences in tariffs from the base periods
foreach x in barley oats corn sorghum soybeans ucotton wheat{
gen wxdomestic_`x'=ss11_weightedaverage_`x'*export_share_2011_`x'
tssmooth ma ma_w_`x'=wxdomestic_`x', window(2,1,0)
tssmooth ma avg3_2011_share_`x'=cont_share_`x', window(2,1,0)
gen year2011xw_`x'=year2011*ma_w_`x'
gen year2011xshare_`x'=year2011*avg3_2011_share_`x'

bys fips: egen year2011_w_`x'=max(year2011xw_`x')
bys fips: egen share2011_`x'=max(year2011xshare_`x')

gen diff_w_`x'=wxdomestic_`x'-year2011_w_`x'
gen ssd_diff_tariff_w_`x'=share2011_`x'*diff_w_`x'
}
egen ssd_diff_tariff_w=rowtotal(ssd_diff_tariff_w_*)

*gen diffs from the 2011 levels
tssmooth ma ma_non_irri_rent_r=non_irri_rent_r, window(2,1,0)
gen year2011xrent=year2011*ma_non_irri_rent_r
bys fips: egen year2011_rent=max(year2011xrent)

tssmooth ma ma_gdd=gdd, window(2,1,0)
gen year2011xgdd=year2011*ma_gdd
bys fips: egen year2011_gdd=max(year2011xgdd)

tssmooth ma ma_dday30C=dday30C, window(2,1,0)
gen year2011xdday30C=year2011*ma_dday30C
bys fips: egen year2011_dday30C=max(year2011xdday30C)

tssmooth ma ma_prec=prec, window(2,1,0)
gen year2011xprec=year2011*ma_prec
bys fips: egen year2011_prec=max(year2011xprec)

tssmooth ma ma_prec2=prec2, window(2,1,0)
gen year2011xprec2=year2011*ma_prec2
bys fips: egen year2011_prec2=max(year2011xprec2)

drop if year2011_rent==. | year2011_rent==0

gen diff_rent=non_irri_rent_r-year2011_rent
gen diff_gdd=gdd-year2011_gdd
gen diff_dday30C=dday30C-year2011_dday30C
gen diff_prec=prec-year2011_prec
gen diff_prec2=prec2-year2011_prec2

label var diff_rent "Rent Change"
label var ssd_diff_tariff_w "Tariff Shock"

keep if year>2011
areg diff_rent ssd_diff_tariff_w diff_gdd diff_dday30C diff_prec diff_prec2 if year==2012, absorb(stateansi) vce(cluster stateansi)
outreg2 using "$figure/preliminary_SSD.tex", replace nocon ctitle("Year 2012") keep(ssd_diff_tariff_w) addtext(Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
areg diff_rent ssd_diff_tariff_w diff_gdd diff_dday30C diff_prec diff_prec2 if year==2013, absorb(stateansi) vce(cluster stateansi)
outreg2 using "$figure/preliminary_SSD.tex", append nocon ctitle("Year 2013") keep(ssd_diff_tariff_w) addtext(Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
areg diff_rent ssd_diff_tariff_w diff_gdd diff_dday30C diff_prec diff_prec2 if year==2014, absorb(stateansi) vce(cluster stateansi)
outreg2 using "$figure/preliminary_SSD.tex", append nocon ctitle("Year 2014") keep(ssd_diff_tariff_w) addtext(Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
areg diff_rent ssd_diff_tariff_w diff_gdd diff_dday30C diff_prec diff_prec2 if year==2016, absorb(stateansi) vce(cluster stateansi)
outreg2 using "$figure/preliminary_SSD.tex", append nocon ctitle("Year 2016") keep(ssd_diff_tariff_w) addtext(Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)
areg diff_rent ssd_diff_tariff_w diff_gdd diff_dday30C diff_prec diff_prec2 if year==2017, absorb(stateansi) vce(cluster stateansi)
outreg2 using "$figure/preliminary_SSD.tex", append nocon ctitle("Year 2017") keep(ssd_diff_tariff_w) addtext(Weather Covariates, Yes, State FE, Yes) nonotes nor2 label tex(frag)



