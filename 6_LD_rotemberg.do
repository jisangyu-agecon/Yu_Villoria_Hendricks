set more off
set matsize 2000

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

gen shock_`x'=ss_weightedaverage_`x'*initial_export_share_`x'

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

gen shock_`x'=ss_weightedaverage_`x'*initial_export_share_`x'

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

foreach x in barley oats corn sorghum soybeans ucotton wheat{
tssmooth ma avg5_shock_`x'=shock_`x', window(4 1 0)
}

*share times prices
foreach x in barley oats corn sorghum soybeans ucotton wheat {
tssmooth ma avg5_price_`x'=`x'_price_r, window(4 1 0)
tssmooth ma avg5_cshare_`x'=cont_share_`x', window(4 1 0)
}
foreach x in barley oats corn sorghum soybeans ucotton wheat {
gen avg5_csharexprice_`x'=cont_share_`x'*avg5_price_`x'
}

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

foreach x in barley oats corn sorghum soybeans ucotton wheat{
gen d_avg5_shock_`x'=d.avg5_shock_`x'
}

foreach x in barley oats corn sorghum soybeans ucotton wheat {
gen d_cont_sharexprice_`x'=d.avg5_csharexprice_`x'
}
label var d_non_irri_rent_r  "Real cash rent"
label var d_cont_tariff_w "Contemp. shares, $ {LT_{it}}$"
label var d_ssd_tariff_w "Contemp. export and init. crop shares, $\tilde{LT}_{it}$"
label var d_ssd_ssd_tariff_w "Init. shares, $\bar{LT}_{it}$"

*LD
keep if year==2017
ivreghdfe d_non_irri_rent_r (d_cont_tariff_w = d_ssd_ssd_tariff_w) d_gdd d_dday30C d_prec d_prec2, absorb(stateansi) cluster(stateansi)
keep if e(sample)==1

*Rotemberg
bartik_weight, z(initial_share_*_excl_irri) weightstub(d_avg5_shock_*) x(d_cont_tariff_w) y(d_non_irri_rent_r) controls(d_gdd d_dday30C d_prec d_prec2) absorb(stateansi)

mat beta = r(beta)
mat alpha = r(alpha)
mat gamma = r(gam)
mat pi = r(pi)
mat G = r(G)
desc initial_share_*_excl_irri, varlist
local varlist = r(varlist)

matlist alpha
matlist beta

