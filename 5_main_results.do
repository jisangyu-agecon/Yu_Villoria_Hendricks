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

*Figure of tariffs by year
preserve
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi)
keep if e(sample)==1
keep year non_irri_rent_r cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w
drop if non_irri_rent_r==. 

graph box cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w, over(year) nooutsides ytitle("Localized Nominal Tariff (ad valorem %)", size(small)) ///
legend(label(1 "Contemp. export and" "crop shares") label(2 "Contemp. export and" "intial crop shares") label(3 "Initial export and" "crop shares") size(vsmall)) note("") scale(1.2) graphregion(color(white))
graph export "$figure/tariff_boxplot.png", replace
restore

*main regression
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
keep if e(sample)==1
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1410.21, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 40.91, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_nominal.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

reghdfe lnnon_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\tilde{LT}$)") addtext(First stage F, 1410.21, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/preliminary_ln_nominal.tex", nocon keep(cont_tariff_w) ctitle("FE-IV ($\bar{LT}$)") addtext(First stage F, 40.91, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal.tex", nocon keep(ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
outreg2 using "$figure/preliminary_ln_nominal.tex", nocon keep(ssd_ssd_tariff_w) ctitle("FE") addtext(First stage F, NA, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

*First stage reg
reghdfe cont_tariff_w ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/first_stage.tex", nocon keep(ssd_tariff_w) ctitle("First stage ($\tilde{LT}$)") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
reghdfe cont_tariff_w ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) cluster(stateansi year)
outreg2 using "$figure/first_stage.tex", nocon keep(ssd_ssd_tariff_w) ctitle("First stage ($\tilde{LT}$)") addtext(Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

*counterfactual (no trade diversion)
*Chinese initial shares (B: 0, Corn: .0003264, Cotton: .3889184, Sorghum: 0, Oats: .0000732, Soybeans: .4153036, Wheat: .039476)
gen china_effect_corn=25*initial_export_share_corn*.0003264 if year==2017
gen china_effect_ucotton=25*initial_export_share_ucotton*.3889184 if year==2017
gen china_effect_oats=10*initial_export_share_oats*.0000732 if year==2017
gen china_effect_soybeans=25*initial_export_share_soybeans*.4153036 if year==2017
gen china_effect_wheat=25*initial_export_share_wheat*.039476 if year==2017 
summ china_effect*

reghdfe non_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
estimates save "$data/main_ssd_ssd", replace
gen cd_ssd_tariff_w=initial_share_corn*25*initial_export_share_corn*.0003264+ ///
initial_share_ucotton*25*initial_export_share_ucotton*.3889184+ ///
initial_share_oats*10*initial_export_share_oats*.0000732+ /// 
initial_share_soybeans*25*initial_export_share_soybeans*.4153036+ ///
initial_share_wheat*25*initial_export_share_wheat*.039476 if year==2017
est use "$data/main_ssd_ssd"
gen delta1=-cd_ssd_tariff_w*_b[ssd_ssd_tariff_w]
reghdfe lnnon_irri_rent_r ssd_ssd_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
estimates save "$data/main_ssd_ssd_ln", replace
est use "$data/main_ssd_ssd_ln"
gen delta1_ln=-100*cd_ssd_tariff_w*_b[ssd_ssd_tariff_w]

preserve
keep if non_irri_rent_r!=. & year==2017
gen N=_N
keep delta* N
label var delta1 "Real reduction"
label var delta1_ln "Percentage reduction"
summ delta*
outreg2 using "$figure/counterfactual_summ.tex", replace tex(frag) sum(log) eqkeep(mean sd min max) label
restore

*maps
preserve
cd "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/us_county_shape"
keep if year==2017
keep if non_irri_rent_r!=. & fips!=.
merge 1:1 fips using cb_2017_us_county_20m.dta

format delta1 delta1_ln %12.2f

spmap delta1 using "cb_2017_us_county_20m_shp.dta", id(fips)  clm(quantile) cln(5)  ///
 fcolor(Heat) legend(pos(4)) ///
ndf(white) ndl("no data") title("Cash rent, predicted reduction", size(small))
gr export "$figure/delta1_nominal.eps", replace

spmap delta1_ln using "cb_2017_us_county_20m_shp.dta", id(fips)  clm(quantile) cln(5)  ///
 fcolor(Heat) legend(pos(4)) ///
ndf(white) ndl("no data") title("Cash rent, predicted % reduction", size(small))
gr export "$figure/delta1_ln.eps", replace
restore




