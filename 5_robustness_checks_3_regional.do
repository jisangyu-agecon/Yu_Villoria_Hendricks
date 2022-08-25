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

*Regional codes
gen region_code=0
*Northeast
replace region_code=1 if stateansi==9 |stateansi==10 | stateansi==23 | stateansi==24 | stateansi==25 | stateansi==33 | stateansi==34 | stateansi==36 | stateansi==42 | stateansi==44 | stateansi==50
*Southeast
replace region_code=2 if stateansi==21 |stateansi==37 | stateansi==47 | stateansi==51 | stateansi==54 | stateansi==1 | stateansi==13 | stateansi==12 | stateansi==45 
*North Central
replace region_code=3 if stateansi==26 |stateansi==18 | stateansi==39 | stateansi==19 | stateansi==27 | stateansi==55 | stateansi==29 | stateansi==17 
*Delta
replace region_code=4 if stateansi==5 |stateansi==22 | stateansi==28 
*Central and Northern Plain
replace region_code=5 if stateansi==31 |stateansi==20 | stateansi==38 | stateansi==46 | stateansi==8 | stateansi==30 | stateansi==56 
*Southern Plains
replace region_code=6 if stateansi==35 | stateansi==40 | stateansi==48
*Far West
replace region_code=7 if stateansi==2 | stateansi==4 | stateansi==6 | stateansi==15 | stateansi==16 | stateansi==32 | stateansi==41 | stateansi==49 | stateansi==53

*main regression
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2, absorb(fips stateyear) vce(cluster stateansi year)
keep if e(sample)==1
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==1, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_regional.tex", nocon keep(cont_tariff_w) ctitle("Northeast") addtext(First stage F, 16.32, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==2, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_regional.tex", nocon keep(cont_tariff_w) ctitle("Southeast") addtext(First stage F, 48.21, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==3, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_regional.tex", nocon keep(cont_tariff_w) ctitle("North Central") addtext(First stage F, 30.78, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==4, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_regional.tex", nocon keep(cont_tariff_w) ctitle("Delta") addtext(First stage F, 41.18, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==5, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_regional.tex", nocon keep(cont_tariff_w) ctitle("Central and Northern Plain") addtext(First stage F, 25.96, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==6, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_regional.tex", nocon keep(cont_tariff_w) ctitle("Southern Plains") addtext(First stage F, 21.85, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe non_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==7, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_regional.tex", nocon keep(cont_tariff_w) ctitle("Far West") addtext(First stage F, 11.39, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append

ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==1, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_ln_regional.tex", nocon keep(cont_tariff_w) ctitle("Northeast") addtext(First stage F, 16.32, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) replace
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==2, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_ln_regional.tex", nocon keep(cont_tariff_w) ctitle("Southeast") addtext(First stage F, 48.21, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==3, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_ln_regional.tex", nocon keep(cont_tariff_w) ctitle("North Central") addtext(First stage F, 30.78, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==4, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_ln_regional.tex", nocon keep(cont_tariff_w) ctitle("Delta") addtext(First stage F, 41.18, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==5, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_ln_regional.tex", nocon keep(cont_tariff_w) ctitle("Central and Northern Plain") addtext(First stage F, 25.96, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==6, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_ln_regional.tex", nocon keep(cont_tariff_w) ctitle("Southern Plains") addtext(First stage F, 21.85, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append
ivreghdfe lnnon_irri_rent_r (cont_tariff_w = ssd_ssd_tariff_w) gdd dday30C prec prec2 if region_code==7, absorb(fips stateyear) cluster(fips year)
outreg2 using "$figure/preliminary_ln_regional.tex", nocon keep(cont_tariff_w) ctitle("Far West") addtext(First stage F, 11.39, Weather Covariates, Yes, County FE, Yes, State-by-year FE, Yes) nonotes nor2 label tex(frag) append



