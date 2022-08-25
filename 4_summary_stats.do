set more off
global data "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/"
global figure "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/draft_tariff_rent/"

use "$data/us_full.dta", clear

*use 2008 - 2017
drop if year>2017

*drop counties with only one year of observation
gen cashrent_report=0
replace cashrent_report=1 if non_irri_rent!=. & year>2007
bys fips: egen ever_report_cashrent=sum(cashrent_report)
drop if ever_report_cashrent<2

*initial export share
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
gen export_share_`x'=1000*imports_`x'/`x'_production_value

tssmooth ma avg_export_share_`x'=export_share_`x', window(4,1,0)

gen year2007xavg_eshare_`x'=year2007*avg_export_share_`x'
bys fips: egen initial_export_share_`x'=max(year2007xavg_eshare_`x')
}

*initial crop share replace values greater than 1 to 1
foreach x in barley oats  corn sorghum soybeans ucotton wheat{
replace initial_share_`x'_excl_irri=1 if initial_share_`x'_excl_irri>1 & initial_share_`x'_excl_irri!=.
}

*shift-share design
*exclude irrigated crops
foreach x in barley oats {
gen cont_share_`x'=(`x'_planted_acres/cropland_2007_nonirrigated)  
replace cont_share_`x'=(`x'_planted_acres/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*weightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_share_`x'_excl_irri*weightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_share_`x'_excl_irri*ss_weightedaverage_`x'*initial_export_share_`x'
}

foreach x in corn sorghum soybeans ucotton wheat{
gen cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2007_nonirrigated)  
replace cont_share_`x'=((`x'_planted_acres-`x'_planted_acres_irri)/cropland_2012_nonirrigated) if year>2011
replace cont_share_`x'=1 if cont_share_`x'>1 & cont_share_`x'!=.
label var cont_share_`x' "Share of `x'"

gen cont_tariff_w_`x'=cont_share_`x'*weightedaverage_`x'*export_share_`x'
gen ssd_tariff_w_`x'=initial_share_`x'_excl_irri*weightedaverage_`x'*export_share_`x'
gen ssd_ssd_tariff_w_`x'=initial_share_`x'_excl_irri*ss_weightedaverage_`x'*initial_export_share_`x'
}


*localized tariff
egen cont_tariff_w=rowtotal(cont_tariff_w_*) 
label var cont_tariff_w "Contemp. shares, $ {LT_{it}}$"
egen ssd_tariff_w=rowtotal(ssd_tariff_w_*)
label var ssd_tariff_w "Contemp. export and init. crop shares, $\tilde{LT}_{it}$"
egen ssd_ssd_tariff_w=rowtotal(ssd_ssd_tariff_w_*)
label var ssd_ssd_tariff_w "Init. shares, $\bar{LT}_{it}$"

*weather data
gen gdd=dday10C-dday30C
gen hdd=dday30C
gen prec=prec_apr_sep
gen prec2=prec^2

* Figure of tariffs by year
preserve
keep year non_irri_rent_r cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w
drop if non_irri_rent_r==. 
drop non_irri_rent_r
bys year: gen N=_N
bysort year: summ cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w

grstyle init
grstyle set plain, horizontal compact minor
grstyle set legend 4, nobox klength(small)
grstyle set color hue, n(3) opacity(50)
graph box cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w, over(year) nooutsides ytitle("Localized Tariff", size(small)) ///
legend(label(1 "Contemp. export and" "crop shares") label(2 "Contemp. export and" "intial crop shares") label(3 "Initial export and" "crop shares") size(vsmall)) note("") scale(1.2)
graph export "$figure/tariff_boxplot.png", replace
restore

***maps***
preserve
cd "$data/us_county_shape"
use cb_2017_us_county_20m_shp.dta, clear
keep if _X>-100
save cb_2017_us_county_20m_dryland_shp.dta, replace
restore

preserve
cd "$data/us_county_shape"
keep if year==2007
merge 1:1 fips using cb_2017_us_county_20m.dta

gen share2007=planted_2007_nonirrigated/cropland_2007_nonirrigated

format share2007 %12.2f
spmap share2007 using "cb_2017_us_county_20m_shp.dta", id(fips)  clm(quantile) cln(5)  ///
 fcolor(Greens) legend(pos(4)) ///
ndf(white) ndl("no data") title("Share of the seven field crops, 2007", size(small))
gr export "$figure/share2007.png", replace
restore

preserve
cd "$data/us_county_shape"
collapse (mean) non_irri_rent_r cont_tariff_w ssd_tariff_w, by(fips)
keep if non_irri_rent_r!=.
merge 1:1 fips using cb_2017_us_county_20m.dta

format non_irri_rent_r %12.2f
spmap non_irri_rent_r using "cb_2017_us_county_20m_shp.dta", id(fips)  clm(quantile) cln(5)  ///
 fcolor(Greens) legend(pos(4) size(medium)) ///
ndf(white) ndl("no data")
gr export "$figure/non_irrigated_avg_08_17.eps", replace
restore

preserve
cd "$data/us_county_shape"
keep if non_irri_rent_r!=.

tssmooth ma non_irri_rent_r_ma=non_irri_rent_r, window(4 1 0)
tssmooth ma ssd_tariff_w_ma=ssd_tariff_w, window(4 1 0)
tssmooth ma cont_tariff_w_ma=cont_tariff_w, window(4 1 0)

keep if year==2012 | year==2017
gen time=1
replace time=2 if year==2017
xtset fips time
gen d_rent=(d.non_irri_rent_r_ma/l.non_irri_rent_r_ma)*100
gen d_ss_tariff=(d.ssd_tariff_w_ma)
gen d_c_tariff=(d.cont_tariff_w_ma)

*keep cross-sectional
keep if time==2
merge 1:1 fips using cb_2017_us_county_20m.dta

bys state: summ d_rent

format d_rent %12.2f
spmap d_rent using "cb_2017_us_county_20m_shp.dta", id(fips)  clm(quantile) cln(5)  ///
 fcolor(Greens) legend(pos(4) size(medium)) ///
ndf(white) ndl("no data")
gr export "$figure/non_irrigated_change_08_17.eps", replace
restore

*summary stats: rent
preserve
keep if year>2007 & year<2018
drop if year==2015

grstyle clear
grstyle init
grstyle set plain, horizontal compact minor
grstyle set legend 4, nobox klength(small)
grstyle set color hue, n(3) opacity(50)
graph box non_irri_rent non_irri_rent_r, over(year) nooutsides ///
graphregion(color(white)) ytitle("Cash Rent (USD/acre)", size(small)) legend(label(1 "Nominal") label(2 "Real (2017 USD)") size(vsmall))  note("") scale(1.2)
gr export "$figure/cash_rent_trend.eps", replace
restore

preserve
keep if year>2007 & year<2018
reghdfe non_irri_rent_r cont_tariff_w gdd dday30C prec prec2 i.year c.trend#i.stateansi c.trend2#i.stateansi, absorb(fips) vce(cluster stateansi year)
keep if e(sample)==1

keep year fips non_irri_rent non_irri_rent_r cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w cont_share_* gdd hdd prec

label var non_irri_rent "Nominal Cash Rent (USD/acre)"
label var non_irri_rent_r "Real Cash Rent (USD/acre, 2017 USD)"

drop if non_irri_rent_r==. 
egen county_group=group(fips)
egen no_county=max(county_group)
gen N=_N
drop county_group

summarize non_irri_rent non_irri_rent_r cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w cont_share_* 
outreg2 using "$figure/summary_draft.tex", replace tex(frag) sum(log) eqkeep(mean sd) label

gen regime=1
replace regime=2 if year>2012 

egen county_group=group(fips) if regime==1
egen no_county1=max(county_group) if regime==1
drop county_group

egen county_group=group(fips) if regime==2
egen no_county2=max(county_group) if regime==2

replace no_county=no_county1 if regime==1
replace no_county=no_county2 if regime==2

bys regime: replace N=_N
drop county_group no_county1 no_county2

summarize non_irri_rent non_irri_rent_r cont_tariff_w ssd_tariff_w ssd_ssd_tariff_w cont_share_* 
bys regime: outreg2 using "$figure/summary_draft.tex", append tex(frag) sum(log) eqkeep(mean sd) label
restore

