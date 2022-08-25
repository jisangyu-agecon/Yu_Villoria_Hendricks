/*

Clean TRAINS data -- nominal and relative tariffs 

*/

clear all
set more off

global data "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev"
global figure "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/draft_tariff_rent/"

import delimited "$data/trains_all_export_field_crops.csv", clear

*keep only 7 crops
drop if product==1002
drop if product==1008
drop if product>5201

*drop World as the partner
drop if partner==0

*check whether preferntial tariff exists throughout the sample period
gen preferential=0
replace preferential=1 if dutytype=="PRF" 
bys reporter partner product tariffyear: egen prf_reported=max(preferential)
summ prf_reported

forval i=2000/2017{
gen preferential_`i'=0
replace preferential_`i'=1 if dutytype=="PRF" & tariffyear==`i'
bys reporter partner product: egen prf_reported_`i'=max(preferential_`i')

gen prfxt_`i'=preferential_`i'*weightedaverage
bys reporter partner product: egen prf_t_`i'=max(prfxt_`i')

summ prf_reported_`i' prf_t_`i'
}

*replace AHS with preferential if preferential tariff are reported in earlier period and not later
forval i=2000/2017{
replace weightedaverage=prf_t_`i' if dutytype=="AHS" & prf_reported_`i'==1 & tariffyear>`i' & prf_reported==0
}
forval i=2000/2017{
replace weightedaverage=prf_t_`i' if dutytype=="AHS" & prf_reported==1 & weightedaverage>prf_t_`i' & tariffyear==`i'
}

*reconcile trade and tariff years
preserve
keep product reporter partner reportername productname partnername tradeyear importsvaluein1000usd
collapse (max) importsvaluein1000usd, by(product reporter partner reportername productname partnername tradeyear)
rename tradeyear year
tempfile temp
save `temp', replace
restore
drop tradeyear importsvaluein1000usd
rename tariffyear year
*keep AHS
keep if dutytype=="AHS"
merge 1:1 product reporter partner reportername productname partnername year using `temp'
drop if _merge==2
drop _merge
replace importsvaluein1000usd=0 if importsvaluein1000usd==.

*impute missing years
keep if year>2002
*group ID
drop if product==.
drop if reporter==.
drop if partner==.
egen product_country=group(product reporter partner)
drop if product_country==.

*impute WITS
preserve
collapse (mean) weightedaverage importsvaluein1000usd, by(product_country product reporter partner reportername productname partnername)
expand 16
bys product_country: gen n=_n
gen year=n+2002
tempfile temp
rename weightedaverage avg_weightedavg
rename importsvaluein1000usd avg_importsvaluein1000usd
save `temp', replace
restore
merge 1:1 product_country year using `temp'
drop _merge
replace weightedaverage=avg_weightedavg if weightedaverage==. 
replace importsvaluein1000usd=avg_importsvaluein1000usd if importsvaluein1000usd==. 
drop avg_weightedavg avg_importsvaluein1000usd

*rename productname
replace productname="Barley" if product==1003
replace productname="Corn" if product==1005
replace productname="Grain Sorghum" if product==1007
replace productname="Oats" if product==1004
replace productname="Rice" if product==1006
replace productname="Soybeans" if product==1201
replace productname="Cotton" if product==5201
replace productname="Wheat" if product==1001

bys productname: summ weightedaverage
bys productname: summ weightedaverage if reporter==156

*China TRQ replace wheat and corn with in-quota rate of 1%
replace weightedaverage=1 if reporter==156 & partner==840 & product==1001
replace weightedaverage=1 if reporter==156 & partner==840 & product==1005

*Compute base import quantity
*xtset
xtset product_country year
tssmooth ma avg5_imports=importsvaluein1000usd, window(4 1 0)
tssmooth ma avg3_imports=importsvaluein1000usd, window(2 1 0)
tssmooth ma avg7_imports=importsvaluein1000usd, window(6 1 0)

*2007 dummy
gen year2007=0
replace year2007=1 if year==2007

gen year2007xavg5_imports=year2007*avg5_imports
bys product_country: egen initial_import=max(year2007xavg5_imports)
replace initial_import=0 if initial_import==.
gen year2007xavg3_imports=year2007*avg3_imports
bys product_country: egen initial_import_alt=max(year2007xavg3_imports)
replace initial_import_alt=0 if initial_import_alt==.
gen year2007xavg7_imports=year2007*avg7_imports
bys product_country: egen initial_import_alt2=max(year2007xavg7_imports)
replace initial_import_alt=0 if initial_import_alt2==.

*Export exposure
bys year product reporter: egen totalimport=sum(importsvaluein1000usd)

*Genearate Tariff data: nominal contemp. import weighted
preserve
*keep only from US
keep if partner==840
*compute weighted average
collapse (rawsum) importsvaluein1000usd (mean) weightedaverage totalimport [aweight=importsvaluein1000usd], by(year product)
*labeling
label var weightedaverage "Weighted average of AHS tariff"
label var totalimport "Total import"
*reshape
reshape wide weightedaverage totalimport importsvaluein1000usd, i(year) j(product)
*rename
rename weightedaverage1001 weightedaverage_wheat
rename totalimport1001 totalimport_wheat
rename importsvaluein1000usd1001 imports_wheat
rename weightedaverage1003 weightedaverage_barley
rename totalimport1003 totalimport_barley
rename importsvaluein1000usd1003 imports_barley
rename weightedaverage1004 weightedaverage_oats
rename totalimport1004 totalimport_oats
rename importsvaluein1000usd1004 imports_oats
rename weightedaverage1005 weightedaverage_corn
rename totalimport1005 totalimport_corn
rename importsvaluein1000usd1005 imports_corn
rename weightedaverage1006 weightedaverage_rice
rename totalimport1006 totalimport_rice
rename importsvaluein1000usd1006 imports_rice
rename weightedaverage1007 weightedaverage_sorghum
rename totalimport1007 totalimport_sorghum
rename importsvaluein1000usd1007 imports_sorghum
rename weightedaverage1201 weightedaverage_soybeans
rename totalimport1201 totalimport_soybeans
rename importsvaluein1000usd1201 imports_soybeans
rename weightedaverage5201 weightedaverage_ucotton
rename totalimport5201 totalimport_ucotton
rename importsvaluein1000usd5201 imports_ucotton
*save
save "$data/trains_tariff_nomcont.dta", replace

*Graph
*label
foreach x in barley corn oats rice sorghum soybeans wheat{
local c=proper("`x'")
label var weightedaverage_`x' "`c'"
}

label var weightedaverage_ucotton "Cotton"

foreach x in barley corn oats rice sorghum soybeans ucotton wheat{

merge m:1 year using  "$data/raw_data_nass/`x'_production_value.dta"
drop _merge

replace weightedaverage_`x'=weightedaverage_`x'*(1000*imports_`x'/`x'_production_value)
}

order *_corn *_soybeans *_wheat *_sorghum *_ucotton *_rice *_barley *_oats
drop *_rice

grstyle clear
grstyle init
grstyle set plain, horizontal compact minor
grstyle set lpattern
grstyle set color hue, n(3) opacity(50)
graph tw line weightedaverage_* year if year>2007 & year<2018, ytitle("Ad valorem tariffs", size(vsmall)) ///
legend(col(4)) xlab(2008(1)2017, labsize(small)) ylab(, labsize(small)) saving(tariff.gph, replace)
gr export "$figure/tariff.eps", replace
restore

*Genearate Tariff data: nominal initial. import weighted
preserve
*keep only from US
keep if partner==840
*compute weighted average
collapse (rawsum) avg5_imports  (mean) weightedaverage totalimport [aweight=initial_import], by(year product)
*labeling
label var weightedaverage "Init. weighted average of AHS tariff"
label var totalimport "Init. weighted total import"
*reshape
reshape wide weightedaverage totalimport avg5_imports, i(year) j(product)
*rename
rename weightedaverage1001 ss_weightedaverage_wheat
rename weightedaverage1003 ss_weightedaverage_barley
rename weightedaverage1004 ss_weightedaverage_oats
rename weightedaverage1005 ss_weightedaverage_corn
rename weightedaverage1006 ss_weightedaverage_rice
rename weightedaverage1007 ss_weightedaverage_sorghum
rename weightedaverage1201 ss_weightedaverage_soybeans
rename weightedaverage5201 ss_weightedaverage_ucotton

rename totalimport1001 ss_totalimport_wheat
rename totalimport1003 ss_totalimport_barley
rename totalimport1004 ss_totalimport_oats
rename totalimport1005 ss_totalimport_corn
rename totalimport1006 ss_totalimport_rice
rename totalimport1007 ss_totalimport_sorghum
rename totalimport1201 ss_totalimport_soybeans
rename totalimport5201 ss_totalimport_ucotton

rename avg5_imports1001 avg5_imports_wheat
rename avg5_imports1003 avg5_imports_barley
rename avg5_imports1004 avg5_imports_oats
rename avg5_imports1005 avg5_imports_corn
rename avg5_imports1006 avg5_imports_rice
rename avg5_imports1007 avg5_imports_sorghum
rename avg5_imports1201 avg5_imports_soybeans
rename avg5_imports5201 avg5_imports_ucotton

*save
save "$data/trains_tariff_nominit.dta", replace

*Graph
*label
foreach x in barley corn oats rice sorghum soybeans wheat{
local c=proper("`x'")
label var ss_weightedaverage_`x' "`c'"
}
label var ss_weightedaverage_ucotton "Cotton"

foreach x in barley corn oats rice sorghum soybeans ucotton wheat{
merge m:1 year using  "$data/raw_data_nass/`x'_production_value.dta"
drop _merge
}

tsset year
foreach x in barley corn oats rice sorghum soybeans ucotton wheat{

gen imp_share_`x'=(1000*avg5_imports_`x'/`x'_production_value)
tssmooth ma import_share_`x'=imp_share_`x', window(4 1 0)
}

*2007 dummy
gen year2007=0
replace year2007=1 if year==2007

foreach x in barley corn oats rice sorghum soybeans ucotton wheat{

gen year2007ximportshare_`x'=year2007*import_share_`x'
egen initial_imports_`x'=max(year2007ximportshare_`x')
}

foreach x in barley corn oats rice sorghum soybeans ucotton wheat{
replace ss_weightedaverage_`x'=ss_weightedaverage_`x'*initial_imports_`x'
}

order *_corn *_soybeans *_wheat *_sorghum *_ucotton *_rice *_barley *_oats
drop *_rice

grstyle clear
grstyle init
grstyle set plain, horizontal compact minor
grstyle set color hue, n(3) opacity(50)
grstyle set lpattern
graph tw line ss_weightedaverage_* year if year>2007 & year<2018, ytitle("Ad valorem tariffs", size(vsmall)) ///
legend(col(4)) xlab(2008(1)2017, labsize(small)) ylab(, labsize(small))  saving(tariff.gph, replace)
gr export "$figure/ss_tariff.eps", replace
restore

*Alternative MA (3-year)
*Genearate Tariff data: nominal initial. import weighted
preserve
*keep only from US
keep if partner==840
*compute weighted average
collapse (rawsum) avg3_imports  (mean) weightedaverage totalimport [aweight=initial_import_alt], by(year product)
*labeling
label var weightedaverage "Init. weighted average of AHS tariff"
label var totalimport "Init. weighted total import"
*reshape
reshape wide weightedaverage totalimport avg3_imports, i(year) j(product)
*rename
rename weightedaverage1001 ss_alt_weightedaverage_wheat
rename weightedaverage1003 ss_alt_weightedaverage_barley
rename weightedaverage1004 ss_alt_weightedaverage_oats
rename weightedaverage1005 ss_alt_weightedaverage_corn
rename weightedaverage1006 ss_alt_weightedaverage_rice
rename weightedaverage1007 ss_alt_weightedaverage_sorghum
rename weightedaverage1201 ss_alt_weightedaverage_soybeans
rename weightedaverage5201 ss_alt_weightedaverage_ucotton

rename totalimport1001 ss_alt_totalimport_wheat
rename totalimport1003 ss_alt_totalimport_barley
rename totalimport1004 ss_alt_totalimport_oats
rename totalimport1005 ss_alt_totalimport_corn
rename totalimport1006 ss_alt_totalimport_rice
rename totalimport1007 ss_alt_totalimport_sorghum
rename totalimport1201 ss_alt_totalimport_soybeans
rename totalimport5201 ss_alt_totalimport_ucotton

rename avg3_imports1001 avg3_imports_wheat
rename avg3_imports1003 avg3_imports_barley
rename avg3_imports1004 avg3_imports_oats
rename avg3_imports1005 avg3_imports_corn
rename avg3_imports1006 avg3_imports_rice
rename avg3_imports1007 avg3_imports_sorghum
rename avg3_imports1201 avg3_imports_soybeans
rename avg3_imports5201 avg3_imports_ucotton

*save
save "$data/trains_tariff_nominit_ma3.dta", replace
restore

*Alternative MA (7-year)
*Genearate Tariff data: nominal initial. import weighted
preserve
*keep only from US
keep if partner==840
*compute weighted average
collapse (rawsum) avg7_imports  (mean) weightedaverage totalimport [aweight=initial_import_alt2], by(year product)
*labeling
label var weightedaverage "Init. weighted average of AHS tariff"
label var totalimport "Init. weighted total import"
*reshape
reshape wide weightedaverage totalimport avg7_imports, i(year) j(product)
*rename
rename weightedaverage1001 ss_alt2_weightedaverage_wheat
rename weightedaverage1003 ss_alt2_weightedaverage_barley
rename weightedaverage1004 ss_alt2_weightedaverage_oats
rename weightedaverage1005 ss_alt2_weightedaverage_corn
rename weightedaverage1006 ss_alt2_weightedaverage_rice
rename weightedaverage1007 ss_alt2_weightedaverage_sorghum
rename weightedaverage1201 ss_alt2_weightedaverage_soybeans
rename weightedaverage5201 ss_alt2_weightedaverage_ucotton

rename totalimport1001 ss_alt2_totalimport_wheat
rename totalimport1003 ss_alt2_totalimport_barley
rename totalimport1004 ss_alt2_totalimport_oats
rename totalimport1005 ss_alt2_totalimport_corn
rename totalimport1006 ss_alt2_totalimport_rice
rename totalimport1007 ss_alt2_totalimport_sorghum
rename totalimport1201 ss_alt2_totalimport_soybeans
rename totalimport5201 ss_alt2_totalimport_ucotton

rename avg7_imports1001 avg7_imports_wheat
rename avg7_imports1003 avg7_imports_barley
rename avg7_imports1004 avg7_imports_oats
rename avg7_imports1005 avg7_imports_corn
rename avg7_imports1006 avg7_imports_rice
rename avg7_imports1007 avg7_imports_sorghum
rename avg7_imports1201 avg7_imports_soybeans
rename avg7_imports5201 avg7_imports_ucotton

*save
save "$data/trains_tariff_nominit_ma7.dta", replace
restore


*2011 weights
preserve
*2011 dummy
gen year2011=0
replace year2011=1 if year==2011
gen year2011xavg5_imports=year2011*avg5_imports
bys product_country: egen initial_share_import=max(year2011xavg5_imports)
*compute weighted average
collapse (rawsum) avg5_imports (mean) weightedaverage [aweight=initial_share_import], by(year product)

label var weightedaverage "Weighted average of AHS tariff (weighted average)"

*reshape
reshape wide weightedaverage avg5_imports, i(year) j(product)

*rename
rename weightedaverage1001 ss11_weightedaverage_wheat
rename weightedaverage1003 ss11_weightedaverage_barley
rename weightedaverage1004 ss11_weightedaverage_oats
rename weightedaverage1005 ss11_weightedaverage_corn
rename weightedaverage1006 ss11_weightedaverage_rice
rename weightedaverage1007 ss11_weightedaverage_sorghum
rename weightedaverage1201 ss11_weightedaverage_soybeans
rename weightedaverage5201 ss11_weightedaverage_ucotton
*save
save "$data/weights_2011.dta", replace
restore

*Genearate Tariff data: relative contemp. import weighted
preserve
*compute weighted average
collapse (mean) weightedaverage [aweight=importsvaluein1000usd], by(year reporter product)
*labeling
label var weightedaverage "Weighted average of AHS tariff"
*rename
rename weightedaverage avg_import_tariff
summ avg_import_tariff
*save
tempfile import_tariffs_by_reporter
save `import_tariffs_by_reporter', replace
restore
preserve
*keep imports from US only
keep if partner==840
merge 1:1 year reporter product using `import_tariffs_by_reporter'
replace weightedaverage=(100*(100+weightedaverage)/(100+avg_import_tariff))
*compute weighted average
collapse (mean) weightedaverage [aweight=importsvaluein1000usd], by(year product)
*labeling
label var weightedaverage "Weighted average of relative AHS tariff"
*reshape
reshape wide weightedaverage, i(year) j(product)
*rename
rename weightedaverage1001 rweightedaverage_wheat
rename weightedaverage1003 rweightedaverage_barley
rename weightedaverage1004 rweightedaverage_oats
rename weightedaverage1005 rweightedaverage_corn
rename weightedaverage1006 rweightedaverage_rice
rename weightedaverage1007 rweightedaverage_sorghum
rename weightedaverage1201 rweightedaverage_soybeans
rename weightedaverage5201 rweightedaverage_ucotton
*save
save "$data/trains_tariff_relcont.dta", replace
restore

*Genearate Tariff data: relative initial. import weighted
preserve
*compute weighted average
collapse (mean) weightedaverage [aweight=importsvaluein1000usd], by(year reporter product)
*labeling
label var weightedaverage "Init. weighted average of AHS tariff"
*rename
rename weightedaverage avg_import_tariff
summ avg_import_tariff
*save
tempfile import_tariffs_by_reporter
save `import_tariffs_by_reporter', replace
restore
preserve
*keep imports from US only
keep if partner==840
merge 1:1 year reporter product using `import_tariffs_by_reporter'
replace weightedaverage=(100*(100+weightedaverage)/(100+avg_import_tariff))
*compute weighted average
collapse (mean) weightedaverage [aweight=initial_import], by(year product)
*labeling
label var weightedaverage "Init. weighted average of relative AHS tariff"
*reshape
reshape wide weightedaverage, i(year) j(product)
*rename
rename weightedaverage1001 ss_rweightedaverage_wheat
rename weightedaverage1003 ss_rweightedaverage_barley
rename weightedaverage1004 ss_rweightedaverage_oats
rename weightedaverage1005 ss_rweightedaverage_corn
rename weightedaverage1006 ss_rweightedaverage_rice
rename weightedaverage1007 ss_rweightedaverage_sorghum
rename weightedaverage1201 ss_rweightedaverage_soybeans
rename weightedaverage5201 ss_rweightedaverage_ucotton
*save
save "$data/trains_tariff_relinit.dta", replace
restore











