set more off
cd "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/"
global data "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/data_rev/"
global figure "/Users/jisangyu/Dropbox/Trade_incidence/Tariff_cash_rent_paper/draft_tariff_rent/"

grstyle init
grstyle set plain, horizontal compact minor
grstyle set legend 4, nobox klength(small)
grstyle set color hue, n(3) opacity(50)

use "$data/placebo_sim_data_years.dta", clear

gr tw kdensity beta, graphregion(color(white)) xline(-4.21) title("(a) Time-series permutations: Exogeneity", size(small)) range(-5 5) ///
text(.25 -4.21 "{&beta}=-4.21 (p-value=0.000)", place(se)) ytitle("Density") xtitle("Coefficients")
graph save beta_placebo_years.gph, replace

gr tw kdensity tstat, graphregion(color(white)) xline(-6.99) title("(b) Time-series permutations: Inference", size(small)) range(-7.5 7.5) ///
text(.4 -6.99 "t=-6.99 (p-value=0.000)", place(se)) ytitle("Density") xtitle("t-statistics")
graph save t_placebo_years.gph, replace

use "$data/placebo_sim_data_cross_sec.dta", clear

gr tw kdensity beta, graphregion(color(white)) xline(-4.21) title("(c) Cross-sectional permutations: Exogeneity", size(small)) range(-5 5) ///
text(3 -4.21 "{&beta}=-4.21 (p-value=0.000)", place(se)) ytitle("Density") xtitle("Coefficients")
graph save beta_placebo_cross_sec.gph, replace

gr tw kdensity tstat, graphregion(color(white)) xline(-6.99) title("(d) Cross-sectional permutations: Inference", size(small)) range(-7.5 7.5) ///
text(.4 -6.99 "t=-6.99 (p-value=0.000)", place(se)) ytitle("Density") xtitle("t-statistics")
graph save t_placebo_cross_sec.gph, replace

graph combine beta_placebo_years.gph t_placebo_years.gph beta_placebo_cross_sec.gph t_placebo_cross_sec.gph, col(2) graphregion(color(white))
graph export "$figure/placebo.png", replace

rm beta_placebo_cross_sec.gph
rm beta_placebo_years.gph

rm t_placebo_cross_sec.gph
rm t_placebo_years.gph

