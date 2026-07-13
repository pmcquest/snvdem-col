***********************************************************************************************
*                                                                                             *  
* New Evidence Reveals Curvilinear Relationship between Levels of Democracy and Deforestation *
*                                                                                             *
* This Version: December 12, 2024                                                             *
*                                                                                             *
***********************************************************************************************

**************
* Data Setup *
**************

use "qog_std_ts_jan23_stata14.dta", clear
xtset ccode year

replace fh_pr=8-fh_pr
replace br_elecyear=. if br_elecyear==2

gen ln_pop=ln(wdi_pop)
gen ln_gdppc=ln(wdi_gdpcapcon2015)

by ccode: carryforward p_polity2, gen(ip_polity2)
gen autocracy=0 if ip_polity2!=.
replace autocracy=1 if ip_polity2<=-6 & ip_polity2!=.
gen anocracy=0 if ip_polity2!=.
replace anocracy=1 if ip_polity2>-6 & ip_polity2<6 & ip_polity2!=.
gen democracy=0 if ip_polity2!=.
replace democracy=1 if ip_polity2>=6 & ip_polity2!=.

*******************************************
* Regression Models - Replication Sanford *
*******************************************

*use "C:\Users\user\Dropbox\Democracy and Deforestation\AJPS Replication\Sanford AJPS.dta", clear
*xtset un year
*xtreg forestdiff forestl democracy_bx pcgdpl pcgdpchangel popgrowthl i.year, fe

eststo clear
eststo: xtreg forestdiff forestl democracy_bx i.year, fe
estimates store reg1
eststo: xtreg forestdiff forestl democracy_bx br_elecyear pcgdpl pcgdpchangel popgrowthl i.year, fe
estimates store reg2
eststo: xtreg forestdiff forestl democracy i.year, fe
estimates store reg3
eststo: xtreg forestdiff forestl democracy br_elecyear pcgdpl pcgdpchangel popgrowthl i.year, fe
estimates store reg4
eststo: xtreg forestdiff forestl c.p_polity2##c.p_polity2 i.year, fe
qui margins, at(p_polity2=(-10(1)10))
marginsplot, level(90) recast(line) recastci(rline) legend(off) xlabel(-10(1)10) scheme(538w) aspectratio(1) name(graph1, replace)
estimates store reg5
eststo: xtreg forestdiff forestl c.p_polity2##c.p_polity2 br_elecyear pcgdpl pcgdpchangel popgrowthl i.year, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)

*********************************
* Regression Models - Extension *
*********************************

eststo clear
eststo: xtreg d.fao_luforest l.fao_lufores c.vdem_polyarchy##c.vdem_polyarchy i.year, fe
estimates store reg1
qui reg d.fao_luforest l.fao_lufores c.vdem_polyarchy##c.vdem_polyarchy i.year i.ccode
qui margins, at(vdem_polyarchy=(0(0.1)1))
marginsplot, level(90) recast(line) recastci(rline) legend(off) xlabel(0(0.1)1) scheme(538w) aspectratio(1) name(graph1, replace)
eststo: xtreg d.fao_luforest l.fao_lufores c.vdem_polyarchy##c.vdem_polyarchy br_elecyear d.ln_pop ln_gdppc d.ln_gdppc i.year, fe
estimates store reg2
eststo: reg d.fao_luforest l.fao_lufores c.vdem_polyarchy##c.vdem_polyarchy br_elecyear d.ln_pop ln_gdppc d.ln_gdppc i.year
estimates store reg3
eststo: xtreg d.fao_luforest l.fao_lufores c.vdem_polyarchy##c.vdem_polyarchy br_elecyear d.ln_pop ln_gdppc d.ln_gdppc i.year, re
estimates store reg4
esttab reg1 reg2 reg3 reg4, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)

eststo: xtreg d.fao_luforest l.fao_lufores c.fh_pr##c.fh_pr i.year, fe
estimates store reg1
qui reg d.fao_luforest l.fao_lufores c.fh_pr##c.fh_pr i.year i.ccode
qui margins, at(fh_pr=(1(1)7))
marginsplot, level(90) recast(line) recastci(rline) legend(off) xlabel(1(1)7) scheme(538w) aspectratio(1) name(graph2, replace)
eststo: xtreg d.fao_luforest l.fao_lufores c.fh_pr##c.fh_pr br_elecyear d.ln_pop ln_gdppc d.ln_gdppc i.year, fe
estimates store reg2
eststo: reg d.fao_luforest l.fao_lufores c.fh_pr##c.fh_pr br_elecyear d.ln_pop ln_gdppc d.ln_gdppc i.year
estimates store reg3
eststo: xtreg d.fao_luforest l.fao_lufores c.fh_pr##c.fh_pr br_elecyear d.ln_pop ln_gdppc d.ln_gdppc i.year, re
estimates store reg4
esttab reg1 reg2 reg3 reg4, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)

***********************************
* Appendix - Descriptive Overview *
***********************************

sum forestdiff forestl democracy_bx pcgdpl pcgdpchangel popgrowthl

pwcorr fao_luforest forest, sig
twoway (scatter fao_luforest forest) (lpolyci fao_luforest forest), scheme(538w) aspectratio(1) legend(off)

preserve
drop if fao_luforest==.
twoway (lpolyci fao_luforest year), scheme(538w) aspectratio(1) legend(off)
restore

sum d.fao_luforest l.d.fao_luforest fao_luforest p_polity2 vdem_polyarchy fh_pr ln_gdppc d.ln_gdppc d.ln_pop br_elecyear if fao_luforest!=.
pwcorr p_polity2 vdem_polyarchy fh_pr, sig

********************************
* Appendix - Robustness Checks *
********************************

eststo clear
eststo: xtreg forestdiff l.forestdiff forestl c.p_polity2##c.p_polity2 i.year, fe
estimates store reg1
eststo: xtreg forestdiff l.forestdiff forestl c.vdem_polyarchy##c.vdem_polyarchy i.year, fe
estimates store reg2
eststo: xtreg forestdiff l.forestdiff forestl c.fh_pr##c.fh_pr i.year, fe
estimates store reg3
eststo: xtreg d.fao_luforest l.d.fao_luforest l.fao_luforest c.p_polity2##c.p_polity2 i.year, fe
estimates store reg4
eststo: xtreg d.fao_luforest l.d.fao_luforest l.fao_luforest c.vdem_polyarchy##c.vdem_polyarchy i.year, fe
estimates store reg5
eststo: xtreg d.fao_luforest l.d.fao_luforest l.fao_luforest c.fh_pr##c.fh_pr i.year, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)
esttab using "TableA3.tex",   /*
 */ se title(Forest Cover Level in the Previous Year)  nonumbers /*
 */ mtitles("Model A1" "Model A2" "Model A3" "Model A4" "Model A5" "Model A6") label nodep nogaps star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) /*
 */ nonotes addnotes("$*$ p < 0.10, $**$ p < 0.05, $***$ p < 0.01" "Standard errors in parentheses") replace 

preserve
gen fao_luforest_diff=d.fao_luforest
sort year
egen forestdiff_SL=mean(forestdiff), by(year)
egen fao_luforest_SL=mean(fao_luforest_diff), by(year)
xtset ccode year
eststo clear
eststo: xtreg forestdiff forestdiff_SL forestl c.p_polity2##c.p_polity2 i.year, fe
estimates store reg1
eststo: xtreg forestdiff forestdiff_SL forestl c.vdem_polyarchy##c.vdem_polyarchy i.year, fe
estimates store reg2
eststo: xtreg forestdiff forestdiff_SL forestl c.fh_pr##c.fh_pr i.year, fe
estimates store reg3
eststo: xtreg d.fao_luforest fao_luforest_SL l.fao_luforest c.p_polity2##c.p_polity2 i.year, fe
estimates store reg4
eststo: xtreg d.fao_luforest fao_luforest_SL l.fao_luforest c.vdem_polyarchy##c.vdem_polyarchy i.year, fe
estimates store reg5
eststo: xtreg d.fao_luforest fao_luforest_SL l.fao_luforest c.fh_pr##c.fh_pr i.year, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)
esttab using "TableA4.tex",   /*
 */ se title(Spatial Forest Cover Change Variable)  nonumbers /*
 */ mtitles("Model A7" "Model A8" "Model A9" "Model A10" "Model A11" "Model A12") label nodep nogaps star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) /*
 */ nonotes addnotes("$*$ p < 0.10, $**$ p < 0.05, $***$ p < 0.01" "Standard errors in parentheses") replace 
restore

eststo clear
eststo: xtreg forestdiff forestl c.p_polity2##c.p_polity2 wdi_trade i.year, fe
estimates store reg1
eststo: xtreg forestdiff forestl c.vdem_polyarchy##c.vdem_polyarchy wdi_trade i.year, fe
estimates store reg2
eststo: xtreg forestdiff forestl c.fh_pr##c.fh_pr wdi_trade i.year, fe
estimates store reg3
eststo: xtreg d.fao_luforest l.fao_luforest c.p_polity2##c.p_polity2 wdi_trade i.year, fe
estimates store reg4
eststo: xtreg d.fao_luforest l.fao_luforest c.vdem_polyarchy##c.vdem_polyarchy wdi_trade i.year, fe
estimates store reg5
eststo: xtreg d.fao_luforest l.fao_luforest c.fh_pr##c.fh_pr wdi_trade i.year, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)
esttab using "TableA5.tex",   /*
 */ se title(Trade Openness)  nonumbers /*
 */ mtitles("Model A13" "Model A14" "Model A15" "Model A16" "Model A17" "Model A18") label nodep nogaps star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) /*
 */ nonotes addnotes("$*$ p < 0.10, $**$ p < 0.05, $***$ p < 0.01" "Standard errors in parentheses") replace 
 
gen left=0
replace left=1 if execrlc==3
replace left=1 if gov1rlc==3
eststo clear
eststo: xtreg forestdiff forestl c.p_polity2##c.p_polity2 i.left i.year, fe
estimates store reg1
eststo: xtreg forestdiff forestl c.vdem_polyarchy##c.vdem_polyarchy i.left i.year, fe
estimates store reg2
eststo: xtreg forestdiff forestl c.fh_pr##c.fh_pr i.left i.year, fe
estimates store reg3
eststo: xtreg d.fao_luforest l.fao_luforest c.p_polity2##c.p_polity2 i.left i.year, fe
estimates store reg4
eststo: xtreg d.fao_luforest l.fao_luforest c.vdem_polyarchy##c.vdem_polyarchy i.left i.year, fe
estimates store reg5
eststo: xtreg d.fao_luforest l.fao_luforest c.fh_pr##c.fh_pr i.left i.year, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)
esttab using "TableA6.tex",   /*
 */ se title(Political Ideology)  nonumbers /*
 */ mtitles("Model A19" "Model A20" "Model A21" "Model A22" "Model A23" "Model A24") label nodep nogaps star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) /*
 */ nonotes addnotes("$*$ p < 0.10, $**$ p < 0.05, $***$ p < 0.01" "Standard errors in parentheses") replace 

gen majoritarian=0
replace majoritarian=1 if pluralty==1 & pr==0
eststo clear
eststo: xtreg forestdiff forestl c.p_polity2##c.p_polity2 i.majoritarian i.year, fe
estimates store reg1
eststo: xtreg forestdiff forestl c.vdem_polyarchy##c.vdem_polyarchy i.majoritarian i.year, fe
estimates store reg2
eststo: xtreg forestdiff forestl c.fh_pr##c.fh_pr i.majoritarian i.year, fe
estimates store reg3
eststo: xtreg d.fao_luforest l.fao_luforest c.p_polity2##c.p_polity2 i.majoritarian i.year, fe
estimates store reg4
eststo: xtreg d.fao_luforest l.fao_luforest c.vdem_polyarchy##c.vdem_polyarchy i.majoritarian i.year, fe
estimates store reg5
eststo: xtreg d.fao_luforest l.fao_luforest c.fh_pr##c.fh_pr i.majoritarian i.year, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)
esttab using "TableA7.tex",   /*
 */ se title(Electoral System)  nonumbers /*
 */ mtitles("Model A25" "Model A26" "Model A27" "Model A28" "Model A29" "Model A30") label nodep nogaps star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) /*
 */ nonotes addnotes("$*$ p < 0.10, $**$ p < 0.05, $***$ p < 0.01" "Standard errors in parentheses") replace 

eststo clear
eststo: xtreg forestdiff forestl c.p_polity2##c.p_polity2 h_polcon3 i.year, fe
estimates store reg1
eststo: xtreg forestdiff forestl c.vdem_polyarchy##c.vdem_polyarchy h_polcon3 i.year, fe
estimates store reg2
eststo: xtreg forestdiff forestl c.fh_pr##c.fh_pr h_polcon3 i.year, fe
estimates store reg3
eststo: xtreg d.fao_luforest l.fao_luforest c.p_polity2##c.p_polity2 h_polcon3 i.year, fe
estimates store reg4
eststo: xtreg d.fao_luforest l.fao_luforest c.vdem_polyarchy##c.vdem_polyarchy h_polcon3 i.year, fe
estimates store reg5
eststo: xtreg d.fao_luforest l.fao_luforest c.fh_pr##c.fh_pr h_polcon3 i.year, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)
esttab using "TableA8.tex",   /*
 */ se title(Veto Players)  nonumbers /*
 */ mtitles("Model A31" "Model A32" "Model A33" "Model A34" "Model A35" "Model A36") label nodep nogaps star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) /*
 */ nonotes addnotes("$*$ p < 0.10, $**$ p < 0.05, $***$ p < 0.01" "Standard errors in parentheses") replace 
 
eststo clear
eststo: xtreg forestdiff forestl p_polity2 i.year if p_polity2>-6, fe
estimates store reg1
eststo: xtreg forestdiff forestl vdem_polyarchy i.year if vdem_polyarchy>=0.5, fe
estimates store reg2
eststo: xtreg forestdiff forestl fh_pr i.year if fh_pr>3, fe
estimates store reg3
eststo: xtreg d.fao_luforest l.fao_luforest p_polity2 i.year if p_polity2>-6, fe
estimates store reg4
eststo: xtreg d.fao_luforest l.fao_luforest vdem_polyarchy i.year if vdem_polyarchy>=0.5, fe
estimates store reg5
eststo: xtreg d.fao_luforest l.fao_luforest fh_pr i.year if fh_pr>3, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)
esttab using "TableA9.tex",   /*
 */ se title(Omitting Full Autocracies)  nonumbers /*
 */ mtitles("Model A37" "Model A38" "Model A39" "Model A40" "Model A41" "Model A42") label nodep nogaps star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) /*
 */ nonotes addnotes("$*$ p < 0.10, $**$ p < 0.05, $***$ p < 0.01" "Standard errors in parentheses") replace 
 
eststo clear
eststo: xtreg forestdiff forestl c.p_polity2##c.p_polity2 i.year if year>1995, fe
estimates store reg1
eststo: xtreg forestdiff forestl c.vdem_polyarchy##c.vdem_polyarchy i.year if year>1995, fe
estimates store reg2
eststo: xtreg forestdiff forestl c.fh_pr##c.fh_pr i.year if year>1995, fe
estimates store reg3
eststo: xtreg d.fao_luforest l.fao_luforest c.p_polity2##c.p_polity2 i.year if year>1995, fe
estimates store reg4
eststo: xtreg d.fao_luforest l.fao_luforest c.vdem_polyarchy##c.vdem_polyarchy i.year if year>1995, fe
estimates store reg5
eststo: xtreg d.fao_luforest l.fao_luforest c.fh_pr##c.fh_pr i.year if year>1995, fe
estimates store reg6
esttab reg1 reg2 reg3 reg4 reg5 reg6, se star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) label drop(*.year)
esttab using "TableA10.tex",   /*
 */ se title(Restricted Time Period: Post-1995 Sample)  nonumbers /*
 */ mtitles("Model A43" "Model A44" "Model A45" "Model A46" "Model A47" "Model A48") label nodep nogaps star(* 0.10 ** 0.05 *** 0.01) b(%10.3f) /*
 */ nonotes addnotes("$*$ p < 0.10, $**$ p < 0.05, $***$ p < 0.01" "Standard errors in parentheses") replace 
 
preserve
gen LDV1=forestdiff
gen LDV2=d.fao_luforest
xtile quart_LDV1 = LDV1, nq(3)
xtile quart_LDV2 = LDV2, nq(3)
xtile quart_pol = p_polity2, nq(3)
xtile quart_vdem = vdem_polyarchy, nq(3)
xtile quart_fh = fh_pr, nq(3)
tab quart_LDV1 quart_pol
tab quart_LDV1 quart_vdem
tab quart_LDV1 quart_fh
tab quart_LDV2 quart_pol
tab quart_LDV2 quart_vdem
tab quart_LDV2 quart_fh
restore 