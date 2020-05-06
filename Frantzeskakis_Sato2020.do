********************************************************************************
*Author: Yuko Sato
*Created Data:04/08/2020; Last modified: 04/08/2020
*Objective: Replication Material for Electoral Studies
********************************************************************************

//program setup
version 15
clear all
set linesize 80
macro drop _all
set scheme lean2
set more off

//cd
use "Frantzeskakis_Sato2020_Individual-Level", replace
xtset id_UN

*** 1. Individual-Level Analysis
*** Descriptive Statistics *****************************************************
logit far_right dic_18_25_fy poco_18_25_fy pos_com pos_dic age_2-age_8 if country!="Itary" & country !="Austria" & far_right_vs >0.03, cluster(id_UN) 
hist y_birth if e(sample) & pos_dic ==0 & pos_com ==0, xscale(range(1900 2000)) xtitle(Birth Year) name(hist1, replace) percent fysize(25)
hist y_birth if e(sample) & pos_dic==1, xscale(range(1900 2000)) xtitle(Birth Year) name(hist2, replace) percent fysize(25)
hist y_birth if e(sample) & pos_com==1, xscale(range (1900 2000)) xtitle(Birth Year) name(hist3, replace) percent fysize(25)

bysort y_birth : egen mean_far_right_dem =mean(far_right)if pos_dic ==0 & pos_com ==0& e(sample)
bysort y_birth : egen mean_far_right_exdic =mean(far_right)if pos_dic ==1 & e(sample)
bysort y_birth : egen mean_far_right_poco =mean(far_right)if pos_com ==1& e(sample)
collapse mean_far_right_dem mean_far_right_exdic mean_far_right_poco, by (y_birth)

*** Figure 1: Average Far-Right Support by the Birth-Year Groups
twoway fpfitci mean_far_right_dem y_birth || scatter mean_far_right_dem y_birth if mean_far_right_dem <0.3, msymbol(o)/*
*/ yscale(range(0 0.30)) ylabel(0(.1)0.3) title("") xtitle("Birth Year (Old Democracies)") ytitle("Mean Far-Right Support (%)") legend(off) name(dem, replace) 
graph combine dem hist1, cols(1)	
twoway fpfitci mean_far_right_exdic y_birth || scatter mean_far_right_exdic y_birth if mean_far_right_dem <0.3, msymbol(x) /*
*/yscale(range(0 0.30)) ylabel(0(.1)0.3) title("") xtitle("Birth Year (Right-Wing Dictatorship)") ytitle("Mean Far-Right Support (%)") legend(off) name(right, replace) 
graph combine right hist2, cols(1)	
twoway fpfitci mean_far_right_poco y_birth || scatter mean_far_right_poco y_birth if mean_far_right_poco <0.3, /*
*/yscale(range(0 0.30)) ylabel(0(.1)0.3) title("") xtitle("Birth Year (Left Wing Dictatorship)") ytitle("Mean Far-Right Support (%)") legend(off) name(left, replace) 
graph combine left hist3, cols(1)	

*** Figure 2: Difference between Old Democracies and New Democracies
gen dif_right = mean_far_right_exdic -  mean_far_right_dem 
gen dif_left = mean_far_right_poco - mean_far_right_dem 

twoway fpfitci dif_right y_birth || scatter dif_right y_birth if mean_far_right_dem <0.3 , msymbol(x) /*
*/yscale(range(0 0.20)) ylabel(-0.2(.1)0.2) title("") xtitle("Birth Year (Right-Wing Dictatorship)") ytitle("Difference in Far-Right Support (%) with Old Democracies") legend(off) name(right_dif, replace) 
twoway fpfitci dif_left y_birth, lpattern(solid) || scatter dif_left y_birth if mean_far_right_dem <0.3, msymbol(+)/*
*/yscale(range(0 0.20)) ylabel(-0.2(.1)0.2) title("") xtitle("Birth Year (Left-Wing Dictatorship)") ytitle("Difference in Far-Right Support (%) with Old Democracies") legend(off) name(left_dif, replace) 
graph combine left_dif right_dif 


*** Table 1: Effects of Variabloes on Far-Right Support ************************
use "Frantzeskakis_Sato2020_Individual-Level", replace
xtset id_UN
*(1) Baseline model
logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com age_2-age_8 if far_right_vs >0.03, cluster(id_UN) 
*(2) Individual level variables control 
logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec unemp age_2-age_8 if far_right_vs >0.03, cluster(id_UN) 
*(3) With country FE (Only the samples with multiple observations in country-year level)
logit far_right dic_18_25_fy poco_18_25_fy female pos_sec sec unemp age_2-age_8 country2-country14 if fe ==1 & far_right_vs >0.03, cluster (id_UN)
*(4) With country-year level variables		
bootstrap, cluster(id_UN) reps(200) seed(1234): logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 age_2-age_8 if far_right_vs >0.03

*** Figure 3: Predicted probability of Far-Right Party Support *****************
set seed 12345
estsimp logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec age_2-age_8 if far_right_vs >0.03, cluster(id_UN)

postutil clear
postfile pv xaxis me se lo hi me2 se2 lo2 hi2 me3 se3 lo3 hi3 using "Average_simulation_ind.dta", replace

qui foreach i of numlist 0(1)8{

    *** Right-wing dictatorship
    setx  dic_18_25_fy `i' poco_18_25_fy 0 pos_dic 1 pos_com 0 female 1 pos_sec 1 sec 0 age_2 0 age_3 0 age_4 0 age_5 1 age_6 0 age_7 0 age_8 0 
	simqi, prval(1) genpr(pr)
	
	qui sum pr
	local me = r(mean)
	local se = r(sd)
	
	_pctile pr, p(2.5 97.5)
	local lo = r(r1)
	local hi = r(r2)
	
    *** Left-wing dictatorship
	setx dic_18_25_fy 0 poco_18_25_fy `i' pos_dic 0 pos_com 1 female 1 pos_sec 1 sec 0 age_2 0 age_3 0 age_4 0 age_5 1 age_6 0 age_7 0 age_8 0 
	simqi, prval(1) genpr(pr2)
	
	qui sum pr2
	local me2 = r(mean)
	local se2 = r(sd)
	
	_pctile pr2, p(2.5 97.5)
	local lo2 = r(r1)
	local hi2 = r(r2)

    *** Old democracies
	setx dic_18_25_fy 0 poco_18_25_fy 0 pos_dic 0 pos_com 0 female 1 pos_sec 1 sec 0 age_2 0 age_3 0 age_4 0 age_5 1 age_6 0 age_7 0 age_8 0 
	simqi, prval(1) genpr(pr3)
	
	qui sum pr3
	local me3 = r(mean)
	local se3 = r(sd)
	
	_pctile pr3, p(2.5 97.5)
	local lo3 = r(r1)
	local hi3 = r(r2)

	
	drop pr
	drop pr2
	drop pr3

	post pv (`i') (`me') (`se') (`lo') (`hi') (`me2') (`se2') (`lo2') (`hi2') (`me3') (`se3') (`lo3') (`hi3') 
}

postclose pv

preserve
use "Average_simulation_ind.dta", clear
twoway (line me xaxis) (line lo xaxis, lpattern(solid) lcolor(black%0)) (line hi xaxis, lpattern(solid) lcolor(gray%0)) (line me3 xaxis, lcolor(gs8)) (line lo3 xaxis, lpattern(solid) lcolor(black%0)) (line hi3 xaxis, lpattern(solid) lcolor(black%0)) ||/*
*/     rarea hi lo xaxis, color(black%20) || rarea hi3 lo3 xaxis, color(black%20) /*
*/     , xtitle("Years of Authoritarian Exposure (yr 18-25)") ytitle("Pr(Far-Right Voting)") legend(order(1 "Post Right-Wing Dictatorship" 4 "Old Democracies") ring(1) position(6) bmargin(small)) name(Ipr1, replace)
restore

*** 2. Country-Level Analysis
use "Frantzeskakis_Sato2020_Country-Level", replace
xtset newidn year
*** Table 2: Effects of Variabloes on Far-Right Vote Share *********************
* (5) Model without Axis Countries
tobit farVote pos_dic pos_com dic_time com_time yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan", ll(0) ul(100) vce(cluster newid)
* (6) Model without Axis Countries with FE
xttobit farVote pos_dic pos_com dic_time com_time yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan" & fe1>1, ll(0) ul(100) 
* (7) Model with Axis Countries
tobit farVote pos_dic pos_com dic_time com_time yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 , ll(0) ul(100) vce(cluster newid)
* (8) Model with Axis Countries with FE
xttobit farVote pos_dic pos_com dic_time com_time yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & fe1>1, ll(0) ul(100) 	

*** Figure 4: Difference in the Marginal Effects of Post Right-Wing Dictatorship
xttobit farVote i.pos_dic##c.yearsDem i.pos_com##c.yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan"& fe1>1, ll(0) ul(100) 
hist dic_time if e(sample) & yearsDem <=45 &  pos_dic==1, xscale(range(0 50)) xtitle(Years since Transition) name(hist, replace) percent fysize(25)
margins r.pos_dic, at(yearsDem = (0(1)45)) level(95) 
marginsplot, recast(line) plot1opts(lcolor(black%90)) ciopt(color(black%20)) recastci(rarea) yline(0) title("") xtitle("Years since Transition") ytitle("Effects on Far-Right Vote Share") name(fd2, replace) 
graph combine fd2 hist, cols(1)	

exit
