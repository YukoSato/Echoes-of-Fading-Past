********************************************************************************
*Author: Yuko Sato
*Created Data:04/08/2020; Last modified: 04/08/2020
*Objective: Replication Material for Electoral Studies - Appendix
********************************************************************************

//program setup
version 15
clear all
set linesize 80
macro drop _all
set scheme lean2
set more off

//cd 
use "Frantzeskakis_Sato2020_Country-Level", replace
xtset newidn year

*** A.Descriptive Statistics and Information about data
*** Descriptive Statistics (Vote Share of Far-Right) ***************************
tobit farVote dic_time com_time pos_dic pos_com yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue lfh_ipolity2 llog_ref_UNHCR_cap if country != "Italy" & country != "Austria" & country != "Japan", ll(0) ul(100) vce(cluster newid)
keep if e(sample)
gen dem_farVote = farVote if pos_dic==0 & pos_com ==0
gen pos_dic_farVote = farVote if pos_dic ==1  
gen pos_com_farVote = farVote if pos_com ==1

*** Figure 1: Distribution of Vote Share of Far-Right Parties
twoway fpfitci dem_farVote year || scatter dem_farVote year, msymbol(o)/*
*/title("") xtitle("Year (Old Democracies)") ytitle("Far-Right Vote Share (%)") legend(off) name(dem, replace) 
twoway fpfitci pos_dic_farVote year || scatter pos_dic_farVote year, msymbol(x) /*
*/title("") xtitle("Year (Right-Wing Dictatorship)") ytitle("Far-Right Vote Share (%)") legend(off) name(right, replace) 
twoway fpfitci pos_com_farVote year || scatter pos_com_farVote year, /*
*/title("") xtitle("Year (Left Wing Dictatorship)") ytitle("Far-Right Vote Share (%)") legend(off) name(left, replace) 
graph combine left right dem, hole(3) 

*** B. Individual-Level Analysis
use "Frantzeskakis_Sato2020_Individual-Level", replace
xtset id_UN

*** Table B1: Effect of Variables on Far-Right Support (PopuList) **************
*(1) Baseline model
logit far_right_populist dic_18_25_fy poco_18_25_fy pos_dic pos_com age_2-age_8 if far_right_populist_vs >0.03, cluster(id_UN) 
*(2) Individual level variables control 
logit far_right_populist dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec unemp age_2-age_8 if far_right_populist_vs >0.03, cluster(id_UN) 
*(3) With country FE (Only the samples with multiple observations)
logit far_right_populist dic_18_25_fy poco_18_25_fy female pos_sec sec unemp age_2-age_8 country_pop2-country_pop15 if fe ==1 & far_right_populist_vs >0.03, cluster (id_UN)
*(4) With country-year level variables		
bootstrap, cluster(id_UN) reps(200) seed(1234): logit far_right_populist dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 age_2-age_8 if far_right_populist_vs >0.03

*** Figure B1: Predicted probability of Far-Right Party Support (PopuList) *****
set seed 12345
estsimp logit far_right_populist dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec age_2-age_8 if far_right_populist_vs >0.03, cluster(id_UN)

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
	
    *** Right-left dictatorship
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
twoway (line me xaxis) (line lo xaxis, lpattern(solid) lcolor(black%0)) (line hi xaxis, lpattern(solid) lcolor(black%0)) (line me3 xaxis, lcolor(gs8)) (line lo3 xaxis, lpattern(solid) lcolor(black%10)) (line hi3 xaxis, lpattern(solid) lcolor(black%0)) ||/*
*/     rarea hi lo xaxis, color(black%20) || /*
*/     rarea hi3 lo3 xaxis, color(black%20) /*
*/     , xtitle("Years of Authoritarian Exposure (yr 18-25)") ytitle("Pr(Far-Right Voting)") legend(order(1 "Post Right-Wing Dictatorship" 4 "Old Democracies") ring(1) position(6) bmargin(small)) name(Ipr1, replace)
restore	

*** Table B2: Model Specification with Different Operationalization of Country-Level Variables
*** Exclude countries with majoritarian system
*(5) Baseline model
logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com age_2-age_8 if far_right_vs >0.03& major==0, cluster(id_UN) 
*(6) Individual level variables control 
logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec unemp age_2-age_8 if far_right_vs >0.03& major==0, cluster(id_UN) 
*(7) With country FE (Only the samples with multiple observations)
logit far_right dic_18_25_fy poco_18_25_fy female pos_sec sec unemp age_2-age_8 country2-country14 if fe ==1 & far_right_vs >0.03& major==0, cluster (id_UN)
*(8) With country-year level variables		
bootstrap, cluster(id_UN) reps(200) seed(1234): logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 age_2-age_8 if far_right_vs >0.03 & major==0
	
*(9) With majoritarian control	
bootstrap, cluster(id_UN) reps(200) seed(1234): logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec major lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 age_2-age_8 if far_right_vs >0.03
*(10) With control of number of migrants per capita
bootstrap, cluster(id_UN) reps(200) seed(1234): logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue leu_migrantn_per lfh_ipolity2 age_2-age_8 if far_right_vs >0.03

*** Table B3: 1% and 5% Response as Threshold
*** 1% response as threshold
*(11) Baseline model
logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com age_2-age_8 if far_right_vs >0.01, cluster(id_UN) 
*(12) Individual level variables control 
logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec unemp age_2-age_8 if far_right_vs >0.01, cluster(id_UN) 
*(13) With country FE (Only the samples with multiple observations)
logit far_right dic_18_25_fy poco_18_25_fy female pos_sec sec unemp age_2-age_8 country2-country13 if fe ==1 & far_right_vs >0.01, cluster (id_UN)
*(14) With country-year level variables		
bootstrap, cluster(id_UN) reps(200) seed(1234): logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 age_2-age_8 if far_right_vs >0.01

*** 5% response as threshold
*(15) Baseline model
logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com age_2-age_8 if far_right_vs >0.05, cluster(id_UN) 
*(16) Individual level variables control 
logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec unemp age_2-age_8 if far_right_vs >0.05, cluster(id_UN) 
*(17) With country FE (Only the samples with multiple observations)
logit far_right dic_18_25_fy poco_18_25_fy female pos_sec sec unemp age_2-age_8 country2-country13 if fe ==1 & far_right_vs >0.05, cluster (id_UN)
*(18) With country-year level variables		
bootstrap, cluster(id_UN) reps(200) seed(1234): logit far_right dic_18_25_fy poco_18_25_fy pos_dic pos_com female pos_sec sec lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 age_2-age_8 if far_right_vs >0.05

*** Table B4: Effects of Exposure to Authoritarianism Regardless of Their Ideological Origins
*(19) Baseline model
logit far_right aut_18_25_fy pos_aut age_2-age_8 if far_right_vs >0.03, cluster(id_UN) 
*(20) Individual level variables control 
logit far_right aut_18_25_fy pos_aut female pos_sec sec unemp age_2-age_8 if far_right_vs >0.03, cluster(id_UN) 
*(21) With country FE (Only the samples with multiple observations)
logit far_right aut_18_25_fy female pos_sec sec unemp age_2-age_8 country2-country14 if fe ==1 & far_right_vs >0.03, cluster (id_UN)
*(22) With country-year level variables		
bootstrap, cluster(id_UN) reps(200) seed(1234): logit far_right aut_18_25_fy pos_aut female pos_sec sec lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 age_2-age_8 if far_right_vs >0.03

*** C. Country-Level Analysis
use "Frantzeskakis_Sato2020_Country-Level", replace
xtset newidn year

*** Table C1: Model Specification with Different Operationalization of Explanatory Variables
*(23) Without Majoritarian Countries
tobit farVote pos_dic pos_com dic_time com_time yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan" & major ==0, ll(0) ul(100) vce(cluster newid)
*(24) Without Majoritarian Countries with FE
xttobit farVote pos_dic pos_com dic_time com_time yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan"& fe1>1 & major ==0, ll(0) ul(100) 

*(25) With majoritarian control	
tobit farVote pos_dic pos_com dic_time com_time yearsDem major lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan", ll(0) ul(100) vce(cluster newid)
*(26) With majoritarian control with FE
xttobit farVote pos_dic pos_com dic_time com_time yearsDem major lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan" & fe1>1 , ll(0) ul(100) 
*(27) With control of number of migrants per capita
tobit farVote pos_dic pos_com dic_time com_time yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue leu_migrantn_per lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan", ll(0) ul(100) vce(cluster newid)
*(28) With control of number of migrants per capita with FE
xttobit farVote pos_dic pos_com dic_time com_time yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue leu_migrantn_per lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan" & fe1>1, ll(0) ul(100) 

*** Table C2: Authoritarian Past on Far-Right Support
*(29) Using level of repression during authoritarianism
tobit farVote c.mrep_dic##c.yearsDem c.mrep_com##c.yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & aut_2000s==0&country !="Germany" & country != "Italy" & country != "Austria" & country != "Japan"& country !="South Africa" & country !="Mexico" &year >1979, ll(0) ul(100) vce(cluster newid)
*(30) Using level of repression during authoritarianism with FE
xttobit farVote c.mrep_dic##c.yearsDem c.mrep_com##c.yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & aut_2000s==0 &country !="Germany" & country != "Italy" & country != "Austria" & country != "Japan"& country !="South Africa" & country !="Mexico" &year >1979& fe1>1, ll(0) ul(100) 

*(31) Effects of Authoritarianism
tobit farVote c.pos_aut##c.yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan", ll(0) ul(100) vce(cluster newid)
*(31) Effects of Authoritarianism with FE
xttobit farVote c.pos_aut##c.yearsDem lgol_enpp1 llog_dpi_mdmh lgdpgro limf_ue llog_ref_UNHCR_cap lfh_ipolity2 if regimeny==100 & country != "Italy" & country != "Austria" & country != "Japan"& fe1>1, ll(0) ul(100) 	
	
exit
