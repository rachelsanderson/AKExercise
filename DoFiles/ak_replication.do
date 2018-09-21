********************************************************************************
* Project:   AK Replication Code
* Analyst:   Rachel Anderson
* Date:      21 Sep 2018 10:20:31
* This file: ak_replication.do
* Description: Code for Sims PS1
********************************************************************************

version 12
clear all
macro drop _all
set more off
// set graphics off

//Locals for saving figures, TeX files, etc.

cd "/Users/rachelanderson/Desktop/Dropbox/515/AKExercise" 
local fig "/Users/rachelanderson/Desktop/Dropbox/515/AKExercise/Figures"
local data "/Users/rachelanderson/Desktop/Dropbox/515/AKExercise/Data"
local tex "/Users/rachelanderson/Desktop/Dropbox/515/AKExercise/TeX"

********************************************************************************

//load data 
import delimited "`data'/akdata.csv" 
drop v1

//Program for making education dummies
capture program drop make_educ_dum
program define make_educ_dum
	local i = 0
	while `i' <= 20 {
		gen educ`i' = educ == `i'
		label variable educ`i' "`i'"
	local i = `i' + 1
	}
end

make_educ_dum

//Program for making yob dummies
capture program drop make_yob_dum
program define make_yob_dum
	local i = 30
	while `i' <= 39 {
		gen yob`i' = yob == `i'
	local i = `i' + 1
	}
end

make_yob_dum

//Create pob dummies
quietly tab pob, gen(dpob)
drop dpob1

//Generate highschool
gen highSchool = 0
replace highSchool = 1 if educ >= 12

//OLS Regression

cd `tex'
eststo ols: quietly reg logwage educ1-educ20 yob31-yob39 dpob*, r  
esttab ols using ak_ols.tex, drop(yob* dpob*) replace se booktabs nonum gaps nostar f

cd `fig'
coefplot ols, vertical drop(dpob* yob* _cons) coeflabels(educ1 = "1") xline(12) xtitle("Years of education")
graph export coefplot_ols.pdf, replace

predict yhatOLS, xb
tabstat yhatOLS, statistics(mean) by(educ)
graph bar yhatOLS, over(educ) ytitle("Mean predicted log wage") 

graph export logwage_ols.pdf, replace

gen wageOLS = exp(yhatOLS)
tabstat wageOLS, statistics(mean) by(educ)
graph bar wageOLS, over(educ) ytitle("Mean predicted wage")
graph export wage_ols.pdf, replace

//IV with x=[educ] 
cd `tex'

eststo clear
eststo iv1: ivreg2 logwage (educ = i.qob), r first savefirst savefprefix(first)
matrix first1 = e(first) 

//IV with X=[educ highSchool] 
eststo iv2: ivreg2 logwage (educ highSchool = i.qob), r first savefirst savefprefix(first)
matrix first2 = e(first)

//IV with x=[educ], with pob dummies etc. 
eststo iv3: quietly ivreg2 logwage (educ = i.qob) yob31-yob39 dpob*, r
eststo iv4: quietly ivreg2 logwage (educ highSchool = i.qob) yob31-yob39 dpob*, r

esttab iv1 iv2 iv3 iv4 using iv.tex, drop(dpob* yob*) replace se longtable nodepvars booktabs gaps f nostar

