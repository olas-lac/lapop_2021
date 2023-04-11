***************************************************************************
******************* LAPOP Income Imputation ***************
***************************************************************************

****** Setting the working directory
clear all
cd "C:\Users\jesse\Desktop\Data Projects\OLAS\lapop_2021\" 


****** Loading the data
use "lapop_2021_income.dta", replace 



****** Loop to interpolate the income by country (I store the results in dta files for each country)

levelsof pais_number, local(levels) 
 foreach l of local levels {
  keep if pais_number == `l'

****** Creating relevant variables

*  We take the logs of the interval boundaries

gen log_min_incomeusd =log(income_min_usd)
gen log_max_incomeusd =log(income_max_usd)

**** We manipulate the data to create some explanatory variables for income (I'm really not thinking about causality here, but just prediction)

* The proportion of children under 13 in the house
gen childrenratio= q12bn/q12c

* A dummy variable for respondents who declare white ethnicity 
gen white=0
replace white=1 if etid==1
replace white=. if etid==.a
replace white=. if etid==.b

* A dummy variable for respondents who declare mixed ethnicity
gen mixed=0
replace mixed=1 if etid==2
replace mixed=. if etid==.a
replace mixed=. if etid==.b


* A dummy variable for respondents who declare university degree
gen higheduc=0
replace higheduc=1 if ed>=16
replace higheduc=. if ed==.a
replace higheduc=. if ed==.b


*** Interval regression for income

intreg log_min_incomeusd log_max_incomeusd higheduc q2 ed childrenratio r3 r4 r6 r7 r15 r16 white mixed, ///
het(higheduc ed estratopri) 

*** Multiple imputation process following Rios-Avila et al (2022)

intreg_mi ilincome_usd, seed(10) reps(100)

gen ilogincome_usd = . 
tempfile tosave
save `tosave'

mi import wide, imputed(ilogincome_usd=  ilincome_usd* ) 
mi passive: gen iincome_usd = exp(ilogincome_usd) 

**** Average income estimates 
egen iinc = rmean(_1_iincome_usd - _100_iincome_usd)

cd "C:\Users\jesse\Desktop\Data Projects\OLAS\lapop_2021\stata_income_generation\" 
  save "lapop_clean_2021pais`l'.dta", replace

 cd  "C:\Users\jesse\Desktop\Data Projects\OLAS\lapop_2021\" 
  use "lapop_2021_income.dta", replace 
 
}

 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 
 