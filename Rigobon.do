********************************************************************************
***********            MPS on Yield Curve: Rigobon Method          *************
***********                Haiqin Liu, Aug-13, 2023                *************
********************************************************************************
clear all
cd "E:\Dropbox\UMP_Paper/Git"


***** First try a Stata-based program. Obviously the SEs are not adjusted ******
capture program drop bootRig
program define bootRig, eclass
	args depvarb indepvarb
	// depvarb=Treasury at various maturities; indepvarb=extrapolated MPShock
	// then you can't do this for MLF,RRR etc separately or the "control" is not pure
	correlate `depvarb' `indepvarb' if pbocdays==1, c // R1=treatment
	mat omega1 = r(C)
	sca N_omega1 = r(N)
	
	*** R2=control group 
	correlate `depvarb' `indepvarb' if controldays==1, c
	mat omega2 = r(C)
	sca N_omega2 = r(N)
	
	* Take difference of variance-covariances on FOMC vs. nonFOMC days
	mat omega=omega1-omega2
	*sca bPATH = omega[1,2]/omega[1,1]
	*** Alternative estimator *** (comment line above and uncomment line below)
	sca bPATH = omega[2,2]/omega[1,2]
	
	tempname bb
	matrix `bb'  = bPATH
	mat colnames `bb'  = Path
	mat list `bb'
	ereturn post `bb'
	ereturn local cmd="bootstrap"  // resample and generate a distri. of bPATH
	ereturn local depvar="`depvarb'"
end

set seed 20230713
global reps1=500
global level1 90 
global level2 68


//******************************* 60-Min Window ******************************//
use CleanData/mpshock, clear

keep if unconvdays==1 | pbocdays==0 
drop pbocdays
rename unconvdays pbocdays
tab pbocdays controldays // 116 PBOC + 5601 controlss 

* (1) extrapolate the PCA to non-pboc days 
foreach wind in broad narrow{
foreach v in irs1y irs5y irsr0071y irsr0075y r0071y r0075y{  
		
	if "`v'"=="irs1y"{
	    local underlying = "`wind'FR0071Y `wind'Shibor3M6M `wind'Shibor3M9M `wind'Shibor3M1Y"
	}
	else if "`v'"=="irs5y"{
	    local underlying = "`wind'FR0071Y `wind'Shibor3M6M `wind'Shibor3M9M `wind'Shibor3M1Y `wind'FR0075Y"
	}
	else if "`v'"=="r0071y"{
	    local underlying = "dr007 `wind'FR0071Y `wind'Shibor3M6M `wind'Shibor3M9M `wind'Shibor3M1Y"
	}
	else if "`v'"=="r0075y"{
	    local underlying = "dr007 `wind'FR0071Y `wind'Shibor3M6M `wind'Shibor3M9M `wind'Shibor3M1Y `wind'FR0075Y"
	}
	else if "`v'"=="irsr0071y"{
	    local underlying = "`wind'FR0071Y `wind'FR0076M `wind'FR0079M"
	}
	else if "`v'"=="irsr0075y"{
	    local underlying = "`wind'FR0071Y `wind'FR0076M `wind'FR0079M `wind'FR0075Y"
	}
	//meaning assuming linearity in the underlying interest rate so reconstruct the 
	//series on non-PBOC days. If there's no shock in any of the interest rates, there's no shock in the series
	regress pcapboc`wind'`v' `underlying' if pbocdays==1, r
	foreach vv in `underlying'{
	    qui gen part`vv' = _b[`vv'] * `vv'  //coef x surprise
	}
	qui egen pcapboc`wind'`v'_extrap = rowtotal(part*),missing //in case only part of the series exist 
	drop part* 
}
}

* (2) run BootRig program 
gen intname=""
gen inttype=""
tsset date,d
foreach mp in pboc{  // MLF LPR  
foreach wind in narrow broad{ // broad measure never works
foreach xx in irs1y irs5y irsr0071y irsr0075y r0071y r0075y{
	
	local v "`wind'`xx'"
	
	est clear
	
	foreach s in raw adj{  // Treasury yields 
		
		foreach mm in _b l1 u1 l2 u2{
		    qui cap gen `mp'`v'`s'`mm' = .
		}
		
		local k=1
		foreach tr in 6m 9m 1y 2y 3y 4y 5y 6y 7y 8y 9y 10y 15y 20y{
			
			local varb "d`s'treasury`tr'"
			local tenor=upper("`tr'")
			local labn="`tenor'"+" Treasury Yields"
			
			lab var `varb' "`labn'"
			qui replace intname="`labn'" if _n==`k'
			qui replace inttype="Treasury" if _n==`k'
			
			cap eststo `s'`tenor'Tr:bootstrap _b, strata(pbocdays) reps($reps1 ) nowarn: ///
												  bootRig `varb' pca`mp'`v'_extrap 
			if _rc==0{  // exactly the same structure as OLS table 
			    di "`varb' Success"
			    ereturn display, level($level1 ) 
				qui matrix res=r(table)
				qui replace `mp'`v'`s'_b=res[1,1] if _n==`k'
				qui replace `mp'`v'`s'l1=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u1=res[6,1] if _n==`k'
			    qui ereturn display, level($level2 ) 
				qui matrix res=r(table)	
				qui replace `mp'`v'`s'l2=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u2=res[6,1] if _n==`k'
			}
			else{
				di "`varb' Failed"
			}
			local k=`k'+1  
		}
	}
	
	local mpn:var lab pca`mp'`v'
	qui esttab raw* using "OutPut/YieldCurve/Rigobon/BootRig`mp'`v'highfreq.csv",replace nocons ///
	star(* 0.1 ** 0.05 *** 0.01) stats(N r2,labels("Obs." "R\$^2\$")) se l ti("`mpn':Nominal")
	qui esttab adj* using "OutPut/yieldcurve/rigobon/BootRig`mp'`v'highfreq.csv",append  nocons ///
	star(* 0.1 ** 0.05 *** 0.01) stats(N r2,labels("Obs." "R\$^2\$")) se l ti("`mpn':Real")
		
	}
}
}


//******************************* Daily Window *******************************//
use CleanData/mpshock, clear

* (1) extrapolate the PCA to non-pboc days 
foreach v in irs irsr0071y irsr0075y r007{  
		
		 if "`v'"=="irs"{
	    local underlying = "dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y"
	}
	else if "`v'"=="irsr0071y"{
	    local underlying = "dr007 dirsr0071y dirsr0076m dirsr0079m"
	}
	else if "`v'"=="irsr0075y"{
	    local underlying = "dr007 dirsr0071y dirsr0076m dirsr0079m dirsr0075y"
	}
	else if "`v'"=="r007"{ // due to data availability concern
	    local underlying = "dr007 dirsr0071y"
	}
	//meaning assuming linearity in the underlying interest rate so reconstruct the 
	//series on PBOC days. If there's no shock in any of the interest rates, there's no shock in the series
	regress pcapboc`v' `underlying' if pbocdays==1, r
	foreach vv in `underlying'{
	    qui gen part`vv' = _b[`vv'] * `vv'  //coef x surprise
	}
	qui egen pcapboc`wind'`v'_extrap = rowtotal(part*),missing //in case only part of the series exist 
	drop part* 
}


//stratify the bootstrap resampling by PBOCdays,so as to get the same
//number of non-missing obs in each sample drawn in the bootstrap. (like to make you draw both control and treat each time?)
gen stratRF3 = 0 if date<mdy(1,1,2015)
replace stratRF3 = 1 if date>=mdy(1,1,2015) & pbocdays==1
replace stratRF3 = 2 if date>=mdy(1,1,2015) & pbocdays==0

* (2) run BootRig program 
gen intname=""
gen inttype=""
tsset date,d
foreach mp in pboc conv unconv{  // 
preserve 
keep if `mp'days==1 | controldays==1  // restrict sample to Treat and Control; avoid overlaps 
foreach v in irs{  // irsr0071y irsr0075y r007
		
	est clear
	
	foreach s in raw adj{  // Treasury yields 
		
		foreach mm in _b l1 u1 l2 u2{
		    qui cap gen `mp'`v'`s'`mm' = .
		}
		
		local k=1
		foreach tr in 6m 9m 1y 2y 3y 4y 5y 6y 7y 8y 9y 10y 15y 20y{
			local varb "d`s'treasury`tr'"
			local tenor=upper("`tr'")
			local labn="`tenor'"+" Treasury Yields"
			
			lab var `varb' "`labn'"
			qui replace intname="`labn'" if _n==`k'
			qui replace inttype="Treasury" if _n==`k'
			

			eststo `s'`tenor'Tr:bootstrap _b, strata(stratRF3) reps($reps1 ) nowarn: ///
								 bootRig `varb' pcapboc`v'_extrap
			if _rc==0{  // exactly the same structure as OLS table 
			    di "`varb' Success"
			    ereturn display, level($level1 ) 
				qui matrix res=r(table)
				qui replace `mp'`v'`s'_b=res[1,1] if _n==`k'
				qui replace `mp'`v'`s'l1=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u1=res[6,1] if _n==`k'
			    qui ereturn display, level($level2 ) 
				qui matrix res=r(table)	
				qui replace `mp'`v'`s'l2=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u2=res[6,1] if _n==`k'
			}
			else{
				di "`varb' Failed"
			}
			local k=`k'+1  
		}
	}
	
	local mpn:var lab pca`mp'`v'
	qui esttab raw* using "OutPut/YieldCurve/Rigobon/BootRig`mp'`v'.csv",replace nocons ///
	star(* 0.1 ** 0.05 *** 0.01) stats(N r2,labels("Obs." "R\$^2\$")) se l ti("`mpn':Nominal")
	qui esttab adj* using "OutPut/YieldCurve/Rigobon/BootRig`mp'`v'.csv",append  nocons ///
	star(* 0.1 ** 0.05 *** 0.01) stats(N r2,labels("Obs." "R\$^2\$")) se l ti("`mpn':Real")
	}
restore 
}


 
 

//******************************* High Frequen *******************************//
use CleanData/mpshock, clear

keep if unconvdays==1 | controldays==1 
drop pbocdays
rename unconvdays pbocdays

* (1) extrapolate the PCA to control days 
foreach wind in broad narrow{
foreach v in irs1y irs5y irsr0071y irsr0075y r0071y r0075y{  
		
	if "`v'"=="irs1y"{
	    local underlying = "`wind'FR0071Y `wind'Shibor3M6M `wind'Shibor3M9M `wind'Shibor3M1Y"
	}
	else if "`v'"=="irs5y"{
	    local underlying = "`wind'FR0071Y `wind'Shibor3M6M `wind'Shibor3M9M `wind'Shibor3M1Y `wind'FR0075Y"
	}
	else if "`v'"=="r0071y"{
	    local underlying = "dr007 `wind'FR0071Y `wind'Shibor3M6M `wind'Shibor3M9M `wind'Shibor3M1Y"
	}
	else if "`v'"=="r0075y"{
	    local underlying = "dr007 `wind'FR0071Y `wind'Shibor3M6M `wind'Shibor3M9M `wind'Shibor3M1Y `wind'FR0075Y"
	}
	else if "`v'"=="irsr0071y"{
	    local underlying = "`wind'FR0071Y `wind'FR0076M `wind'FR0079M"
	}
	else if "`v'"=="irsr0075y"{
	    local underlying = "`wind'FR0071Y `wind'FR0076M `wind'FR0079M `wind'FR0075Y"
	}
	//meaning assuming linearity in the underlying interest rate so reconstruct the 
	//series on non-PBOC days. If there's no shock in any of the interest rates, there's no shock in the series
	regress pcapboc`wind'`v' `underlying' if pbocdays==1, r
	foreach vv in `underlying'{
	    qui gen part`vv' = _b[`vv'] * `vv'  //coef x surprise
	}
	qui egen pcapboc`wind'`v'_extrap = rowtotal(part*),missing //in case only part of the series exist 
	drop part* 
}
}

//tw line pcapbocbroad*extrap date
gen year=year(date)
gen stratRF3=0 if year(date)<=2015
replace stratRF3=1 if year(date)>=2015 & pbocdays==1 
replace stratRF3=2 if year(date)>=2015 & pbocdays==0 
egen nummiss=rowmiss(dr007 broadFR0071Y broadShibor3M6M broadShibor3M9M broadShibor3M1Y) 
drop if nummiss >0  //44 

keep date year pbocdays dr007 broadFR0071Y broadShibor3M6M broadShibor3M9M broadShibor3M1Y ///
 pcapbocirs pcapbocbroadirs1y_extrap dadjtreasury* drawtreasury* pcapbocbroadirs1y stratRF3 //inflation
order date year pbocdays dr007 broadFR0071Y broadShibor3M6M broadShibor3M9M broadShibor3M1Y ///
 pcapbocirs pcapbocbroadirs1y_extrap dadjtreasury* drawtreasury* pcapbocbroadirs1y stratRF3 //inflation 
export delimited CleanData/dataForMatlab-PBOCHighFreq.csv,replace



//******************************* Daily Window *******************************//
use CleanData/mpshock, clear

* (1) extrapolate the PCA to non-pboc days 
foreach v in irs irsr0071y irsr0075y r007{  
		
		 if "`v'"=="irs"{
	    local underlying = "dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y"
	}
	else if "`v'"=="irsr0071y"{
	    local underlying = "dr007 dirsr0071y dirsr0076m dirsr0079m"
	}
	else if "`v'"=="irsr0075y"{
	    local underlying = "dr007 dirsr0071y dirsr0076m dirsr0079m dirsr0075y"
	}
	else if "`v'"=="r007"{ // due to data availability concern
	    local underlying = "dr007 dirsr0071y"
	}
	//meaning assuming linearity in the underlying interest rate so reconstruct the 
	//series on PBOC days. If there's no shock in any of the interest rates, there's no shock in the series
	regress pcapboc`v' `underlying' if pbocdays==1, r
	foreach vv in `underlying'{
	    qui gen part`vv' = _b[`vv'] * `vv'  //coef x surprise
	}
	qui egen pcapboc`wind'`v'_extrap = rowtotal(part*),missing //in case only part of the series exist 
	drop part* 
}


gen year=year(date)
gen stratRF3 = 0 if date<mdy(1,1,2015)
replace stratRF3 = 1 if date>=mdy(1,1,2015) & pbocdays==1
replace stratRF3 = 2 if date>=mdy(1,1,2015) & pbocdays==0

egen nummiss=rowmiss(dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y) 
drop if nummiss >0

keep date year pbocdays dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y ///
  pcapbocirsr0071y_extrap pcapbocirs_extrap dadjtreasury* drawtreasury* dadjforward* drawforward* pcapbocirs stratRF3 //inflation
order date year pbocdays dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y ///
  pcapbocirsr0071y_extrap pcapbocirs_extrap dadjtreasury* drawtreasury* dadjforward* drawforward* pcapbocirs stratRF3 //inflation 
export delimited CleanData/dataForMatlab-PBOCDaily.csv,replace











