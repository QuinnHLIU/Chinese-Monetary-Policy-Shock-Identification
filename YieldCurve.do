**************************************************************
***********   MPS Responses on the Yield Curve   *************
***********       Haiqin Liu, May-03, 2022       *************
**************************************************************
clear all
cd "E:\Dropbox\UMP_Paper/Git"


**************************************************
* 0. Setup Shock Series
/*************************************************
use CleanData/mpshock,replace 

//the high freq shock contains several data points on conventional days, yet they're totally outliers
//drop them altogether. restrict the analysis to innovative events only 
drop pcapbocbroad* pcapbocnarrow* pcaconvbroad* pcaconvnarrow* pcaRRRbroad* pcaRRRnarrow*
findname pcaunconvbroad* pcaunconvnarrow*,local(vs)
foreach v in `vs'{
    local vn = subinstr("`v'","unconv","pboc",.)
	rename `v' `vn'
}

keep  date *days /// indicators of event types 
	/// X = MPS_t  
	/// Part 1. high frequency shocks
	pcapbocbroad* pcapbocnarrow* pcaMLFbroad* pcaMLFnarrow* pcaLPRbroad* pcaLPRnarrow* pcacontrolbroad* pcacontrolnarrow* ///
	broad* narrow* /// underlying high freq surprises 
	/// Part 2. daily shocks
	pcapbocirs* pcapbocr007 pcaconv* pcaunconv* pcaMLFirs* pcaMLFr007 pcaLPRirs* pcaLPRr007 pcaRRR* pcaLR* ///
	dr007 dirs* /// underlying daily surprises 
	/// Y = daily changes in yields/forward rates, nominal & real (adjusted by inflation)
	drawtreasury* dadjtreasury* drawforward* dadjforward* 
	
order date *days /// indicators of event types 
	/// X = MPS_t  
	/// Part 1. high frequency shocks
	pcapbocbroad* pcapbocnarrow* pcaMLFbroad* pcaMLFnarrow* pcaLPRbroad* pcaLPRnarrow* pcacontrolbroad* pcacontrolnarrow* ///
	broad* narrow* /// underlying high freq surprises 
	/// Part 2. daily shocks
	pcapbocirs* pcapbocr007 pcaconv* pcaunconv* pcaMLFirs* pcaMLFr007 pcaLPRirs* pcaLPRr007 pcaRRR* pcaLR* ///
	dr007 dirs* /// underlying daily surprises 
	/// Y = daily changes in yields/forward rates, nominal & real (adjusted by inflation)
	drawtreasury* dadjtreasury* drawforward* dadjforward* 

save CleanData/mpshock,replace
*/

**************************************************
* 1. High Frequency Shocks
**************************************************
use CleanData/mpshock, clear

gen intname=""
gen inttype=""

tsset date,d

foreach mp in pboc MLF LPR{
foreach wind in narrow broad{
foreach xx in irs1y irs5y irsr0071y irsr0075y r0071y r0075y{
    
	local v "`wind'`xx'" 
		
	est clear
	
	foreach s in raw adj{  // Treasury yields 
	
		foreach mm in _b l1 l2 u1 u2{
			qui gen `mp'`v'`s'`mm'=.
		}
		
		local k=1
		foreach tr in 6m 9m 1y 2y 3y 4y 5y 6y 7y 8y 9y 10y 15y 20y{
			local varb "d`s'treasury`tr'"
			local tenor=upper("`tr'")
			local labn="`tenor'"+" Treasury Yields"
			
			lab var `varb' "`labn'"
			qui replace intname="`labn'" if _n==`k'
			qui replace inttype="Treasury" if _n==`k'
			
			cap eststo Tr`s'`tenor':reg `varb' pca`mp'`v' if `mp'days==1, r
			if _rc==0{
				qui ereturn display,level(90)
				mat define res=r(table)
				qui replace `mp'`v'`s'_b=res[1,1] if _n==`k'
				qui replace `mp'`v'`s'l1=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u1=res[6,1] if _n==`k'		
				qui ereturn display,level(68)
				mat define res=r(table)
				qui replace `mp'`v'`s'l2=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u2=res[6,1] if _n==`k'	
			}
			else{
				di "`varb' Failed"
			}
			
			local k=`k'+1  // switch line only if another product--tenor 
		}
		
		foreach tr in 6m 1y 2y 3y 4y 5y 6y 7y 8y 9y 10y 15y 20y{ 
		    local varb="d`s'forward`tr'"
			local tenor=upper("`tr'")
			local labn="`tenor'"+" Treasury Forward"
			lab var `varb' "`labn'"
			qui replace intname="`labn'" if _n==`k'
			qui replace inttype="Forward" if _n==`k'
			
			cap eststo Fr`s'`tenor':reg `varb' pca`mp'`v' if `mp'days==1,r
			if _rc==0{
				
				qui ereturn display,level(90)
				mat define res=r(table)
				qui replace `mp'`v'`s'_b=res[1,1] if _n==`k'
				qui replace `mp'`v'`s'l1=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u1=res[6,1] if _n==`k'		
				qui ereturn display,level(68)
				mat define res=r(table)
				qui replace `mp'`v'`s'l2=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u2=res[6,1] if _n==`k'	
			}
			else{
				di "`varb' Failed"
			}
			local k = `k'+1
		}
	}
	
	local mpn:var lab pca`mp'`v'
	qui esttab Trraw* Frraw* using "OutPut/YieldCurve/OLS/HighFreq/Table/`mp'`v'.csv",replace nocons ///
	star(* 0.1 ** 0.05 *** 0.01) stats(N r2,labels("Obs." "R\$^2\$")) ci l ti("`mpn':Nominal")
	qui esttab Tradj* Fradj* using "OutPut/YieldCurve/OLS/HighFreq/Table/`mp'`v'.csv",append  nocons ///
	star(* 0.1 ** 0.05 *** 0.01) stats(N r2,labels("Obs." "R\$^2\$")) ci l ti("`mpn':Real")
	}
}
}

//generate figure 2
keep intname inttype *_b *l1 *l2 *u1 *u2 
drop if intname==""
gen tic = _n ,a(intname)
local handlab = "lab define termlab "
local Nobs=_N
forv k=1/`Nobs'{  // note:only the label for Treasury yield is correct
	local labstr=intname[`k']
	local labstr=subinstr("`labstr'"," Treasury Yields","",.)
	local labstr=subinstr("`labstr'"," Treasury Forward","",.)
	local handlab `"`handlab'`k' "{stSerif:`labstr'}" "'
}
`handlab',replace
lab values tic termlab

foreach mp in pboc MLF LPR{
foreach wind in broad narrow{ //
foreach xx in irs1y irs5y irsr0071y irsr0075y r0071y r0075y{
local v "`wind'`xx'" 
preserve 
keep if inttype=="Treasury" &tic>=4&tic<=17
tw (line `mp'`v'raw_b tic,lc(blue) lw(medthick)) (line `mp'`v'adj_b tic,lc(blue) lw(medthick) lp(dash)) ///
   (line `mp'`v'rawl1 tic,lc(black)) (line `mp'`v'adjl1 tic,lc(black) lp(dash)) ///
   (line `mp'`v'rawu1 tic,lc(black)) (line `mp'`v'adju1 tic,lc(black) lp(dash)) ///
   ,graphr(c(white) margin(l+10 r=20  b-1 t-1)) plotr(c(white)) xsiz(20) ysiz(10) aspect(.45) name(mps,replace)  ///
   xlab(4(1)17,valuelabel labs(60pt) angle(45)) xti("")  ///
   ylab(,labs(60pt) format(%2.1fc) angle(horizontal)) ///
   leg(order(1 "{stSerif:Nominal}" 2 "{stSerif:Real}" 3 "{stSerif:90% CI}") r(1) siz(60pt))
qui gr export "OutPut/YieldCurve/OLS/HighFreq/Figure/`mp'`v'.png",replace  
restore 
}
}
}


  
**************************************************
* 2. Daily Shock 
**************************************************
use CleanData/mpshock, clear

gen intname=""
gen inttype=""

tsset date,d

foreach mp in pboc conv unconv{
foreach v in irs irsr0071y irsr0075y r007{
		
	est clear
	
	foreach s in raw adj{  // Treasury yields 
	
		foreach mm in _b l1 l2 u1 u2{
			qui gen `mp'`v'`s'`mm'=.
		}
		
		local k=1
		foreach tr in 6m 9m 1y 2y 3y 4y 5y 6y 7y 8y 9y 10y 15y 20y{ // 1m 2m 3m 30y 40y 50y
			local varb "d`s'treasury`tr'"
			local tenor=upper("`tr'")
			local labn="`tenor'"+" Treasury Yields"
			
			lab var `varb' "`labn'"
			qui replace intname="`labn'" if _n==`k'
			qui replace inttype="Treasury" if _n==`k'
			
			cap eststo Tr`s'`tenor':reg `varb' pca`mp'`v' if `mp'days==1,r //newey
			if _rc==0{
				qui ereturn display,level(90)
				mat define res=r(table)
				qui replace `mp'`v'`s'_b=res[1,1] if _n==`k'
				qui replace `mp'`v'`s'l1=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u1=res[6,1] if _n==`k'		
				qui ereturn display,level(68)
				mat define res=r(table)
				qui replace `mp'`v'`s'l2=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u2=res[6,1] if _n==`k'	
			}
			else{
				di "`varb' Failed"
			}
			
			local k=`k'+1  // switch line only if another product--tenor 
		}
	
		foreach tr in 6m 1y 2y 3y 4y 5y 6y 7y 8y 9y 10y 15y 20y{ 
		    local varb="d`s'forward`tr'"
			local tenor=upper("`tr'")
			local labn="`tenor'"+" Treasury Forward"
			lab var `varb' "`labn'"
			qui replace intname="`labn'" if _n==`k'
			qui replace inttype="Forward" if _n==`k'
			
			cap eststo Fr`s'`tenor':reg `varb' pca`mp'`v' if `mp'days==1,r
			if _rc==0{
				
				qui ereturn display,level(90)
				mat define res=r(table)
				qui replace `mp'`v'`s'_b=res[1,1] if _n==`k'
				qui replace `mp'`v'`s'l1=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u1=res[6,1] if _n==`k'		
				qui ereturn display,level(68)
				mat define res=r(table)
				qui replace `mp'`v'`s'l2=res[5,1] if _n==`k'
				qui replace `mp'`v'`s'u2=res[6,1] if _n==`k'	
			}
			else{
				di "`varb' Failed"
			}
			local k = `k'+1
		}
	}
	
	local mpn:var lab pca`mp'`v'
	qui esttab Trraw* Frraw* using "OutPut/YieldCurve/OLS/Daily/Table/`mp'`v'.csv",replace nocons ///
	star(* 0.1 ** 0.05 *** 0.01) stats(N r2,labels("Obs." "R\$^2\$")) ci l ti("`mpn':Nominal")
	qui esttab Tradj* Fradj* using "OutPut/YieldCurve/OLS/Daily/Table/`mp'`v'.csv",append  nocons ///
	star(* 0.1 ** 0.05 *** 0.01) stats(N r2,labels("Obs." "R\$^2\$")) ci l ti("`mpn':Real")
	}
}

keep intname inttype *_b *l1 *l2 *u1 *u2 
drop if intname==""
gen tic = _n ,a(intname)
local handlab = "lab define termlab "
local Nobs=_N
forv k=1/`Nobs'{  // note:only the label for Treasury yield is correct
	local labstr=intname[`k']
	local labstr=subinstr("`labstr'"," Treasury Yields","",.)
	local labstr=subinstr("`labstr'"," Treasury Forward","",.)
	local handlab `"`handlab'`k' "{stSerif:`labstr'}" "'
}
`handlab',replace
lab values tic termlab

foreach v in irs irsr0071y irsr0075y r007{ //  fwd fut2y fut5y
preserve 
keep if inttype=="Treasury"

tw (line pboc`v'raw_b tic,lc(blue) lw(medthick)) (line pboc`v'adj_b tic,lc(blue) lw(medthick) lp(dash)) ///
   (line pboc`v'rawl1 tic,lc(black)) (line pboc`v'adjl1 tic,lc(black) lp(dash)) ///
   (line pboc`v'rawu1 tic,lc(black)) (line pboc`v'adju1 tic,lc(black) lp(dash)) ///
   ,graphr(c(white) margin(l+10 r=20  b-1 t-1)) plotr(c(white)) xsiz(20) ysiz(14) name(mps,replace)  ///
   xlab(1(1)14,valuelabel labs(60pt)) xti("")  ///
   ylab(,labs(60pt) format(%2.1fc) angle(horizontal))  /// 
   ti("{stSerif:MPS Baseline}",c(black) siz(70pt)) ///
   leg(order(1 "{stSerif:Nominal Rate}" 2 "{stSerif:Real Rate}" 3 "{stSerif:90% CI}") r(1) siz(60pt))

   forv p=1/1{
	local digit 1
	
	qui egen maxirf = rowmax(conv`v'rawu1 unconv`v'rawu1 conv`v'adju1 unconv`v'adju1)  // 
	qui su maxirf
	local `v'max = r(max)
	if ``v'max' == 0.1{
		local `v'max = ceil(r(max)*100)/100
		local digit = 2
	}
	qui egen minirf = rowmin(conv`v'rawl1 unconv`v'rawl1 conv`v'adjl1 unconv`v'adjl1)  // 
	qui su minirf
	local `v'min = r(min)
	if ``v'min' == -0.1{
		local `v'min = floor(r(min)*100)/100
		local digit = 2
	}
	drop maxirf minirf 
	local `v'sep = round(10*(``v'max'-``v'min')/4)/10  // approx. 4 tickers
	if ``v'sep' == 0{
		local `v'sep = round(100*(``v'max'-``v'min')/4)/100
		local digit = 2
	}
	
	local `v'lim = "``v'min'(``v'sep')``v'max'"
	
	/*
	local ntic = floor((``v'max'-``v'min')/``v'sep')
	if ``v'min'+``v'sep'*(`ntic'-1)<``v'max'{ 
		local `}v'max=``v'min'+``v'sep'*`ntic'
	
	foreach xx in nomi { //real
		foreach vv in conv unconv{
			foreach mm in b lb ub{
				replace `xx'`vv'`v'_`mm'=``v'min' if `xx'`vv'`v'_`mm'<``v'min'
				replace `xx'`vv'`v'_`mm'=``v'max' if `xx'`vv'`v'_`mm'>``v'max'
			}
		}
	}
	*/
	
	if ``v'min'==.{
		local `v'lim ""
	}
	
	di "``v'lim'"
	
	local intp = int(``v'min')
	if length(subinstr("`intp'","-","",.)) > 1 & `digit'>0{
		local dsm = `digit' -1
		local `v'fmt = "format(%5.`dsm'fc)" // integer part > 2 digit --> no decimals
	}
	else{
		local `v'fmt = "format(%5.`digit'fc)"
	}
	}

tw (line conv`v'raw_b tic,lc(blue) lw(medthick)) (line conv`v'adj_b tic,lc(blue) lw(medthick) lp(dash)) ///
   (line conv`v'rawl1 tic,lc(black)) (line conv`v'adjl1 tic,lc(black) lp(dash)) ///
   (line conv`v'rawu1 tic,lc(black)) (line conv`v'adju1 tic,lc(black) lp(dash)) ///
   , graphr(c(white)) plotr(c(white)) leg(off) ///
   xlab(1(2)14,valuelabel labs(70pt)) xti("")  ///
   ylab("``v'lim'",``v'fmt' labs(70pt) angle(horizontal)) ///
   aspectratio(.56) xsiz(20) ysiz(13.5) yticks("``v'lim'",labs(minuscule)) ///
   ti("{stSerif:MPS Conventional}",c(black) siz(80pt)) name(conv,replace)
   
tw (line unconv`v'raw_b tic,lc(blue) lw(medthick)) (line unconv`v'adj_b tic,lc(blue) lw(medthick) lp(dash)) ///
   (line unconv`v'rawl1 tic,lc(black)) (line unconv`v'adjl1 tic,lc(black) lp(dash)) ///
   (line unconv`v'rawu1 tic,lc(black)) (line unconv`v'adju1 tic,lc(black) lp(dash)) ///
   , graphr(c(white)) plotr(c(white)) leg(off) ///
   xlab(1(2)14,valuelabel labs(70pt)) xti("")  ///
   ylab("``v'lim'",``v'fmt' labs(70pt) angle(horizontal)) ///
   aspectratio(.56) xsiz(20) ysiz(13.5) yticks("``v'lim'",labs(minuscule))  ///
   ti("{stSerif:MPS Innovative}",c(black) siz(80pt)) name(unconv,replace)
   
gr combine conv unconv,imargin(tiny) r(1) graphr(c(white)) plotr(c(white)) ///
   name(mps2,replace) xsiz(20) ysiz(6)
gr combine mps mps2,imargin(zero) r(2) graphr(c(white)) plotr(c(white)) ///
   name(`v',replace) xsiz(20) ysiz(16)
local vn = strupper("`v'")
qui gr export "OutPut/YieldCurve/OLS/Daily/Figure/`vn'.png",width(3000) replace     
restore
}


