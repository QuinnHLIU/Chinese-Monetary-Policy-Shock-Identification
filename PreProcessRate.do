***************************************************************************************************
********* Preprocess financial prices to feed into R imputed PCA. Haiqin Liu, Aug-10, 2023 ********
********* Input:Minute-level event timing; OutPut: underlying interest rates for each spec ********
***************************************************************************************************
global root "E:\Dropbox\UMP_Paper"
cd $root 

/************ 1. Retrieve IRS Transaction Daily by Minutes (2015-) *************
// shocks for non-pboc dates (1) create null sample (2) retrieve window & shocks
clear
set obs 2 
gen date=mdy(3,17,2015) if _n == 1 
replace date=mdy(9,20,2022) if _n==2 
tsset date,d 
tsfill 
gen mptic = runiformint($begt +15, $endt - 45)  // randomly pick a tic and make sure the window won't cross days
gen weekday=dow(date)
gen nonpbocdays=1
save TempFiles/Announcement/nonpboc_Announce_Tic,replace 

filelist, dir("E:\Dropbox\UMP_Paper\RawData\IRS_transaction") pat("*.xlsx") nor
qui levelsof filename,local(fs)
foreach f in `fs'{
    qui import excel using "$root\RawData\IRS_transaction/`f'",firstrow clear
	local fn = subinstr("`f'","xlsx","dta",.)
	di "`fn'"
	qui destring 最优* *量*,replace force 
	qui drop if strpos(合约品种,"*") | (strpos(合约品种,"/") & ~strpos(合约品种,"ShiborO/N"))
	cap drop if 序号==.
	cap drop 序号
	qui gen date=date(日期,"YMD")
	format date %td 
	qui su date 
	tab date if date==r(min)
	qui su date 
	tab date if date==r(max)
	di _N
	cap save TempFiles/IRS_transaction/`fn',replace 
}

//open in excel, and change all the format to chinese. Very funny you'll find it in STATA language...
filelist, dir("E:\Dropbox\UMP_Paper\RawData\IRS_transaction/2021-2023") pat("*.xlsx") nor
qui levelsof filename,local(fs)
foreach f in `fs'{
    qui import excel using "$root\RawData\IRS_transaction/2021-2023/`f'",firstrow clear allstring
	local fn = subinstr("`f'","xlsx","dta",.)
	di "`fn'"
	qui destring 最优* *量*,replace force 
	qui drop if strpos(合约品种,"*") | (strpos(合约品种,"/") & ~strpos(合约品种,"ShiborO/N"))
	qui gen date=date(日期,"DMY")
	format date %td 
	qui su date 
	tab date if date==r(min)
	qui su date 
	tab date if date==r(max)
	di _N
	cap save TempFiles/IRS_transaction/`fn',replace 
}

filelist, dir("E:\Dropbox\UMP_Paper\TempFiles\IRS_transaction") pat("*.dta") nor
qui levelsof filename,local(fs)
clear
foreach f in `fs'{
	append using TempFiles/IRS_transaction/`f'
}
duplicates drop 
drop if date==. | 时间==""
gen hour = substr(时间,1,2)
gen minute=substr(时间,4,2)
gen second=substr(时间,7,2)
destring hour minute second,replace
drop 日期 时间
format date %td
*tab 合约品种,m 
drop if 合约品种=="" //| strpos(合约品种,"AAA","CDB","D10/G10","GB10","S3M")
//one record is either sell or buy ... or both?
gen sell=1 if 最优报买 != . & 最优报卖 == . 
replace sell=0 if 最优报买 == . & 最优报卖 != . 
su if sell==. // sell and buy is not the same while close...look like quotes?
drop sell
//最优报卖价=the highest bid; 最优报买价=the lowest ask
count if 最优报买价 > 最优报卖价 & 最优报卖价 != . & 最优报买价 != .  // 563, so ask price always lower than bid price
egen price=rowmean(最优报买价 最优报卖价)
egen volume=rowmean(报买量亿元 报卖量亿元)
gen contract=substr(合约品种,1,strpos(合约品种,"_")-1)
tab contract
replace contract = "ShiborON" if contract == "ShiborO/N"
drop if strpos(contract,"/")
gen tenor = substr(合约品种,strpos(合约品种,"_")+1,.)
drop 合约品种
rename contract type
gen contract = type+tenor
gen tic = dhms(date,hour,minute,second),a(date) 
format tic %tc // inaccurate 
egen ticgroup = group(date hour minute second)
unique ticgroup contract 
bys ticgroup contract:gen track=_N 
tab track
edit if track > 1  // duplicates bcos operational issues (you request twice sometimes)
drop if track > 1 & (tic == . | volume == .)
drop track
bys contract date hour minute second:egen avgvol=mean(volume)
replace volume=avgvol 
drop avgvol 
bys contract date hour minute second:egen avgprice=mean(price)
replace price=avgprice 
drop avgprice 
duplicates drop
order date tic hour minute second type tenor price volume
drop 最优报买价 最优报卖价 报买量亿元 报卖量亿元
drop tic type tenor
drop ticgroup 
reshape wide price volume,i(date hour minute second) j(contract) str
keep date hour minute second priceShibor* priceLPR* priceFR007* priceFDR007* 
findname price*,local(pvs)
foreach pv in `pvs'{
    local vn = subinstr("`pv'","price","",.)
	rename `pv' `vn'
}
egen nonmiss = rownonmiss(Shibor* LPR* FR007* FDR007*)
drop if nonmiss == 0
drop nonmiss
save TempFiles/IRSintraday,replace 




filelist, dir("E:\Dropbox\UMP_Paper\TempFiles\IRS_transaction") pat("*.dta") nor
qui levelsof filename,local(fs)
clear
foreach f in `fs'{
	append using TempFiles/IRS_transaction/`f'
}
duplicates drop 
gen date=date(日期,"YMD"),a(日期)
bys 合约品种 date:gen numDeal = _N  // no. of transactions 

gen hour = substr(时间,1,2)
gen minute=substr(时间,4,2)
gen second=substr(时间,7,2)
destring hour minute second,replace
drop 日期 时间
format date %td
*tab 合约品种,m 
drop if 合约品种=="" 
egen price=rowmean(最优报买价 最优报卖价)
egen volume=rowmean(报买量亿元 报卖量亿元)
rename 合约品种 contract
bys contract date:egen totvolume =total(volume)
bys contract date:egen avgprice=mean(price)

gen mdate=mofd(date),a(date) 
bys contract mdate:egen totvolumem=total(volume)
bys contract mdate:egen totnoDealm=total(numDeal)
bys contract mdate:egen avgpricem =mean(price)
keep contract mdate totvolumem avgpricem totnoDealm
duplicates drop
format mdate %tm
sort mdate contract
gen type=substr(contract,1,strpos(contract,"_")-1),a(contract)
gen tenor = substr(contract,strpos(contract,"_")+1,.),a(type)
gen contract = type+tenor
keep type tenor totvolumem totnoDealm mdate
edit if mdate==ym(2020,1) & (strpos(type,"Shibor") | strpos(type,"FR007") | strpos(type,"LPR"))
export excel using OutPut/TabLiquidity.xlsx if mdate==ym(2020,1) & (strpos(type,"Shibor") | strpos(type,"FR007") | strpos(type,"LPR")),firstrow(variables) replace
gsort mdate -totvolumem
//bys contract:egen volume=mean(totvolume)
//keep contract volume 

//keep contract date totvolume avgprice 
//duplicates drop
//bys contract:egen volume=mean(totvolume)
//keep contract volume 
//duplicates drop
//gsort -volume


use TempFiles/IRSintraday,replace 
keep date hour minute second FR007* 
egen nonmiss = rownonmiss(FR007*)
drop if nonmiss == 0
drop nonmiss
su hour 
su minute if hour == 8   // only one obs at 8:53 ≈ 9 o'clock
edit if hour==8  // and a weird 100 FR0072Y. So drop it 
/*
gen hour2 = 9 if hour==8 & minute==r(max)
replace minute=0 if hour==8 & minute==r(max) 
replace hour=hour2 if hour2 != . 
drop hour2 
*/
drop if hour < 9
su minute if hour == 9     // 53 
su minute if hour == 17    // 17:30
//>>> trading hours =      // 9:00-17:30 
hist minute if hour == 17  // and there are evenly distributed??? (I thought it would bunch at 17:30 or something)
hist minute if hour == 9
hist hour,xlab(9(1)17)     // a relatively even distribution 
hist minute,xlab(0(10)59)  // bunch around 0/30/59 but not severe 
gen mptic=hour*60+minute 
drop hour minute second
save TempFiles/IRSR007intraday,replace

*/


************************* Part A. High Frequency Shock *************************

************* 2. Construct daily surprises around accurate window **************
foreach xxx in "construction"{

tempfile 

global begt = 9*60
global endt = 17*60 + 30 
foreach wind in broad narrow{

if "`wind'"=="broad"{
	global prep = 15
	global post = 45
}
else if "`wind'"=="narrow"{
	global prep = 10
	global post = 20
}

foreach mp in MLF LPR LR RRR control{  

foreach int in FR0076M FR0079M FR0071Y FR0075Y Shibor3M6M Shibor3M9M Shibor3M1Y{ // FR0072Y FR0073Y FR0074Y FR0077Y FR00710Y Shibor3M10Y Shibor3M2Y Shibor3M3Y Shibor3M4Y Shibor3M5Y Shibor3M7Y ShiborON1M ShiborON1Y ShiborON2Y ShiborON3M ShiborON3Y ShiborON6M ShiborON9M LPR1Y10Y LPR1Y1Y LPR1Y2Y LPR1Y3Y LPR1Y4Y LPR1Y5Y LPR1Y6M LPR1Y7Y LPR1Y9M LPR5Y10Y LPR5Y1Y LPR5Y2Y LPR5Y3Y LPR5Y4Y LPR5Y5Y LPR5Y6M LPR5Y7Y LPR5Y9M FDR0071Y FDR0072Y FDR0073M FDR0073Y FDR0074Y FDR0075Y FDR0076M FDR0079M

use Announcement/`mp'_Announce_Tic,replace
qui cap gen weekday=dow(date)
qui gen weekend = inlist(weekday,0,6),a(mptic)
qui gen shock`int' = .

local Nobs = _N 
forv k = 1/`Nobs'{
    
	forv p=1/1{  // define window 
	local wd = weekday[`k']
	local tt = mptic[`k']
	
	//if the window is on the same day 
	//only if announced on Mon-Fri., after 9:15 and before 16:45 
    local dl = date[`k']
	local du = date[`k']
	local tl = `tt'-$prep
	local tu = `tt'+$post  
	
	//if announced on Mon. before 9:00
	if `wd' == 1 & `tt' <= $begt{
	    local dl = `dl'-3
		local tl = $endt - $prep
	}
	
	//if announced on Mon. during 9:00-9:15, the first part of the pre 15 min on the end of last Fri.
	if `wd' == 1 & `tt' > $begt & `tt' < $begt + $prep {
	    local dl = `dl'-3
		local tl = $endt - ($prep -(`tt'-$begt )) 
	}
	
	//if announced on Tue.-Fri. before 9:00
	if `wd'>=2 & `wd'<=5 & `tt'<= $begt{
	    local dl = `dl'-1
		local tl = $endt -$prep 
	}
	
	//if announced on Tue.-Fri. during 9:00-9:15,.the first part of the pre 15 min on the end of last day
	if `wd'>=2 & `wd'<=5 & `tt' > $begt & `tt' < $begt + $prep {
	    local dl = `dl'-1
		local tl = $endt - ($prep -(`tt'-$begt ))
	}
	
	//if announced on Mon.-Thur. during 17:15-17:30,.the second part of the post 45 min on the beg of next day
	//(very likely for mp announcements: 11 of these in the final event sample)
	if `wd'>=1 & `wd'<=4 & `tt' > $endt - $post & `tt' <= $endt{
	    local du = `dl'+1
		local tu = $begt + ($post -($endt -`tt')) 
	}
	
	//if announced on Mon.-Thur. after 17:30 (7 obs)
	if `wd'>=1 & `wd'<=4 & `tt' > $endt{
	    local du = `du'+1 
		local tu = $begt +$post 
	}
	
	//if announced on Fri. during 17:15-17:30, the second part of the post 45 min on the beg of next Mon.
	if `wd' == 5 & `tt' > $endt - $post & `tt' <= $endt{
	    local du = `dl'+3
		local tu = $begt + ($post -($endt-`tt')) 
	}
	
	//if announced on Fri. after 17:30 (1 obs)
	if `wd' == 5 & `tt' > $endt{
	    local du = `dl'+3
		local tu = $begt + $post
	}
	
	//if announced on Sat.
	if `wd' == 6{
	    local dl = `dl'-1
		local tl = $endt -$prep
	    local du = `dl'+2
		local tu = $begt +$post
	}
	
	//if announced on Sun. 
	if `wd' == 0{
	    local dl = `dl'-2
		local tl = $endt -$prep
	    local du = `dl'+1
		local tu = $begt +$post
	}
	}
	
	preserve 
	qui use TempFiles/IRSintraday,replace 
	keep date hour minute second `int'
	qui drop if hour < 9
	qui gen mptic=hour*60+minute 
	// note how you've been wrong on this:you retrieve obs outside the window for end-of-day event
	// and you can never retrieve begin-of-day window when tu < tl ! 
	//qui keep if (date>=`dl' & mptic >= `tl') & (date<=`du' & mptic <= `tu') 
	if `dl'!=`du'{
	qui keep if (date==`dl'&mptic>=`tl'&mptic<=$endt ) | (date==`du'&mptic<=`tu'&mptic>=$begt ) ///
	          | (date>`dl'&date<`du') 
	}
	else{ //can't be incorporated in the last case or you will include the whole day t
		qui keep if date==`dl'&mptic>=`tl'&mptic<=`tu'
	}
	qui keep date mptic `int' 
	qui drop if `int' == . 
	qui sort date mptic 
	local begval = `int'[1]  // first value observed within the 60 min window 
	local endval = `int'[_N] //  last value observed within the 60 min window 
	restore 
	
	qui replace shock`int' = `endval' - `begval' if _n == `k'
	}
rename shock`int' `wind'`int'

tempfile `mp'`int'`wind'
qui save ``mp'`int'`wind'',replace
}

}

foreach mp in MLF LPR LR RRR control{  

use ``mp'FR0076M`wind'',replace 
foreach int in FR0076M FR0079M FR0071Y FR0075Y Shibor3M6M Shibor3M9M Shibor3M1Y{
	merge 1:1 date using ``mp'`int'`wind'',nogen 
}
gen `mp'days = 1 
tempfile highfreqShock`mp'
save `highfreqShock`mp'',replace 
}


clear 
use `highfreqShockMLF',replace
foreach mp in LPR LR RRR control{ 
	merge 1:1 date using `highfreqShock`mp'',nogen
}
keep  date mptic *days `wind'*
order date mptic *days `wind'*
gen weekday=dow(date)
save `highfreqShock`wind'',replace 
}

use `highfreqShocknarrow',replace 
merge 1:1 date using `highfreqShockbroad',nogen 
save OutPut/highfreqShock,replace
}

/* 
use "$root/OutPut/highfreqShock",replace
su broad* narrow* if date <mdy(1,1,2015) 
drop if date <mdy(1,1,2015) 

 gen acrossdaysbroad  =  ~(inlist(weekday,1,2,3,4,5) & mptic >=$begt + 15 & mptic <=$endt - 15)
 gen acrossdaysnarrow =  ~(inlist(weekday,1,2,3,4,5) & mptic >=$begt + 10 & mptic <=$endt - 10)
egen noexitbroad  = rownonmiss(broadFR0071Y*)
egen noexitnarrow = rownonmiss(narrowFR0071Y*)
count if acrossdaysbroad ==1 & noexitbroad >0   // 45
count if acrossdaysnarrow==1 & noexitnarrow>0   // 34
tabstat broadFR0071Y narrowFR0071Y, by(acrossdaysbroad) stat(sd mean)

acrossdaysbroad |  br~0071Y  na~0071Y
----------------+--------------------
              0 |  .0195049  .0143131
                |  .0007643  .0010042
----------------+--------------------
              1 |  .0587953  .0377786
                | -.0039467  .0026511
----------------+--------------------
          Total |  .0281887  .0194421
                |  .0001199  .0012426
-------------------------------------

the crossday shock is very much more volatile than sameday one (fortunately), the sd is about 3 times larger 
*/

********************* 3. Prepare data for imputed PCA in R *********************
//is it possible to just use raw changes? but PCA is what NS did/
 use OutPut/highfreqShock,replace 
 drop if date <mdy(1,1,2015) 
 
tsset date,d 
tsfill
gen qdate=qofd(date)
gen year=year(date)
gen mdate=mofd(date)
findname *days,local(ds)
foreach dd in `ds'{
	replace `dd'=0 if `dd'==. 
}
findname broad* narrow*,local(ssn)
foreach ss in `ssn'{ // Don't forget this, or 2 magnitudes while PCA with DR007!
	replace `ss'=`ss'*100 
}
merge 1:1 date using OutPut/daily_surprise/AllSrp, // daily changes around MPs
drop if _merge==2
drop _merge // match=133

replace convdays = (LRdays==1 | RRRdays==1)
replace unconvdays = (LPRdays==1 | MLFdays==1)  //
replace pbocdays = convdays | unconvdays

tab pbocdays ,m  // 133 in total 

foreach spec in broad narrow{

	foreach meet in pboc control unconv conv MLF LR RRR LPR{ 
		
		* 1. daily change in R007, high frequency change in IRS R007 1/5Y and 3M-SHIBOR 6/9/12M
		preserve 
		qui keep if `meet'days==1
		qui keep dr007 `spec'FR0071Y `spec'Shibor3M6M `spec'Shibor3M9M `spec'Shibor3M1Y `spec'FR0075Y
		qui export excel using OutPut/MDA/`meet'`spec'r0075y.xlsx,firstrow(variables) replace 
		qui drop `spec'FR0075Y
		qui export excel using OutPut/MDA/`meet'`spec'r0071y.xlsx,firstrow(variables) replace 
		restore 
		
		* 2. high frequency change in IRS R007 1/5Y and 3M-SHIBOR 6/9/12M --- this is baseline!
		preserve 
		qui keep if `meet'days==1
		qui keep `spec'FR0071Y `spec'Shibor3M6M `spec'Shibor3M9M `spec'Shibor3M1Y `spec'FR0075Y
		qui export excel using OutPut/MDA/`meet'`spec'irs5y.xlsx,firstrow(variables) replace 
		qui drop `spec'FR0075Y
		qui export excel using OutPut/MDA/`meet'`spec'irs1y.xlsx,firstrow(variables) replace 
		restore 
		
		* 3. high frequency change in IRS R007 6/9M and 1/5Y
		preserve 
		qui keep if `meet'days==1
		qui keep `spec'FR0076M `spec'FR0079M `spec'FR0071Y `spec'FR0075Y
		qui export excel using OutPut/MDA/`meet'`spec'irsr0075y.xlsx,firstrow(variables) replace 
		qui drop `spec'FR0075Y
		qui export excel using OutPut/MDA/`meet'`spec'irsr0071y.xlsx,firstrow(variables) replace 
		restore 
		
	}
}


************************ 4. Run pca_missMDA_highfreq.R *************************

******************* 5. Read back the PCA results and visualize *****************

foreach meet in pboc control unconv MLF conv LR RRR LPR{  // 
	foreach spec in broad narrow{ 
		foreach ss in r0075y r0071y irs5y irs1y irsr0075y irsr0071y{
			
			local int = "`spec'`ss'"
			
			preserve 
			cap import excel using OutPut/MDA/PCAhighfreq/`meet'`int'PCAmiss.xlsx,firstrow clear 
			qui if _rc==0{
				keep Dim1   // pca1
				gen id=_n 
				local vn=strupper("`meet'")
				lab var Dim1 "MPS `vn'"
				rename Dim1 pca`meet'`int'
				save OutPut/MDA/PCAhighfreq/`meet'`int'PCAmiss,replace 
			}
			restore
			qui if _rc==0{
				preserve 
				keep if `meet'days==1
				gen id=_n 
				merge 1:1 id using OutPut/MDA/PCAhighfreq/`meet'`int'PCAmiss,nogen 
				drop id 
				save TempFiles/MDA`meet'`int',replace 
				restore
			}
			cap merge 1:1 date using TempFiles/MDA`meet'`int',nogen 
		}
	}
}

foreach meet in pboc control unconv MLF conv RRR LR LPR{  //
	foreach spec in broad narrow{ 
		foreach ss in r0075y r0071y irs5y irs1y irsr0075y irsr0071y{
			local int = "`spec'`ss'"
			
			** recale to 1pp response in 1-year Treasury
			cap reg dtreasury1y pca`meet'`int' if `meet'days==1,r
			if _rc==0{
				mat scale = e(b)
				replace pca`meet'`int'=pca`meet'`int'*scale[1,1] 
			}
		}
	}		
}

/** STATA PCA
foreach meet in pboc nonpboc unconv MLF conv RRR LR LPR{  //
	foreach spec in broad narrow{ 
		foreach ss in r0075y r0071y irs5y irs1y irsr0075y irsr0071y{
			
			local int = "`spec'`ss'"
			qui cap pca `int'Shibor3M6M `int'Shibor3M9M `int'Shibor3M1Y `int'FR0071Y `int'FR0075Y if `meet'days==1
			qui cap predict path_m_`meet'_`int' if `meet'days==1,score
			lab var path_m_`meet'_`int' "MPS `vn' (NA)"
		}
	}
}
*/
order *date *days pca* //path* 
order MLFdays LPRdays,a(RRRdays)
order pbocdays convdays unconvdays,b(RRRdays)
order year,a(mdate)
tsset date,d
tsfill
drop dirs* *forward* *future* *shibor* *fut* 
drop dgdpexp* dinfexp* dstock* dtreasury*
save TempFiles/surprise_highfreq_clean,replace
 use TempFiles/surprise_highfreq_clean,replace
 
/* produce Figure 1 */
keep if pbocdays==1
gen month=month(date)
gen day=day(date)
tostring year month day, replace 

foreach v in "fancylabeltimesnewroman"{
local mon "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"
local n: word count `mon'
forvalues i=1/`n'{
	local mon`i' : word `i' of `mon'
	replace month="`mon`i''" if month=="`i'"
}

gen datestr = day+" "+ month+" "+year,a(date)

tsset date,d
gen dt=_n,a(date)  // indicator for PBOC dates
local hand "label define datelabs "
local obs=_N
forv nn =1/`obs'{
    local dn=dt[`nn']
	local cnt=datestr[`nn']
	local hand `"`hand'`dn' "{stSerif:`cnt'}" "' // font=Times New Roman
}
`hand',replace 
lab values dt datelabs   
}
  
foreach spec in broad narrow{ 
foreach ss in r0075y r0071y irs5y irs1y irsr0075y irsr0071y{
	
local int "`spec'`ss'"
foreach mp in pboc conv unconv{
replace pcaunconv`int'=. if pcaunconv`int'==0 
}

su dt
local ma=r(max)
local mi=r(min)
tw (bar pcapboc`int' dt, fc(brown) lsty(none)) /// 
   (scatter pcaconv`int' dt,mc(blue) ms(oh) msiz(10pt))  ///
   (scatter pcaunconv`int' dt,mc(red) ms(oh) msiz(10pt)) ///
   ,graphr(c(white)) plotr(c(white)) leg(order(1 "{stSerif: MPS}" ///
   2 "{stSerif:MPS Conv.}" 3 "{stSerif:MPS Innov.}") siz(60pt) /// 
   r(1) ring(0) bplacement(neast)) xsiz(20) ysiz(8) aspect(.3) yti("") /// 
   ylab(,format(%3.1fc) axis(1) labs(60pt) angle(horizontal)) xti("") ///
   xlab(`mi'(5)`ma',valuelabel labs(40pt) angle(45)) name(`int'1,replace) 

su dt
local ma=r(max)
local mi=r(min)
tw (bar pcaMLF`int' dt, fc(maroon) lsty(none))  ///
   (bar pcaLPR`int' dt, fc(blue) lsty(none)) /// 
   (bar pcaRRR`int' dt, fc(navy) lsty(none)) ///  (bar pcaLR`int'  dt, fc(red)  lsty(none)) ///
   ,graphr(c(white)) plotr(c(white)) leg(order(1 "{stSerif:MLF}"    ///
   2 "{stSerif:LPR}" 3 "{stSerif:RRR}") siz(60pt) /// 4 "{stSerif:LR}" -- non exist
   r(1) ring(0) bplacement(neast)) xsiz(20) ysiz(8) aspect(.3) yti("") /// 
   ylab(,format(%3.1fc) axis(1) labs(60pt) angle(horizontal)) xti("") ///
   xlab(`mi'(5)`ma',valuelabel labs(40pt) angle(45)) name(`int'2,replace) 
   
gr combine `int'1 `int'2,c(1) graphr(c(white)) plotr(c(white)) ysiz(17.5) xsiz(20) imargin(zero)
gr export "OutPut/Graphics/MPShocks/Highfreq/MPSBaseline_Highfreq_`int'.png",width(3600) replace
}
}




***************************** Part B. Daily Shock ******************************
foreach xxm in "construct"{

do backup_functions/construct_precise_window.do

use OutPut/daily_surprise/AllSrp,replace // precise window around events 
merge 1:1 date using OutPut/daily_surprise/SurpNonPBOC_AllSrp, nogen // null
drop if (date<d(22nov2006)|date>d(20sep2022))   
// drop RRR day on 03nov2006;if don't restrict this,it will estimate PCA using dr007 only

tsset date,d 
tsfill
replace qdate=qofd(date)
gen year=year(date)
replace mdate=mofd(date)
findname *days,local(ds)
foreach dd in `ds'{
	replace `dd'=0 if `dd'==. 
}
gen nonpbocdays = ~pbocdays

order date *days dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y
foreach meet in pboc nonpboc conv unconv MLF LPR RRR LR{ // EXR M2 MPR
** +Forward | +Future 2Y | +Future 5Y
	
	** pca drops too many sample --> decide to use missMDA in R
	
	preserve 
	qui keep if `meet'days==1
	
	********** Change here on May-19-2023: DON'T USE dirsr007_2 x **********
	qui keep dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y 
	********** Change here on May-19-2023: DON'T USE dirsr007_2 x **********
	qui export excel using OutPut/MDA/`meet'irs.xlsx,firstrow(variables) replace 
	qui egen nomiss=rowmiss(dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y)
	qui count if nomiss > 0
	local missn=r(N)
	if `missn'>0{
		di "`meet' `int': `missn' missings" 
	}
	
	********** Change here on Jun-06-2023: IRSSHIBOR Look No Good **********
	qui keep dr007 dirsr0071y
	********** Change here on Jun-06-2023: IRSSHIBOR Look No Good **********
	qui export excel using OutPut/MDA/`meet'r007.xlsx,firstrow(variables) replace 
	restore
	
	preserve 
	qui keep if `meet'days==1
	
	qui keep dr007 dirsr0076m dirsr0079m dirsr0071y dirsr0075y
	qui export excel using OutPut/MDA/`meet'irsr0075y.xlsx,firstrow(variables) replace 
	
	qui keep dr007 dirsr0076m dirsr0079m dirsr0071y
	qui export excel using OutPut/MDA/`meet'irsr0071y.xlsx,firstrow(variables) replace 
	restore
}

pca_missMDA.R

foreach meet in pboc nonpboc conv unconv MLF LPR RRR LR{  
** IRS R007+IRS Shibor, Conventional(LR/RRR) | New(MLF/LPR) | Both(PBOC)
** +Forward | +Future 2Y | +Future 5Y
	foreach int in irs irsr0071y irsr0075y r007{ // fwd fut2y fut5y
		
		preserve 
		cap import excel using OutPut/MDA/`meet'`int'PCAmiss.xlsx,firstrow clear 
		qui if _rc==0{
			keep Dim1   // pca1
			gen id=_n 
			local vn=strupper("`meet'")
			lab var Dim1 "MPS `vn'"
			rename Dim1 pca`meet'`int'
			save OutPut/MDA/`meet'`int'PCAmiss,replace 
		}
		restore
		if _rc==0{
			preserve 
			qui keep if `meet'days==1
			qui gen id=_n 
			qui merge 1:1 id using OutPut/MDA/`meet'`int'PCAmiss
			qui count if _merge != 3
			if r(N) >0{
			    di "Something wrong with the data"
			}
			drop id _merge 
			qui save TempFiles/MDA`meet'`int',replace 
			restore
		}
		cap merge 1:1 date using TempFiles/MDA`meet'`int',nogen 
	}
}

foreach meet in pboc nonpboc conv unconv MLF LPR RRR LR{ 
	foreach int in irs r007 irsr0071y irsr0075y{
		
		** recale to 1pp response in 1-year Treasury >> totally sig. positive
	    reg dtreasury1y pca`meet'`int' if `meet'days==1,r
		if _rc==0{
			//gen pca`meet'`int'_noscale=pca`meet'`int'
			replace pca`meet'`int'=pca`meet'`int'*_b[pca`meet'`int'] 
		}
	}		
}

** STATA PCA
foreach meet in pboc nonpboc conv unconv MLF LPR RRR LR{ 
	foreach int in irs{
		qui cap pca dr007 dirsr0071y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y if `meet'days==1
		qui cap predict path_m_`meet'_`int' if `meet'days==1,score
		lab var path_m_`meet'_`int' "MPS `vn' (NA)"
	}
}


order *date *days pca* path* 
order MLFdays LPRdays,a(RRRdays)
order pbocdays convdays unconvdays,b(RRRdays)
order year,a(mdate)
tsset date,d  // no gaps 
save TempFiles/surprise_final,replace
 use TempFiles/surprise_final,replace
//keep date dtreasury* 
//save TempFiles/dtreasury,replace

use TempFiles/surprise_final,replace
// dates with extrapolated data == . : like weekend, no interest rates at all
drop *forward* *future* *fut* dgdpexp* dinfexp* dstock* dtreasury*  
drop dirsshiboron* dirsshibor1w* dshibor*
save TempFiles/surprise_clean,replace
}
 use TempFiles/surprise_clean,replace 
 
**************************** Figure 1.MPS Baseline *****************************
foreach xx in "Figure1"{
keep if pbocdays==1
gen month=month(date)
gen day=day(date)
tostring year month day, replace 

foreach v in "fancylabeltimesnewroman"{
local mon "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"
local n: word count `mon'
forvalues i=1/`n'{
	local mon`i' : word `i' of `mon'
	replace month="`mon`i''" if month=="`i'"
}

gen datestr = day+" "+ month+" "+year,a(date)

tsset date,d
gen dt=_n,a(date)  // indicator for PBOC dates
local hand "label define datelabs "
local obs=_N
forv nn =1/`obs'{
    local dn=dt[`nn']
	local cnt=datestr[`nn']
	local hand `"`hand'`dn' "{stSerif:`cnt'}" "' // font=Times New Roman
}
`hand',replace 
lab values dt datelabs   
}
  
egen totdays = rowtotal(RRRdays LRdays LPRdays MLFdays),missing  // MPRdays
edit pca*irs if totdays > 1  // 9 obs
// shocks of different events occuring on the same day are not the same 
// bcos the PCA analysis is conducted on different sample.
// not a big deal unless they're highly correlated (make sure to report in appendix table)
gen indicator = pcaLRirs + pcaRRRirs if totdays > 1& LRdays& RRRdays // still not the same for different shocks because you use MDA...so just use the baseline shocks, the different types are just subsample! 
replace indicator=pcaMLFirs+pcaRRRirs if totdays > 1 & MLFdays& RRRdays
*count if convdays & unconvdays  // 2 


su dt
local ma=r(max)
local mi=r(min)
tw (bar pcapbocirs dt, fc(brown) lsty(none)) ///
   (scatter pcaconvirs dt,mc(blue) ms(oh) msiz(18pt))  ///
   (scatter pcaunconvirs dt,mc(red) ms(oh) msiz(18pt)) ///
   ,graphr(c(white)) plotr(c(white)) leg(order(1 "{stSerif:PBOC}" ///
   2 "{stSerif:Conv.}" 3 "{stSerif:Innov.}") ///
   r(1) ring(0) bplacement(neast) siz(60pt)) xsiz(20) ysiz(8) aspect(.3) yti("") /// 
   ylab(,format(%3.0fc) axis(1) labs(60pt) angle(horizontal)) xti("") ///
   xlab(`mi'(9)`ma',valuelabel labs(50pt) angle(45)) name(MPSpca,replace) 

su dt
local ma=r(max)
local mi=r(min)
tw (bar pcaRRRirs dt, fc(navy) lsty(none)) ///
   (bar pcaLRirs  dt, fc(blue) lsty(none)) ///(bar pcaMPRirs dt if totdays==1, fc(lavender) lsty(none)) ///
   (bar pcaMLFirs dt, fc(maroon) lsty(none))  ///
   (bar pcaLPRirs dt, fc(red) lsty(none)) /// 
   (scatter indicator dt if totdays==2 & LRdays & RRRdays, mc(blue) ms(oh)) ///
   (scatter indicator dt if totdays==2 & MLFdays& RRRdays, mc(red) ms(oh)) ///
   ,graphr(c(white)) plotr(c(white)) leg(order(1 "{stSerif:RRR}"  ///
   2 "{stSerif:LR}" 3 "{stSerif:MLF}" ///"{stSerif:MPS MPR}" 4 
   4 "{stSerif:LPR}" 5 "{stSerif:LR+RRR}" 6 "{stSerif:MLF+RRR}") ///
   c(4) ring(0) bplacement(neast) symy(2) symx(3) textw(10) forces size(45pt)) ///
   xsiz(20) ysiz(8) aspect(.3) yti("") xti("") /// 
   ylab(,format(%3.0fc) axis(1) labs(60pt) angle(horizontal)) ///
   xlab(`mi'(9)`ma',valuelabel labs(50pt) angle(45)) name(MPSpca2,replace) 
   
gr combine MPSpca MPSpca2,c(1) graphr(c(white)) plotr(c(white)) ysiz(17.5) xsiz(20) imargin(zero)
gr export "OutPut/Graphics/MPSBaseline.png",width(3600) replace

}

global pboc_name "Baseline"
global unconv_name "Innovative"
global conv_name "Conventional"
foreach mp in LPR MLF LR RRR MPR{
	global `mp'_name "`mp'"
}

************* Table. Shocks Volatility for PBOC and Non-PBOC days **************
//show IRSR007 5Y but not 6M bcos the ratio test of the later gives .74 < 1
//IRS 3M-SHIBOR 9M produces 0.97. But can argue that we replace by IRSR0079M for robustness
 use TempFiles/surprise_final,replace

gen mps = pcapbocirs 
replace mps = pcanonpbocirs if mps==. 

tabstat mps dr007 dirsr0079m dirsr0071y dirsr0075y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y,by(pbocdays) nototal stat(mean sd) format(%04.2fc) save
return list
putexcel set TempFiles/comparePBOC_Non.xlsx, replace sheet(NonPBOC)
putexcel A1 = matrix(r(Stat1)), names
putexcel set TempFiles/comparePBOC_Non.xlsx, modify sheet(PBOC)
putexcel A2 = matrix(r(Stat2)), names
tabstat mps dr007 dirsr0079m dirsr0071y dirsr0075y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y,by(convdays) nototal stat(mean sd) format(%04.2fc) save
return list
putexcel set TempFiles/comparePBOC_Non.xlsx, modify sheet(Conv)
putexcel A3 = matrix(r(Stat2)), names
tabstat mps dr007 dirsr0079m dirsr0071y dirsr0075y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y,by(unconvdays) nototal stat(mean sd) format(%04.2fc) save
return list
putexcel set TempFiles/comparePBOC_Non.xlsx, modify sheet(Unconv)
putexcel A4 = matrix(r(Stat2)), names

ttest mps,by(pbocdays)  // mean values obviously won't be sig. different since they're both 0 !

foreach ss in mps dr007 dirsr0079m dirsr0071y dirsr0075y dirsshibor3m6m dirsshibor3m9m dirsshibor3m1y{
    sdtest `ss',by(pbocdays)
}

sdtest mps,by(pbocdays)


********** Figure A. MPS and Actual Policy Rate Changes  ***********
foreach v in "ActualvsMPS"{
clear 
foreach mp in MLF LPR RRR LR{
	append using TempFiles/Announcement/`mp'_Announce_Tic
}
keep date event policy 
sort date
reshape wide policy,i(date) j(event) str
tsset date,d
merge 1:1 date using TempFiles/surprise_clean
tab pbocdays if _merge==2  // 0 
tab pbocdays if _merge==3  // 1 
tab date if _merge==1  //pre 2006nov 
keep if _merge==3
keep date policy* *days pca*irs path*
drop pbocdays 

// MDA does nothing wrong; correlation with PCA is strong; plus it avoids a lot of holes
foreach xx in "checkActualRRR"{
preserve 
keep if RRRdays 
gen month = month(date),a(date)
gen day = day(date),a(date)
gen year=year(date),a(date)
tostring year month day, replace 
replace month="0"+month if length(month)==1
replace day="0"+day if length(day)==1
gen datestr = year+month+day,a(date) // font=Times New Roman

gen dateid1 = 4*_n-3, b(date)  // 1
gen dateid3 = dateid1 + 1, b(date)
gen dateid5 = dateid1 + 2, b(date)

qui levelsof datestr, local(dates)
local hand "label define datelabs "
cap lab drop datelabs 
local cnt 1 
foreach dt in `dates'{
	local hand `"`hand'`cnt' "{stSerif:`dt'}" "'
	local cnt = `cnt'+4
}
`hand'
lab values dateid* datelabs
destring year month day,replace

// to produce sensible tickers in y-axis
replace policyRRR = policyRRR/10

su dateid1 if RRRdays
local did=r(min) 
local ens=r(max)
su pcaRRRirs
local max1=r(max)
local min1=r(min)
su path_m_RRR_irs
local max2=r(max)
local min2=r(min)
su policyRRR 
local max=ceil( max(`max1',`max2',r(max))/5)*5  
local min=floor(min(`min1',`min2',r(min))/5)*5  
local sep=floor(ceil((`max'-`min')/6.5/5)*5)    // gap of yticks=5

local xtx="`did'(4)`ens'"
pwcorr pcaRRR path_m_RRR_irs policyRRR
mat define Cmat=r(C) 
local cor1 = round(Cmat[1,2],.001)
local cor2 = round(Cmat[1,3],.001)
local txtx = `ens'-8
local txty = `max'-`sep'/3
local txtz = `max'-2*`sep'/3
tw (bar pcaRRRirs dateid5,col(blue) barw(1)) (bar path_m_RRR_irs dateid3,col(green) barw(1)) /// 
   (bar policyRRR dateid1,col(pink) barw(1)) if dateid1>=`did', xlab(`xtx',valuelabel labs(12pt) angle(vertical) ///
   grid glp(-) glw(thin)) xti("") ylab(`min'(`sep')`max',labs(12pt) nogrid) yti("") graphr(c(white)) /// 
   plotr(c(white)) ti("{stSerif:MPS RRR}",c(black) si(25pt)) text(`txty' `txtx' "{stSerif:ρ(MDA,PCA) = `cor1'}") ///
   text(`txtz' `txtx' "{stSerif:ρ(MDA,Δ) = `cor2'}") name(RRR,replace) xsiz(20) ysiz(8) /// 
   leg(order(1 "{stSerif:MPS}" 2 "{stSerif:MPS (NA)}" 3 "{stSerif:Actual}") r(1))
gr export OutPut/Graphics/MPSRRRvsActual.png,replace width(1500)
restore 
}

foreach xx in "checkActualLR"{
preserve 
keep if LRdays 
gen month = month(date),a(date)
gen day = day(date),a(date)
gen year=year(date),a(date)
tostring year month day, replace 
replace month="0"+month if length(month)==1
replace day="0"+day if length(day)==1
gen datestr = year+month+day,a(date) // font=Times New Roman

gen dateid1 = 4*_n-3, b(date)  // 1
gen dateid3 = dateid1 + 1, b(date)
gen dateid5 = dateid1 + 2, b(date)

qui levelsof datestr, local(dates)
local hand "label define datelabs "
cap lab drop datelabs 
local cnt 1 
foreach dt in `dates'{
	local hand `"`hand'`cnt' "{stSerif:`dt'}" "'
	local cnt = `cnt'+4
}
`hand'
lab values dateid* datelabs
destring year month day,replace

// to produce sensible tickers in y-axis
replace policyLR = policyLR/10

su dateid1 if LRdays
local did=r(min) 
local ens=r(max)
su pcaLRirs
local max1=r(max)
local min1=r(min)
su path_m_LR_irs
local max2=r(max)
local min2=r(min)
su policyLR 
local max=ceil( max(`max1',`max2',r(max))/5)*5  
local min=floor(min(`min1',`min2',r(min))/5)*5  
local sep=floor(ceil((`max'-`min')/6.5/5)*5)    // gap of yticks=5

local xtx="`did'(4)`ens'"
pwcorr pcaLR path_m_LR_irs policyLR
mat define Cmat=r(C) 
local cor1 = round(Cmat[1,2],.001)
local cor2 = round(Cmat[1,3],.001)
local txtx = `ens'-8
local txty = `max'-`sep'/3
local txtz = `max'-2*`sep'/3
tw (bar pcaLRirs dateid5,col(blue) barw(1)) (bar path_m_LR_irs dateid3,col(green) barw(1)) /// 
   (bar policyLR dateid1,col(pink) barw(1)) if dateid1>=`did', xlab(`xtx',valuelabel labs(12pt) angle(vertical) ///
   grid glp(-) glw(thin)) xti("") ylab(`min'(`sep')`max',labs(12pt) nogrid) yti("") graphr(c(white)) /// 
   plotr(c(white)) ti("{stSerif:MPS LR}",c(black) si(25pt)) text(`txty' `txtx' "{stSerif:ρ(MDA,PCA) = `cor1'}") ///
   text(`txtz' `txtx' "{stSerif:ρ(MDA,Δ) = `cor2'}") name(LR,replace) xsiz(20) ysiz(8) /// 
   leg(order(1 "{stSerif:MPS}" 2 "{stSerif:MPS (NA)}" 3 "{stSerif:Actual}") r(1))
gr export OutPut/Graphics/MPSLRvsActual.png,replace width(1500)
restore 
}
// I believe there's an anticipation effect in LPR shocks; better use intraday data? but it released at 9:30...should I use 30min in the last day?

foreach xx in "checkActualLPR"{  // e.g. 2020Apr20
preserve 
keep if LPRdays 
gen month = month(date),a(date)
gen day = day(date),a(date)
gen year=year(date),a(date)
tostring year month day, replace 
replace month="0"+month if length(month)==1
replace day="0"+day if length(day)==1
gen datestr = year+month+day,a(date) // font=Times New Roman

gen dateid1 = 4*_n-3, b(date)  // 1
gen dateid3 = dateid1 + 1, b(date)
gen dateid5 = dateid1 + 2, b(date)

qui levelsof datestr, local(dates)
local hand "label define datelabs "
cap lab drop datelabs 
local cnt 1 
foreach dt in `dates'{
	local hand `"`hand'`cnt' "{stSerif:`dt'}" "'
	local cnt = `cnt'+4
}
`hand'
lab values dateid* datelabs
destring year month day,replace

// to produce sensible tickers in y-axis
replace policyLPR = policyLPR/10

su dateid1 if LPRdays
local did=r(min) 
local ens=r(max)
su pcaLPRirs
local max1=r(max)
local min1=r(min)
su path_m_LPR_irs
local max2=r(max)
local min2=r(min)
su policyLPR 
local max=ceil( max(`max1',`max2',r(max))/5)*5  
local min=floor(min(`min1',`min2',r(min))/5)*5  
local sep=floor(ceil((`max'-`min')/6.5/5)*5)    // gap of yticks=5

local xtx="`did'(4)`ens'"
pwcorr pcaLPR path_m_LPR_irs policyLPR
mat define Cmat=r(C) 
local cor1 = round(Cmat[1,2],.001)
local cor2 = round(Cmat[1,3],.001)
local txtx = `ens'-6
local txty = `max'-`sep'/3
local txtz = `max'-2*`sep'/3
tw (bar pcaLPRirs dateid5,col(blue) barw(1)) (bar path_m_LPR_irs dateid3,col(green) barw(1)) /// 
   (bar policyLPR dateid1,col(pink) barw(1)) if dateid1>=`did', xlab(`xtx',valuelabel labs(12pt) angle(vertical) ///
   grid glp(-) glw(thin)) xti("") ylab(`min'(`sep')`max',labs(12pt) nogrid) yti("") graphr(c(white)) /// 
   plotr(c(white)) ti("{stSerif:MPS LPR}",c(black) si(25pt)) text(`txty' `txtx' "{stSerif:ρ(MDA,PCA) = `cor1'}") ///
   text(`txtz' `txtx' "{stSerif:ρ(MDA,Δ) = `cor2'}") name(LPR,replace) xsiz(20) ysiz(8) /// 
   leg(order(1 "{stSerif:MPS}" 2 "{stSerif:MPS (NA)}" 3 "{stSerif:Actual}") r(1))
gr export OutPut/Graphics/MPSLPRvsActual.png,replace width(1500)
restore 

}

foreach xx in "checkActualRRR"{
preserve 
keep if RRRdays 
gen month = month(date),a(date)
gen day = day(date),a(date)
gen year=year(date),a(date)
tostring year month day, replace 
replace month="0"+month if length(month)==1
replace day="0"+day if length(day)==1
gen datestr = year+month+day,a(date) // font=Times New Roman

gen dateid1 = 3*_n-2, b(date)  // 1
gen dateid3 = dateid1 + 1, b(date)

qui levelsof datestr, local(dates)
local hand "label define datelabs "
cap lab drop datelabs 
local cnt 1 
foreach dt in `dates'{
	local hand `"`hand'`cnt' "{stSerif:`dt'}" "'
	local cnt = `cnt'+3
}
`hand'
lab values dateid* datelabs
destring year month day,replace

// to produce sensible tickers in y-axis
replace policyRRR = policyRRR/10

su dateid1 if RRRdays
local did=r(min) 
local ens=r(max)
su pcaRRRirs
local max1=r(max)
local min1=r(min)
su policyRRR 
local max=ceil( max(`max1',r(max))/5)*5  
local min=floor(min(`min1',r(min))/5)*5  
local sep=floor(ceil((`max'-`min')/6.5/5)*5)    // gap of yticks=5

local xtx="`did'(3)`ens'"
pwcorr pcaRRR policyRRR
mat define Cmat=r(C) 
local cor1 = round(r(rho),.001)
local txtx = `ens'-8
local txty = `max'-`sep'/3
tw (bar pcaRRRirs dateid3,col(blue) barw(1)) /// 
   (bar policyRRR dateid1,col(pink) barw(1)) if dateid1>=`did', xlab(`xtx',valuelabel labs(34pt) angle(vertical) ///
   grid glp(-) glw(thin)) xti("") ylab(`min'(`sep')`max',labs(36pt) nogrid) yti("") graphr(c(white)) /// 
   plotr(c(white)) ti("{stSerif:MPS RRR}",c(black)) text(`txty' `txtx' "{stSerif:ρ = `cor1'}") /// 
   leg(off) name(RRR,replace) xsiz(20) ysiz(8)
restore 
}

foreach xx in "checkActualLR"{
preserve 
keep if LRdays 
gen month = month(date),a(date)
gen day = day(date),a(date)
gen year=year(date),a(date)
tostring year month day, replace 
replace month="0"+month if length(month)==1
replace day="0"+day if length(day)==1
gen datestr = year+month+day,a(date) // font=Times New Roman

gen dateid1 = 3*_n-2, b(date)  // 1
gen dateid3 = dateid1 + 1, b(date)

qui levelsof datestr, local(dates)
local hand "label define datelabs "
cap lab drop datelabs 
local cnt 1 
foreach dt in `dates'{
	local hand `"`hand'`cnt' "{stSerif:`dt'}" "'
	local cnt = `cnt'+3
}
`hand'
lab values dateid* datelabs
destring year month day,replace

// to produce sensible tickers in y-axis
replace policyLR = policyLR/10

su dateid1 if LRdays
local did=r(min) 
local ens=r(max)
su pcaLRirs
local max1=r(max)
local min1=r(min)
su policyLR 
local max=ceil( max(`max1',r(max))/5)*5  
local min=floor(min(`min1',r(min))/5)*5  
local sep=floor(ceil((`max'-`min')/6.5/5)*5)    // gap of yticks=5

local xtx="`did'(3)`ens'"
pwcorr pcaLR policyLR
mat define Cmat=r(C) 
local cor1 = round(r(rho),.001)
local txtx = `ens'-8
local txty = `max'-`sep'/3
tw (bar pcaLRirs dateid3,col(blue) barw(1)) /// 
   (bar policyLR dateid1,col(pink) barw(1)) if dateid1>=`did', xlab(`xtx',valuelabel angle(vertical) ///
   grid glp(-) glw(thin)) xti("") ylab(`min'(`sep')`max', labs(36pt) nogrid) yti("") graphr(c(white)) /// 
   plotr(c(white)) ti("{stSerif:MPS LR}",c(black)) text(`txty' `txtx' "{stSerif:ρ = `cor1'}") /// 
   leg(off) name(LR,replace) xsiz(20) ysiz(8)
restore 
}

// I believe there's an anticipation effect in LPR shocks; better use intraday data? but it released at 9:30...should I use 30min in the ;ast day?
foreach xx in "checkActualLPR"{  // e.g. 2020Apr20
preserve 
keep if LPRdays 
gen month = month(date),a(date)
gen day = day(date),a(date)
gen year=year(date),a(date)
tostring year month day, replace 
replace month="0"+month if length(month)==1
replace day="0"+day if length(day)==1
gen datestr = year+month+day,a(date) // font=Times New Roman

gen dateid1 = 3*_n-2, b(date)  // 1
gen dateid3 = dateid1 + 1, b(date)

qui levelsof datestr, local(dates)
local hand "label define datelabs "
cap lab drop datelabs 
local cnt 1 
foreach dt in `dates'{
	local hand `"`hand'`cnt' "{stSerif:`dt'}" "'
	local cnt = `cnt'+3
}
`hand'
lab values dateid* datelabs
destring year month day,replace

// to produce sensible tickers in y-axis
replace policyLPR = policyLPR/10

su dateid1 if LPRdays
local did=r(min) 
local ens=r(max)
su pcaLPRirs
local max1=r(max)
local min1=r(min)
su policyLPR 
local max=ceil( max(`max1',r(max))/5)*5  
local min=floor(min(`min1',r(min))/5)*5  
local sep=floor(ceil((`max'-`min')/6.5/5)*5)    // gap of yticks=5

local xtx="`did'(3)`ens'"
pwcorr pcaLPR policyLPR
mat define Cmat=r(C) 
local cor1 = round(r(rho),0.001)
local txtx = `ens'-4
local txty = `max'-`sep'/3
tw (bar pcaLPRirs dateid3,col(blue) barw(1)) /// 
   (bar policyLPR dateid1,col(pink) barw(1)) if dateid1>=`did', xlab(`xtx',valuelabel angle(45) ///
   grid glp(-) glw(thin)) xti("") ylab(`min'(`sep')`max',nogrid) yti("") graphr(c(white)) /// 
   plotr(c(white)) ti("{stSerif:MPS LPR}",c(black)) text(`txty' `txtx' "{stSerif:ρ = `cor1'}") /// 
   leg(order(1 "{stSerif:MPS}" 2 "{stSerif:Actual}") r(1)) name(LPR,replace) xsiz(20) ysiz(9)
restore 

}

gr combine RRR LR LPR,c(1) imargin(zero) xsiz(18) ysiz(20) plotr(c(white)) graphr(c(white))
gr export OutPut/Graphics/MPSvsActual.png,width(1200) replace

format pca* %4.2fc
edit if convdays
edit if unconvdays
// copy paste & excel2latex >>> event table (full tabulation)

// overlapping dates and shock correlation >>> quite large
di _N  // 181 PBoC days 
foreach mp in conv unconv RRR LR MLF LPR{
    count if `mp'days
	pwcorr pcapbocirs pca`mp'irs if `mp'days 
	// this would be much lower if you use pcaconvirs instead: they are mostly 0 on unconvdays, so close to 0
}

foreach mp in unconv RRR LR MLF LPR{
    count if convdays & `mp'days 
	pwcorr pcaconvirs pca`mp'irs if convdays & `mp'days
}

foreach mp in RRR LPR MLF LR{
    count if unconvdays & `mp'days 
	pwcorr pcaunconvirs pca`mp'irs if unconvdays & `mp'days
}

foreach mp in LR MLF LPR{
    count if RRRdays & `mp'days 
	pwcorr pcaRRRirs pca`mp'irs if RRRdays & `mp'days
}

foreach mp in MLF LPR{
    count if LRdays & `mp'days // 0 
}

}


********** Table A. Full Tabulation of Event Timing  ***********
use TempFiles/surprise_clean,replace
keep if pbocdays==1
edit date *days if convdays==1
keep date policy pcapbocirs 
export excel using TempFiles/baselineshocktab.xlsx,firstrow(variables) replace





