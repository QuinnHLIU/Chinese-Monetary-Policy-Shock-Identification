**************************************************************
************       Haiqin Liu, June-3, 2023      *************
**************************************************************
clear all
global root "E:\Dropbox\UMP_Paper/Git"
cd $root 

global pboc_name "Baseline"
global unconv_name "Innovative"
global conv_name "Conventional"
foreach mp in LPR MLF LR RRR MPR{
	global `mp'_name "`mp'"
}


**************************** Step 0. Preprocess Data ***************************
if 0==1{
foreach ss in "part1"{
/* Part 1: Enterprise and Corporate bonds
use TempFiles/bondreg2,replace
drop path* pca* policy dfr007 dr007 irsr007* fr007 r007
drop if date < mdy(11,3,2006)  // no MPS data 
merge 1:1 stkcd date using TempFiles/supOutlier,replace update
drop _merge  // request outliers but still many ... 

replace ytm = 到期收益率 if ytm == .
replace tradevol = 成交金额 if tradevol==0 & 成交金额 != . 
drop 到期收益率 收盘价 收盘结算价 成交* track 

rename Wind债券一级分类 bondtype 
bys stkcd (bondtype):replace bondtype=bondtype[_N]
drop if bondtype =="金融债"

*keep if Wind债券一级分类 =="金融债"
*save TempFiles/finbonds,replace

drop id 
egen id=group(stkcd)
xtset id date,d  // no gaps 
bys id (date):replace ytm=ytm[_n-1] if ytm==.
cap drop ytmtrim
//winsor2 ytm,cuts(0.1 99.9) trim suffix(trim)
//su ytm*  // all that above 15bps and below 0 are trimmed 
drop ytmtrim
winsor2 ytm,cuts(0.5 99.9) trim suffix(trim)
su ytm*  // all that above 15bps and below 0 are trimmed 
foreach v in close tradenum tradevol clearprice{
	bys id (date):replace `v'=`v'[_n-1] if `v'==. 
}
replace maturity = maturity[_n-1]-1 if maturity==.  // 0
drop M2days EXRdays MPRdays
foreach v in 上市日期 实际到期日 是否跨市场 bondtype 是否上市公司 发行人企业性质 是否免税 是否含权债 发行信用评级 industry 利率类型 是否城投债ChinaBond 是否城投债THS{
	bys id (`v'):replace `v'=`v'[_N] 
}
foreach v in 发行期限 发行价格 票面利率发行时 固定利差 债券余额 税率 实际发行总额{
	bys id (`v'):replace `v'=`v'[1] 
}
merge m:1 date using TempFiles/surprise_clean,update replace
keep if _merge>2
drop _merge
drop 发行公告日 market
xtset id date, d // no gaps 
gen dytm = d.ytmtrim,a(ytmtrim)
drop if inlist(rating,"AA-","A+","A","A-","A-1") | strpos(rating,"B") | strpos(rating,"C")
compress
save TempFiles/bondreg_part1,replace
 use TempFiles/bondreg_part1,replace
merge 1:1 stkcd date using TempFiles/remainingamount,update replace 
drop if _merge==2
drop _merge
merge 1:1 stkcd date using TempFiles/remaingbalance2,update replace 
drop if _merge==2
drop _merge

preserve 
keep if 债券余额==. 
keep stkcd date 
gen cnt = _n
export excel using TempFiles/remaingbalance3.xlsx if cnt<=1000000,firstrow(variables) replace 
export excel using TempFiles/remaingbalance4.xlsx if cnt> 1000000 & cnt <=2000000,firstrow(variables) replace 
export excel using TempFiles/remaingbalance5.xlsx if cnt> 2000000 & cnt <=3000000,firstrow(variables) replace 

import excel using TempFiles/remaingbalance3.xlsx,firstrow clear 
save TempFiles/remaingbalance3,replace 
import excel using TempFiles/remaingbalance4.xlsx,firstrow clear 
save TempFiles/remaingbalance4,replace 
import excel using TempFiles/remaingbalance5.xlsx,firstrow clear 
save TempFiles/remaingbalance5,replace 
restore 

merge 1:1 stkcd date using TempFiles/remaingbalance3,update replace 
drop if _merge==2
drop _merge
merge 1:1 stkcd date using TempFiles/remaingbalance4,update replace 
drop if _merge==2
drop _merge
merge 1:1 stkcd date using TempFiles/remaingbalance5,update replace 
drop if _merge==2
drop _merge
save TempFiles/bondreg_part1_full,replace
*/
 use TempFiles/bondreg_part1_full,replace
xtset id date,d
tsfill

foreach v in stkcd 上市日期 实际到期日 是否跨市场 bondtype 是否上市公司 发行人企业性质 是否免税 是否含权债 发行信用评级{
	bys id (`v'):replace `v'=`v'[_N] 
}
foreach v in 发行期限 发行价格 票面利率发行时 固定利差 税率 实际发行总额{
	bys id (`v'):replace `v'=`v'[1] 
}
bys stkcd (date): replace clearprice=clearprice[_n-1] if clearprice==.
bys stkcd (date): replace rating=rating[_n-1] if rating==""
bys stkcd (date): replace ytm=ytm[_n-1] if ytm==.
bys stkcd (date): replace maturity=maturity[_n-1]-1 if maturity==.
replace tradevol=0 if tradevol==.

drop ytmtrim
winsor2 ytm,cuts(1 99.9) suffix(trim) trim
su ytm*   // 1.8963-44.9855

merge m:1 date using TempFiles/surprise_clean,update replace
keep if _merge>2
drop _merge

merge m:1 date using TempFiles/CDBYield,update replace 
drop if _merge == 2 
drop _merge 
qui{
findname 中债国开债到期收益率*,local(cs)
foreach cv in `cs'{
    bys stkcd (date):replace `cv'=`cv'[_n-1] if `cv'==. // weekend 
}
cap drop spreadCDB
	gen spreadCDB=ytmtrim-中债国开债到期收益率1个月  if maturity < (1 + 2)*30/2 
replace spreadCDB=ytmtrim-中债国开债到期收益率2个月  if maturity < (2 + 3)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率3个月  if maturity < (3 + 6)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率6个月  if maturity < (6 + 9)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率9个月  if maturity < (9 +12)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率1年    if maturity < (1+2)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率2年    if maturity < (2+3)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率3年    if maturity < (3+4)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率4年    if maturity < (4+5)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率5年    if maturity < (5+6)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率6年    if maturity < (6+7)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率7年    if maturity < (7+8)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率8年    if maturity < (8+9)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率9年    if maturity < (9+10)*365/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率10年   if maturity < (10+15)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率15年   if maturity < (15+20)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率20年   if maturity < (20+30)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率30年   if maturity < (30+50)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率50年   if spreadCDB == .
}

merge m:1 date using TempFiles/CGBYield,update replace 
drop if _merge == 2 
drop _merge 
qui{
findname CGB*,local(cs)
foreach cv in `cs'{
    bys stkcd (date):replace `cv'=`cv'[_n-1] if `cv'==. // weekend 
}
cap drop spreadCGB
	gen spreadCGB=ytmtrim-CGB3M  if maturity < (3 + 6)*30/2 
replace spreadCGB=ytmtrim-CGB6M  if maturity < (6 + 12)*30/2 & spreadCGB == .
replace spreadCGB=ytmtrim-CGB1Y  if maturity < (1+3)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB3Y  if maturity < (3+5)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB5Y  if maturity < (5+7)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB7Y  if maturity < (7+10)*365/2  & spreadCGB == .
replace spreadCGB=ytmtrim-CGB10Y if maturity < (10+30)*365/2 & spreadCGB == .
replace spreadCGB=ytmtrim-CGB30Y if spreadCGB == .
}

gen maturitytype=. // the maturity can't be exact. use 1-year bins
forv k=1/29{       // match maturity by annual bins 
	replace maturitytype=`k' if maturity >= 365*(`k'-1) & maturity <= 365*`k'
}
replace maturitytype=30 if maturitytype==.
preserve 
keep if strpos(rating,"AAA")
 bys maturitytype bondtype date:egen AAAyield_bytype = mean(ytmtrim)
keep maturitytype bondtype date AAAyield_bytype
duplicates drop 
save TempFiles/AAAyield,replace 
restore 
merge m:1 bondtype maturitytype date using TempFiles/AAAyield,update replace 
drop if _merge == 2 
drop _merge 
bys stkcd (date):replace AAAyield_bytype=AAAyield_bytype[_n-1] if AAAyield_bytype==.
gen spreadAAA=ytmtrim-AAAyield_bytype

qui{
    cap drop spread
findname treasury*,local(cs)
foreach cv in `cs'{
    bys stkcd (date):replace `cv'=`cv'[_n-1] if `cv'==. // weekend 
}
cap drop spreadTreasury
	gen spreadTreasury=ytmtrim-treasury1m if maturity < (1+2)*30/2
replace spreadTreasury=ytmtrim-treasury2m if maturity < (2 + 3)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury3m if maturity < (3 + 6)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury6m if maturity < (6 + 9)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury9m if maturity < (9 +12)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury1y if maturity < (1+2)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury2y if maturity < (2+3)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury3y if maturity < (3+4)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury4y if maturity < (4+5)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury5y if maturity < (5+6)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury6y if maturity < (6+7)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury7y if maturity < (7+8)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury8y if maturity < (8+9)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury9y if maturity < (9 +10)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury10y if maturity< (10+15)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury15y if maturity< (15+20)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury20y if maturity< (30+20)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury30y if maturity< (30+40)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury40y if maturity< (50+40)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury50y if spreadTreasury == .

*replace spreadCDB=ytmtrim-中债国开债到期收益率1年  if maturity == . // 0 obs
*replace spreadCGB=ytmtrim-CGB1Y  if maturity == .  // 0 changes 
*replace spreadTreasury=ytmtrim-treasury1y  if maturity == .
}

findname spread* ytmtrim ytm,local(vs)
foreach v in `vs'{
    replace `v' = `v'*100 // in bps 
}
foreach v in begdate enddate{
    bys stkcd (`v'):replace `v'=`v'[1]
}
compress
xtset id date,d

drop if bondtype=="金融债"
unique stkcd if bondtype==""  // 6033 
unique stkcd,by(bondtype)
drop _Unique 
drop if bondtype==""
xtset id date,d
replace dytm = d.ytm
findname spread*,local(sns)
foreach sv in `sns'{
	gen d`sv'=d.`sv'
}
order stkcd id date ytm ytmtrim dytm spread* dspread* maturity 债券余额 clearprice tradevol rating 是否城投债* industry *days pca* 
save TempFiles/bondreg_part1_full,replace
*use TempFiles/bondreg_part1_full,replace

keep if pbocdays==1 
save TempFiles/bondreg_part1_pboconly,replace
*use TempFiles/bondreg_part1_pboconly,replace
}

foreach ss in "part2"{
/* Part 2: GSEs, Bills, Financial etc. 
use TempFiles/supBond,replace
merge m:1 stkcd using TempFiles/bondinfo_full,keep(1 3) update
drop _merge
duplicates drop stkcd date,force   // 36 
egen cid=group(stkcd)
xtset cid date,d
merge 1:1 stkcd date using TempFiles/histRating_otherbond
drop if _merge==2 
drop _merge 
*bys cid (date):replace rating=rating[_N-1] if rating==""
*bys cid (date):replace rating=rating[_N+1] if rating==""  // 0 
replace rating=发行信用评级 if rating==""
//bys cid (rating):gen minrate=rating[1]
//bys cid (rating):gen maxrate=rating[_N]
//count if minrate != "" & rating == ""  // 0 -- so each bond has rating for each day as long as it ever has
//tab Wind债券一级分类 if rating==""
//tab Wind债券一级分类 if rating!=""  // 同业存单, 大多数金融债没有评级
//drop if 发行信用评级==""
rename 到期收益率 ytm 
rename 成交量 tradevol 
rename 成交笔数 tradenum 
rename 收盘价 close
rename 剩余期限 maturity 
rename 收盘结算价 clearprice
//drop if date<mdy(11,3,2006)  // 0 
tab rating,m
//just exclude them, but don't restrict...
drop if inlist(发行信用评级,"AA-","A+","A","A-","A-1") | strpos(发行信用评级,"B") | strpos(发行信用评级,"C")  // 77,720
merge 1:1 stkcd date using TempFiles/finbonds,update 
drop _merge

winsor2 ytm,cuts(1 99.9) suffix(trim) trim
su ytm*

merge m:1 date using TempFiles/surprise_clean,update replace
keep if _merge>2
drop _merge

drop cid 
egen cid = group(stkcd)
xtset cid date, d 
drop id 
rename cid id 

rename Wind债券一级分类 bondtype
tab 是否城投债ChinaBond  // 感觉城投债都是企业债，为什么这里还会有被classify为城投的情况呢？ 

merge m:1 date using TempFiles/CDBYield,keep(1 3) nogen
qui{
    cap drop spreadCDB
	gen spreadCDB=ytmtrim-中债国开债到期收益率1个月  if maturity < (1 + 2)*30/2 
replace spreadCDB=ytmtrim-中债国开债到期收益率2个月  if maturity < (2 + 3)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率3个月  if maturity < (3 + 6)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率6个月  if maturity < (6 + 9)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率9个月  if maturity < (9 +12)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率1年    if maturity < (1+2)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率2年    if maturity < (2+3)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率3年    if maturity < (3+4)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率4年    if maturity < (4+5)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率5年    if maturity < (5+6)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率6年    if maturity < (6+7)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率7年    if maturity < (7+8)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率8年    if maturity < (8+9)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率9年    if maturity < (9+10)*365/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率10年   if maturity < (10+15)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率15年   if maturity < (15+20)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率20年   if maturity < (20+30)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率30年   if maturity < (30+50)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率50年   if spreadCDB == .
}

merge m:1 date using TempFiles/CGBYield,keep(1 3) nogen // CGB after 2006 not downloaded
qui{
	gen spreadCGB=ytmtrim-CGB3M  if maturity < (3 + 6)*30/2 
replace spreadCGB=ytmtrim-CGB6M  if maturity < (6 + 12)*30/2 & spreadCGB == .
replace spreadCGB=ytmtrim-CGB1Y  if maturity < (1+3)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB3Y  if maturity < (3+5)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB5Y  if maturity < (5+7)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB7Y  if maturity < (7+10)*365/2  & spreadCGB == .
replace spreadCGB=ytmtrim-CGB10Y if maturity < (10+30)*365/2 & spreadCGB == .
replace spreadCGB=ytmtrim-CGB30Y if spreadCGB == .
}

preserve 
keep if strpos(发行信用评级,"AAA")

gen maturitytype=. // the maturity can't be exact. use 1-year bins
forv k=1/29{       // match maturity by annual bins 
	replace maturitytype=`k' if maturity >= 365*(`k'-1) & maturity <= 365*`k'
}
bys maturitytype bondtype date:egen AAAyield_bytype = mean(ytmtrim)
keep maturitytype bondtype date AAAyield_bytype
duplicates drop 
save TempFiles/AAAyieldOther,replace 
restore 

gen maturitytype=. // the maturity can't be exact. use 1-year bins
forv k=1/29{       // match maturity by annual bins 
	replace maturitytype=`k' if maturity >= 365*(`k'-1) & maturity <= 365*`k'
}
merge m:1 bondtype maturitytype date using TempFiles/AAAyieldOther,keep(1 3) nogen 
gen spreadAAA=ytmtrim-AAAyield_bytype

qui{
    cap drop spread
	cap drop spreadTreasury
    gen spreadTreasury=ytmtrim-treasury1m if maturity < (1+2)*30/2
replace spreadTreasury=ytmtrim-treasury2m if maturity < (2 + 3)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury3m if maturity < (3 + 6)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury6m if maturity < (6 + 9)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury9m if maturity < (9 +12)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury1y if maturity < (1+2)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury2y if maturity < (2+3)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury3y if maturity < (3+4)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury4y if maturity < (4+5)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury5y if maturity < (5+6)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury6y if maturity < (6+7)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury7y if maturity < (7+8)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury8y if maturity < (8+9)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury9y if maturity < (9 +10)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury10y if maturity< (10+15)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury15y if maturity< (15+20)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury20y if maturity< (30+20)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury30y if maturity< (30+40)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury40y if maturity< (50+40)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury50y if spreadTreasury == .

*replace spreadCDB=ytmtrim-中债国开债到期收益率1年  if maturity == . // 0 obs
*replace spreadCGB=ytmtrim-CGB1Y  if maturity == .  // 0 changes 
*replace spreadTreasury=ytmtrim-treasury1y  if maturity == .
}

findname spread* ytmtrim,local(vs)
foreach v in `vs'{
    replace `v' = `v'*100 // in bps 
}

save TempFiles/bondreg-supOther,replace 
*/
use TempFiles/bondreg-supOther,replace 

merge 1:1 stkcd date using TempFiles/remainingamount,update replace 
drop if _merge==2
drop _merge
merge 1:1 stkcd date using TempFiles/remaingbalance2,update replace 
drop if _merge==2
drop _merge

replace tradevol=成交金额 if tradevol==.
drop 成交金额 

xtset id date,d
tsfill

drop 发行公告日 dforward* dfuture* dirs* dr007 dfr007 dshibor* forward* future* fr007 r007 shibor* MPRdays M2days EXRdays

foreach v in stkcd 上市日期 实际到期日 是否跨市场 bondtype 是否上市公司 发行人企业性质 是否免税 是否含权债 发行信用评级{
	bys id (`v'):replace `v'=`v'[_N] 
}
foreach v in 发行期限 发行价格 票面利率发行时 固定利差 税率 实际发行总额{
	bys id (`v'):replace `v'=`v'[1] 
}
bys stkcd (date): replace clearprice=clearprice[_n-1] if clearprice==.
bys stkcd (date): replace rating=rating[_n-1] if rating==""
bys stkcd (date): replace ytm=ytm[_n-1] if ytm==.
bys stkcd (date): replace maturity=maturity[_n-1]-1 if maturity==.
replace tradevol=0 if tradevol==.

drop ytmtrim
winsor2 ytm,cuts(1 99.9) suffix(trim) trim
su ytm*   // 1.328-48.5037

merge m:1 date using TempFiles/surprise_clean,update replace
keep if _merge>2
drop _merge

merge m:1 date using TempFiles/CDBYield,update replace 
drop if _merge == 2 
drop _merge 
qui{
findname 中债国开债到期收益率*,local(cs)
foreach cv in `cs'{
    bys stkcd (date):replace `cv'=`cv'[_n-1] if `cv'==. // weekend 
}
cap drop spreadCDB
	gen spreadCDB=ytmtrim-中债国开债到期收益率1个月  if maturity < (1 + 2)*30/2 
replace spreadCDB=ytmtrim-中债国开债到期收益率2个月  if maturity < (2 + 3)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率3个月  if maturity < (3 + 6)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率6个月  if maturity < (6 + 9)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率9个月  if maturity < (9 +12)*30/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率1年    if maturity < (1+2)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率2年    if maturity < (2+3)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率3年    if maturity < (3+4)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率4年    if maturity < (4+5)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率5年    if maturity < (5+6)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率6年    if maturity < (6+7)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率7年    if maturity < (7+8)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率8年    if maturity < (8+9)*365/2   & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率9年    if maturity < (9+10)*365/2  & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率10年   if maturity < (10+15)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率15年   if maturity < (15+20)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率20年   if maturity < (20+30)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率30年   if maturity < (30+50)*365/2 & spreadCDB == .
replace spreadCDB=ytmtrim-中债国开债到期收益率50年   if spreadCDB == .
}

merge m:1 date using TempFiles/CGBYield,update replace 
drop if _merge == 2 
drop _merge 
qui{
findname CGB*,local(cs)
foreach cv in `cs'{
    bys stkcd (date):replace `cv'=`cv'[_n-1] if `cv'==. // weekend 
}
cap drop spreadCGB
	gen spreadCGB=ytmtrim-CGB3M  if maturity < (3 + 6)*30/2 
replace spreadCGB=ytmtrim-CGB6M  if maturity < (6 + 12)*30/2 & spreadCGB == .
replace spreadCGB=ytmtrim-CGB1Y  if maturity < (1+3)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB3Y  if maturity < (3+5)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB5Y  if maturity < (5+7)*365/2   & spreadCGB == .
replace spreadCGB=ytmtrim-CGB7Y  if maturity < (7+10)*365/2  & spreadCGB == .
replace spreadCGB=ytmtrim-CGB10Y if maturity < (10+30)*365/2 & spreadCGB == .
replace spreadCGB=ytmtrim-CGB30Y if spreadCGB == .
}

replace maturitytype=. // the maturity can't be exact. use 1-year bins
forv k=1/29{       // match maturity by annual bins 
	replace maturitytype=`k' if maturity >= 365*(`k'-1) & maturity <= 365*`k'
}
replace maturitytype=30 if maturitytype==.
drop AAAyield_bytype
preserve 
keep if strpos(rating,"AAA")
 bys maturitytype bondtype date:egen AAAyield_bytype = mean(ytmtrim)
keep maturitytype bondtype date AAAyield_bytype
duplicates drop 
save TempFiles/AAAyieldOther,replace 
restore 
merge m:1 bondtype maturitytype date using TempFiles/AAAyieldOther,update replace 
drop if _merge == 2 
drop _merge 
bys stkcd (date):replace AAAyield_bytype=AAAyield_bytype[_n-1] if AAAyield_bytype==.
replace spreadAAA=ytmtrim-AAAyield_bytype

qui{
    cap drop spread
findname treasury*,local(cs)
foreach cv in `cs'{
    bys stkcd (date):replace `cv'=`cv'[_n-1] if `cv'==. // weekend 
}
cap drop spreadTreasury
	gen spreadTreasury=ytmtrim-treasury1m if maturity < (1+2)*30/2
replace spreadTreasury=ytmtrim-treasury2m if maturity < (2 + 3)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury3m if maturity < (3 + 6)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury6m if maturity < (6 + 9)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury9m if maturity < (9 +12)*30/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury1y if maturity < (1+2)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury2y if maturity < (2+3)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury3y if maturity < (3+4)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury4y if maturity < (4+5)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury5y if maturity < (5+6)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury6y if maturity < (6+7)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury7y if maturity < (7+8)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury8y if maturity < (8+9)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury9y if maturity < (9 +10)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury10y if maturity< (10+15)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury15y if maturity< (15+20)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury20y if maturity< (30+20)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury30y if maturity< (30+40)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury40y if maturity< (50+40)*365/2 & spreadTreasury == .
replace spreadTreasury=ytmtrim-treasury50y if spreadTreasury == .

*replace spreadCDB=ytmtrim-中债国开债到期收益率1年  if maturity == . // 0 obs
*replace spreadCGB=ytmtrim-CGB1Y  if maturity == .  // 0 changes 
*replace spreadTreasury=ytmtrim-treasury1y  if maturity == .
}

findname spread* ytmtrim ytm,local(vs)
foreach v in `vs'{
    replace `v' = `v'*100 // in bps 
}

compress

drop market 
foreach v in begdate enddate{
    bys stkcd (`v'):replace `v'=`v'[1]
}
xtset id date,d
replace dytm = d.ytm
findname spread*,local(sns)
foreach sv in `sns'{
	gen d`sv'=d.`sv'
}

order stkcd id date ytm ytmtrim dytm spread* dspread* maturity 债券余额 clearprice tradevol rating 是否城投债* industry *days pca*
save TempFiles/bondreg_part2_full,replace
*use TempFiles/bondreg_part2_full,replace
 
keep if pbocdays==1
save TempFiles/bondreg_part2_pboconly,replace
*use TempFiles/bondreg_part2_pboconly,replace
}

foreach xxm in "AAAmedian"{
use TempFiles/bondreg_part1_full,replace //must use full or can't construct d.
append using TempFiles/bondreg_part2_full

drop if maturity < 365 

gen term=0 if maturity >= 3650
forv k=1/10{
    replace term=`k' if maturity >= `k'*365 & maturity < 365*(`k'+1)
}
//don't by bond types any more!
bys date term: egen ytmAAAmed=median(ytmtrim)
keep date term ytmAAAmed
duplicates drop 
egen tid=group(term)
xtset tid date,d
gen dytmAAAmed=d.ytmAAAmed
drop tid
save TempFiles/ytmAAAmed,replace 
}

use TempFiles/bondreg_part1_pboconly,replace
append using TempFiles/bondreg_part2_pboconly
merge m:1 date using TempFiles/surprise_highfreq_clean,update replace
drop if _merge==2
drop _merge
merge m:1 date using TempFiles/surprise_clean,update replace
drop if _merge==2
drop _merge *_noscale 中债国开债* CGB* path* mptic policy dirsshibor*
replace mdate=mofd(date)
replace qdate=qofd(date)
rename dr007 dr007_daily 
rename dirsr0071y dirsr0071y_daily

//retrieve monthly interest rate changes 
merge m:1 mdate using TempFiles/srpmonthly,update replace nogen
merge m:1 qdate using TempFiles/kaijiAERshock,update replace nogen
drop if stkcd==""

//trim at 1% and 99% 
cap drop dytmtrim 
winsor2 dytm,cut(1 99) trim suffix(trim)  

//sample selection
drop if maturity < 365 | strpos(rating,"B") | strpos(rating,"C") | rating=="AA-" | rating=="A-1"
drop if industry=="金融业"
drop if bondtype=="短期融资券" | bondtype==""  //金融债

replace dr007 =dr007/100 
replace dirsr0071y=dirsr0071y/100
replace dr007_daily     =dr007_daily/100 
replace dirsr0071y_daily=dirsr0071y_daily/100

gen term=0 if maturity >= 3650
forv k=1/10{
    replace term=`k' if maturity >= `k'*365 & maturity < 365*(`k'+1)
}
merge m:1 date term using RawData/ytmAAAmed,update replace  //median AAA yield with matched maturity 
drop if _merge==2
drop _merge

gen spreadAAAmed=ytmtrim-ytmAAAmed
gen dspreadAAAmed=dytmtrim-dytmAAAmed

drop id
egen id=group(stkcd)
xtset id date,d 


drop pca* broad* narrow* dirs* dfuture* dshibor* dfr007 
rename dr007 dr007_monthly
merge m:1 date using CleanData/mpshock,update replace
drop if _merge==2  // control days 
drop _merge 
replace dr007_daily=dr007_daily*100
save CleanData/bondreg,replace 
}


/*** First Stage 
use  CleanData/bondreg,replace
keep date dr007_* pca* 
duplicates drop 

foreach wind in broad narrow{
foreach xx in irs1y irs5y irsr0071y irsr0075y r0071y r0075y{
    
	local ss "`wind'`xx'"
	
	cap efolder `ss', cd("$root/OutPut/Regression/FirstStage")

	reghdfe dr007_daily `ss',a(cid) r 
}
}

*/
	
use  CleanData/bondreg,replace

**************************** Part 1. High Freq Shock ***************************
foreach yy in dytmtrim dspreadAAAmed{
foreach wind in broad narrow{
foreach xx in irs1y irs5y irsr0071y irsr0075y r0071y r0075y{
    
	local ss "`wind'`xx'"
	
	cap efolder `ss', cd("$root/OutPut/Regression/Highfreq")
	
	foreach mp in pboc MLF LPR{ //unconventional shocks only
	est clear 
	
	// model h*: OLS; model k: IV for dr007_daily; model m*: IV for dr007 (monthly)
	cap eststo h1:  reghdfe `yy'              pcapboc`ss'  if `mp'days==1,a(id) vce(cluster id)
	cap eststo k1:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k1
	estadd scalar kplm  = `e(idstat)': k1
	estadd scalar kplmp = `e(idp)': k1
	estadd scalar kpf   = `e(rkf)': k1 
	}
	cap eststo m1:ivreghdfe `yy' (dr007_month=pcapboc`ss') if `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m1
	estadd scalar kplm  = `e(idstat)': m1
	estadd scalar kplmp = `e(idp)': m1
	estadd scalar kpf   = `e(rkf)': m1 
	}

	// by ratings
	cap eststo h2:  reghdfe `yy'              pcapboc`ss'  if strpos(rating,"AAA") & `mp'days==1,a(id) vce(cluster id)
	cap eststo k2:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if strpos(rating,"AAA") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k2
	estadd scalar kplm  = `e(idstat)': k2
	estadd scalar kplmp = `e(idp)': k2
	estadd scalar kpf   = `e(rkf)': k2 
	}
	cap eststo m2:ivreghdfe `yy' (dr007_month=pcapboc`ss') if strpos(rating,"AAA") & `mp'days==1,a(id) cluster(id) first 
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m2
	estadd scalar kplm  = `e(idstat)': m2
	estadd scalar kplmp = `e(idp)': m2
	estadd scalar kpf   = `e(rkf)': m2
	}
	
	cap eststo h3:  reghdfe `yy'              pcapboc`ss'  if strpos(rating,"AA+") & `mp'days==1,a(id) vce(cluster id)
	cap eststo k3:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if strpos(rating,"AA+") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k3
	estadd scalar kplm  = `e(idstat)': k3
	estadd scalar kplmp = `e(idp)': k3
	estadd scalar kpf   = `e(rkf)': k3 
	}
	cap eststo m3:ivreghdfe `yy' (dr007_month=pcapboc`ss') if strpos(rating,"AA+") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m3
	estadd scalar kplm  = `e(idstat)': m3
	estadd scalar kplmp = `e(idp)': m3
	estadd scalar kpf   = `e(rkf)': m3
	}
	
	cap eststo h4:  reghdfe `yy'              pcapboc`ss'  if inlist(rating,"AA","AApi") & `mp'days==1,a(id) vce(cluster id)
	cap eststo k4:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if inlist(rating,"AA","AApi") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k4
	estadd scalar kplm  = `e(idstat)': k4
	estadd scalar kplmp = `e(idp)': k4
	estadd scalar kpf   = `e(rkf)': k4
	}
	cap eststo m4:ivreghdfe `yy' (dr007_month=pcapboc`ss') if inlist(rating,"AA","AApi") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m4
	estadd scalar kplm  = `e(idstat)': m4
	estadd scalar kplmp = `e(idp)': m4
	estadd scalar kpf   = `e(rkf)': m4 
	}
	
	
	// municipal or not
	cap eststo h5:  reghdfe `yy'              pcapboc`ss'  if 是否城投债ChinaBond=="是" | 是否城投债THS=="是" & `mp'days==1,a(id) vce(cluster id)
	cap eststo k5:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if 是否城投债ChinaBond=="是" | 是否城投债THS=="是" & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k5
	estadd scalar kplm  = `e(idstat)': k5
	estadd scalar kplmp = `e(idp)': k5
	estadd scalar kpf   = `e(rkf)': k5
	}
	cap eststo m5:ivreghdfe `yy' (dr007_month=pcapboc`ss') if 是否城投债ChinaBond=="是" | 是否城投债THS=="是" & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m5
	estadd scalar kplm  = `e(idstat)': m5
	estadd scalar kplmp = `e(idp)': m5
	estadd scalar kpf   = `e(rkf)': m5
	}
	
	cap eststo h6:  reghdfe `yy'              pcapboc`ss'  if ~(是否城投债ChinaBond=="是" | 是否城投债THS=="是") & `mp'days==1,a(id) vce(cluster id)
	cap eststo k6:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if ~(是否城投债ChinaBond=="是" | 是否城投债THS=="是") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k6
	estadd scalar kplm  = `e(idstat)': k6
	estadd scalar kplmp = `e(idp)': k6
	estadd scalar kpf   = `e(rkf)': k6
	}
	cap eststo m6:ivreghdfe `yy' (dr007_month=pcapboc`ss') if ~(是否城投债ChinaBond=="是" | 是否城投债THS=="是") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m6
	estadd scalar kplm  = `e(idstat)': m6
	estadd scalar kplmp = `e(idp)': m6
	estadd scalar kpf   = `e(rkf)': m6
	}
	
	//by bond types   
	local kn 7
	local th ""
	local tk ""
	local tm ""
	foreach tt in 企业债 公司债 金融债 中期票据 地方政府债{
		
	cap eststo h`kn':  reghdfe `yy'              pcapboc`ss'  if bondtype=="`tt'" & `mp'days==1,a(id) vce(cluster id)
	if _rc==0{
		local th = `"`th' "`tt'""'
	}
	cap eststo k`kn':ivreghdfe `yy' (dr007_daily=pcapboc`ss') if bondtype=="`tt'" & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
		local tk = `"`tk' "`tt'""'
		estadd scalar swf   = `e(widstat)': k`kn'
		estadd scalar kplm  = `e(idstat)': k`kn'
		estadd scalar kplmp = `e(idp)': k`kn'
		estadd scalar kpf   = `e(rkf)': k`kn'
	}
	cap eststo m`kn':ivreghdfe `yy' (dr007_month=pcapboc`ss') if bondtype=="`tt'" & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
		local tm = `"`tm' "`tt'""'
		estadd scalar swf   = `e(widstat)': m`kn'
		estadd scalar kplm  = `e(idstat)': m`kn'
		estadd scalar kplmp = `e(idp)': m`kn'
		estadd scalar kpf   = `e(rkf)': m`kn'
	}
	
	local kn = `kn'+1
	
	}
	
	estfe  *, labels(id "Bond FE" date "Date FE")
	esttab h* using `yy'`mp'.csv,indicate(`r(indicate_fe)') star(* 0.1 ** 0.05 *** 0.01) nocons replace keep(pcapboc`ss') order(pcapboc`ss') ///
		mti("Full" "AAA" "AA+" "AA" "Muni" "Non-Muni" `th') stats(N r2,labels("Obs." "R2")) 
		
	esttab k* using `yy'`mp'.csv,indicate(`r(indicate_fe)') star(* 0.1 ** 0.05 *** 0.01) nocons append  keep(dr007_daily) order(dr007_daily) ///
		mti("Full" "AAA" "AA+" "AA" "Muni" "Non-Muni" `tk') ///
		stats(swf kplm kplmp kpf,labels("SW F" "KP LM Stat. (underid)" "KP LM p-val" "KP Wald F (weak iv)"))
		
	esttab m* using `yy'`mp'.csv,indicate(`r(indicate_fe)') star(* 0.1 ** 0.05 *** 0.01) nocons append  keep(dr007_monthly) order(dr007_monthly) ///
	    mti("Full" "AAA" "AA+" "AA" "Muni" "Non-Muni" `tm') ///
		stats(swf kplm kplmp kpf,labels("SW F" "KP LM Stat. (underid)" "KP LM p-val" "KP Wald F (weak iv)"))
}
}
}
}

****************************** Part 2. Daily Shock *****************************
foreach yy in dytmtrim dspreadAAAmed{
foreach ss in irs irsr0071y irsr0075y r007{
    
	cap efolder `ss', cd("$root/OutPut/Regression/Daily")
	
	foreach mp in pboc unconv conv MLF LPR LR RRR{ //unconventional shocks only
	est clear 
	
	// model h*: OLS; model k: IV for dr007_daily; model m*: IV for dr007 (monthly)
	cap eststo h1:  reghdfe `yy'              pcapboc`ss'  if `mp'days==1,a(id) vce(cluster id)
	cap eststo k1:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k1
	estadd scalar kplm  = `e(idstat)': k1
	estadd scalar kplmp = `e(idp)': k1
	estadd scalar kpf   = `e(rkf)': k1 
	}
	cap eststo m1:ivreghdfe `yy' (dr007_month=pcapboc`ss') if `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m1
	estadd scalar kplm  = `e(idstat)': m1
	estadd scalar kplmp = `e(idp)': m1
	estadd scalar kpf   = `e(rkf)': m1 
	}

	// by ratings
	cap eststo h2:  reghdfe `yy'              pcapboc`ss'  if strpos(rating,"AAA") & `mp'days==1,a(id) vce(cluster id)
	cap eststo k2:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if strpos(rating,"AAA") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k2
	estadd scalar kplm  = `e(idstat)': k2
	estadd scalar kplmp = `e(idp)': k2
	estadd scalar kpf   = `e(rkf)': k2 
	}
	cap eststo m2:ivreghdfe `yy' (dr007_month=pcapboc`ss') if strpos(rating,"AAA") & `mp'days==1,a(id) cluster(id) first 
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m2
	estadd scalar kplm  = `e(idstat)': m2
	estadd scalar kplmp = `e(idp)': m2
	estadd scalar kpf   = `e(rkf)': m2
	}
	
	cap eststo h3:  reghdfe `yy'              pcapboc`ss'  if strpos(rating,"AA+") & `mp'days==1,a(id) vce(cluster id)
	cap eststo k3:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if strpos(rating,"AA+") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k3
	estadd scalar kplm  = `e(idstat)': k3
	estadd scalar kplmp = `e(idp)': k3
	estadd scalar kpf   = `e(rkf)': k3 
	}
	cap eststo m3:ivreghdfe `yy' (dr007_month=pcapboc`ss') if strpos(rating,"AA+") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m3
	estadd scalar kplm  = `e(idstat)': m3
	estadd scalar kplmp = `e(idp)': m3
	estadd scalar kpf   = `e(rkf)': m3
	}
	
	cap eststo h4:  reghdfe `yy'              pcapboc`ss'  if inlist(rating,"AA","AApi") & `mp'days==1,a(id) vce(cluster id)
	cap eststo k4:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if inlist(rating,"AA","AApi") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k4
	estadd scalar kplm  = `e(idstat)': k4
	estadd scalar kplmp = `e(idp)': k4
	estadd scalar kpf   = `e(rkf)': k4
	}
	cap eststo m4:ivreghdfe `yy' (dr007_month=pcapboc`ss') if inlist(rating,"AA","AApi") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m4
	estadd scalar kplm  = `e(idstat)': m4
	estadd scalar kplmp = `e(idp)': m4
	estadd scalar kpf   = `e(rkf)': m4 
	}
	
	
	// municipal or not
	cap eststo h5:  reghdfe `yy'              pcapboc`ss'  if   是否城投债ChinaBond=="是" | 是否城投债THS=="是" & `mp'days==1,a(id) vce(cluster id)
	cap eststo k5:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if   是否城投债ChinaBond=="是" | 是否城投债THS=="是" & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k5
	estadd scalar kplm  = `e(idstat)': k5
	estadd scalar kplmp = `e(idp)': k5
	estadd scalar kpf   = `e(rkf)': k5
	}
	cap eststo m5:ivreghdfe `yy' (dr007_month=pcapboc`ss') if   是否城投债ChinaBond=="是" | 是否城投债THS=="是" & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m5
	estadd scalar kplm  = `e(idstat)': m5
	estadd scalar kplmp = `e(idp)': m5
	estadd scalar kpf   = `e(rkf)': m5
	}
	
	cap eststo h6:  reghdfe `yy'              pcapboc`ss'  if ~(是否城投债ChinaBond=="是" | 是否城投债THS=="是") & `mp'days==1,a(id) vce(cluster id)
	cap eststo k6:ivreghdfe `yy' (dr007_daily=pcapboc`ss') if ~(是否城投债ChinaBond=="是" | 是否城投债THS=="是") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': k6
	estadd scalar kplm  = `e(idstat)': k6
	estadd scalar kplmp = `e(idp)': k6
	estadd scalar kpf   = `e(rkf)': k6
	}
	cap eststo m6:ivreghdfe `yy' (dr007_month=pcapboc`ss') if ~(是否城投债ChinaBond=="是" | 是否城投债THS=="是") & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
	estadd scalar swf   = `e(widstat)': m6
	estadd scalar kplm  = `e(idstat)': m6
	estadd scalar kplmp = `e(idp)': m6
	estadd scalar kpf   = `e(rkf)': m6
	}
	
	//by bond types   
	local kn 7
	local th ""
	local tk ""
	local tm ""
	foreach tt in 企业债 公司债 金融债 中期票据 地方政府债{
		
	cap eststo h`kn':  reghdfe `yy'              pcapboc`ss'  if bondtype=="`tt'" & `mp'days==1,a(id) vce(cluster id)
	if _rc==0{
		local th = `"`th' "`tt'""'
	}
	cap eststo k`kn':ivreghdfe `yy' (dr007_daily=pcapboc`ss') if bondtype=="`tt'" & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
		local tk = `"`tk' "`tt'""'
		estadd scalar swf   = `e(widstat)': k`kn'
		estadd scalar kplm  = `e(idstat)': k`kn'
		estadd scalar kplmp = `e(idp)': k`kn'
		estadd scalar kpf   = `e(rkf)': k`kn'
	}
	cap eststo m`kn':ivreghdfe `yy' (dr007_month=pcapboc`ss') if bondtype=="`tt'" & `mp'days==1,a(id) cluster(id) first
	if _rc==0{
		local tm = `"`tm' "`tt'""'
		estadd scalar swf   = `e(widstat)': m`kn'
		estadd scalar kplm  = `e(idstat)': m`kn'
		estadd scalar kplmp = `e(idp)': m`kn'
		estadd scalar kpf   = `e(rkf)': m`kn'
	}
	
	local kn = `kn'+1
	
	}
	
	estfe  *, labels(id "Bond FE" date "Date FE")
	esttab h* using `yy'`mp'.csv,indicate(`r(indicate_fe)') star(* 0.1 ** 0.05 *** 0.01) nocons replace keep(pcapboc`ss') order(pcapboc`ss') ///
		mti("Full" "AAA" "AA+" "AA" "Muni" "Non-Muni" `th') stats(N r2,labels("Obs." "R2")) ar2
		
	esttab k* using `yy'`mp'.csv,indicate(`r(indicate_fe)') star(* 0.1 ** 0.05 *** 0.01) nocons append  keep(dr007_daily) order(dr007_daily) ///
		mti("Full" "AAA" "AA+" "AA" "Muni" "Non-Muni" `tk') ///
		stats(swf kplm kplmp kpf,labels("SW F" "KP LM Stat. (underid)" "KP LM p-val" "KP Wald F (weak iv)"))
		
	esttab m* using `yy'`mp'.csv,indicate(`r(indicate_fe)') star(* 0.1 ** 0.05 *** 0.01) nocons append  keep(dr007_monthly) order(dr007_monthly) ///
	    mti("Full" "AAA" "AA+" "AA" "Muni" "Non-Muni" `tm') ///
		stats(swf kplm kplmp kpf,labels("SW F" "KP LM Stat. (underid)" "KP LM p-val" "KP Wald F (weak iv)"))
}
}
}






