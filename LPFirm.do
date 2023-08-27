*********************************************************************
****************  LPIRFs of Firms (STATA version)  ******************
****************       Haiqin Liu, May-4-2022      ******************
*********************************************************************
global root "E:\Dropbox\UMP_Paper/Git"
cd "$root"

clear all
set maxvar 120000

//* (1) aggregate daily shocks to quarterly *//
foreach xx in "quarterlyAggregate"{ 
    
	
use CleanData/mpshock,replace
drop *days   // we then lose the event timing info 

gen qdate = qofd(date)
findname pca*,local(vs)
foreach v in `vs'{
	bys qdate:egen `v'ss=total(`v'),missing 
	drop `v'
	rename `v'ss `v'
}

bys qdate:egen dr007ss = total(dr007),missing 
drop dr007
rename dr007ss dr007

keep qdate pca* dr007 
duplicates drop 
tsset qdate,q

tw line pcapbocirs pcaconvirs pcaunconvirs pcapbocbroadirs1y qdate,xti("") yti("")
gr export OutPut/Figure/shocks_quarterly.png,replace
save TempFiles/shocks_quarterly,replace

}

for xx in "constructVariable"{   // now I start to hesitate; should you deflate? have you winsorized?

import excel using RawData/firm_quarterly_bs.xlsx,firstrow clear

drop if 所有者权益合计<0  //2,250

//import excel using TempFiles/BSquarterlyv2.xlsx,firstrow clear
gen Date=date(date,"YMD"),a(date)
gen qdate=qofd(Date),a(date)
gen year=year(Date),a(date)
drop if year<2006 | year>2022
drop date Date 
merge m:1 qdate using TempFiles/CPIcore,nogen keep(1 3)
//su cpicore  //complete
egen fid=group(stockcode)
xtset fid qdate,q
tsfill 
foreach xx in stockcode 首发上市日期 公司性质同花顺 发行人企业性质公司公告 所属证监会行业{
    bys fid (qdate):replace `xx'=`xx'[_n-1] if `xx'==""
	forv tt=1/20{
	bys fid (qdate):replace `xx'=`xx'[_n+1] if `xx'==""
	}
}


findname stockcode qdate year 首发上市日期 员工总数 公司性质同花顺 发行人企业性质公司公告 所属证监会行业 净利润营业总收入 净资产收益率ROE* cpicore,not local(xs)
foreach v in `xs'{
	bys stockcode (qdate):ipolate `v' qdate,gen(`v'ipo)
	winsor2 `v'ipo,cut(1 99) suffix(ws)
	gen `v'def=`v'ipows/cpicore/10^8  // in 100 Mil RMB deflated 
	//[might not need this since we'll basically use ratios or growth rates]
	su `v'* 
}
//ratios --- winsorize 
findname 净利润营业总收入 净资产收益率ROE*,local(xs)
foreach v in `xs'{
	bys stockcode (qdate):ipolate `v' qdate,gen(`v'ipo)
	winsor2 `v'ipo,cut(1 99) suffix(ws)
	su `v'* 
}
// this data is annual! 
bys stockcode year (员工总数):replace 员工总数=员工总数[1]
foreach v in 员工总数{  
	bys stockcode (qdate):ipolate `v' qdate,gen(`v'ipo)
	winsor2 `v'ipo,cut(1 99) suffix(ws)
	su `v'* 
}


xtset fid qdate,q


** Investment 
gen logasset = ln(资产总计ipows)
gen inv1=d.logasset
lab var inv1 "Investment: △ln(Asset)"

//replace 投资活动现金流入小计ipows=-投资活动现金流入小计ipows
//egen inv3=rowtotal(投资活动现金流出小计ipows 投资活动现金流入小计ipows),missing

gen inv3=投资支付的现金ipows/固定资产ipows
winsor2 inv3,suffix(ws) cut(1 99)
drop inv3 
rename inv3ws inv3 
lab var inv3 "I / K"


** Sales
gen logsales=log(营业总收入ipows)
gen dlogsale=d.logsales 
lab var logsales "Ln(Sales)"
lab var dlogsale "D.Ln(Sales)"


** Employment 
rename 员工总数 emp 
gen logemp=ln(emp)
gen dlogemp=logemp-l4.logemp //annual !
lab var emp "Employee"
lab var logemp "Ln(Employee)"
lab var dlogemp "D.Ln(Employee)"


** Capital Structure
gen cash=-货币资金ipows
egen asset_=rowtotal(资产总计ipows cash),missing
egen equity=rowtotal(所有者权益合计ipows cash),missing
gen Lev=asset_/equity
replace Lev=资产总计ipows/所有者权益合计ipows if Lev<=0
winsor2 Lev,cut(1 99) suffix(ws)
drop Lev 
rename Levws Lev 
lab var Lev "Leverage"  // (L-Cash)/(A-Cash)
gen lnLev=ln(Lev)
lab var lnLev "Ln(Leverage)"
gen dlnLev=d.lnLev
lab var dlnLev "D.Ln(Leverage)"


** Operational Liquidity (based on account payables/bills)
egen optdebt = rowtotal(应收账款ipows 应收票据ipows 预付款项ipows),missing //应收>0 >>> weaker power
egen optasset= rowtotal(应付账款ipows 应付票据ipows 应收款项融资ipows),missing
replace optdebt = -optdebt 
egen optlev = rowtotal(optasset optdebt),missing
replace optlev=optlev/营业总收入ipows //类似应收账款周转率
winsor2 optlev,suffix(ws) cut(1 99)
hist optlevws
drop optlev 
rename optlevws optlev
lab var optlev "Operating Liquidity"
gen lnoptlev=ln(optlev)
gen doptlev=d.lnoptlev
lab var lnoptlev "Ln(Operating Liquidity)"
lab var optlev "D.Operating Liquidity"


** Debt 
egen shortdebt=rowtotal(短期借款ipows 应付票据ipows 应付账款ipows 应付职工薪酬ipows 应付利息ipows 应付股利ipows 应付手续费及佣金ipows 应交税费ipows 一年内到期的非流动负债ipows),missing
egen longdebt=rowtotal(长期借款ipows 应付债券ipows),missing 
egen totaldebt=rowtotal(shortdebt longdebt),missing


** Profitability
replace 递延所得税资产ipows=-递延所得税资产ipows
egen tobinq=rowtotal(总市值ipows 优先股ipows totaldebt 递延所得税资产ipows 递延所得税负债ipows),missing
replace tobinq=tobinq/资产总计ipows
// (mktval + preferredstock + totaldebt - deferredtax)/asset  // - 投资免税收入,这个数据没有
gen dq=d.tobinq
lab var tobinq "Tobin's Q"
lab var dq "D.Tobin's Q"

su 净资产收益率*
rename 净资产收益率ROE摊薄公布值ipows roe  
gen droe=d.roe
lab var roe "ROE"
lab var droe "D.ROE"


rename totaldebt debt 
gen lndebt=log(debt)
gen dlndebt=d.lndebt 
lab var debt "Toatl Debt"
lab var lndebt "Ln(Toatl Debt)"
lab var dlndebt "D.Ln(Toatl Debt)"

rename longdebt ldebt 
gen lnldebt=log(ldebt)
gen dlnldebt=d.lnldebt 
lab var ldebt "Long Debt"
lab var lnldebt "Ln(Long Debt)"
lab var dlnldebt "D.Ln(Long Debt)"

rename shortdebt sdebt 
gen lnsdebt=log(sdebt)
gen dlnsdebt=d.lnsdebt 
lab var sdebt "Short Debt"
lab var lnsdebt "Ln(Short Debt)"
lab var dlnsdebt "D.Ln(Short Debt)"



bys qdate:egen medsize=median(logasset)
bys qdate:egen stdsize=sd(logasset)
gen Sizeindex=(logasset-medsize)/stdsize

gen startdate=date(首发上市日期,"YMD")
gen startyear=year(startdate)
gen age=year-startyear

egen hasdata=rownonmiss(资产总计ipows 负债合计ipows) 
gen hasdatayear=year if hasdata>0
bys fid:egen hasdataminyr=min(hasdatayear)
replace age=year-hasdataminyr if age==.  // 0
replace age=0 if age<0  // 0
winsor2 age,suffix(ws99) cut(1 99)
winsor2 age,suffix(ws95) cut(1 95)
winsor2 age,suffix(ws90) cut(1 90)
preserve 
keep fid year age agews*
dstat histogram age agews99 agews95 agews90,nose common graph(merge)
gr export OutPut/Graphics/age_distribution.png,replace
restore 
drop age 
rename agews99 age
bys qdate:egen medage=median(age)
bys qdate:egen stdage=sd(age)
gen Ageindex=(age-medage)/stdage // truncate?

gen SA=(-.737*Sizeindex)+(.043*Sizeindex^2)-(.04*Ageindex)  // see hadlock 2010
su SA 
hist SA  //the distri. looks perfect...
gen networth=log(所有者权益合计ipows)
compress
save TempFiles/firm_quarterly_lp,replace
}


 use TempFiles/firm_quarterly_lp,replace
merge m:1 qdate using TempFiles/shocks_quarterly,nogen keep(3)


gen SOE=inlist(发行人企业性质公司公告,"中央企业","地方国有企业") | inlist(公司性质同花顺,"地市国资控股","省属国资控股","央企国资控股","其他国有")
unique stockcode,by(SOE)
/*
  +---------------+
  | SOE   _Unique |
  |---------------|
  |   0      3496 |
  |   1      1415 |
  +---------------+
*/
drop _Unique


gen horizon=_n-1 if _n<=$horizon +1

	gen group = 1 if Sizeindex >  0 & Ageindex >  0 
replace group = 2 if Sizeindex >  0 & Ageindex <= 0 
replace group = 3 if Sizeindex <= 0 & Ageindex >  0 
replace group = 4 if Sizeindex <= 0 & Ageindex <= 0 
//replace group = 1 if Sizeindex == . | Ageindex == .  // 843

xtset fid qdate,q
cap drop inv1 
cap drop inv2 
cap drop empl 
cap drop Debt Ldebt Sdebt Q Sales ROE  //levr OptLev


gen inv1=d.logasset
gen inv2K_1 = inv1/l1.固定资产ipows
winsor2 inv2K_1,cut(1 99) suffix(ws)
hist inv2K_1ws

gen sale2K_1=营业收入ipows/l1.固定资产ipows
hist sale2K_1
winsor2 sale2K_1,cut(1 99) suffix(ws)
hist sale2K_1ws  // very fat-tailed 

rename pcapbocbroadirs1y broad 
rename pcapbocnarrowirs1y narrow 
rename pcapbocirs mps 

gen l1dr007=l1.dr007


keep fid qdate inv1 inv2K_1ws logasset networth SA logemp ln*debt sale2K_1ws roe group SOE mps broad narrow l1dr007 horizon Lev tobinq



/* 1. Not Cumulative IRFs: lnY_{i,t+h} = a_i + b_h \widehat{MPS}_t + \sum_{k=1}^p X_{i,t-k} + e_{i,t+h} */

foreach xm in ols iv{ //
forv ll=4(4)8{  // 
	
	global lags `ll'
	
	global firmcontrols ""
	qui foreach v in logasset networth tobinq{  // logliability logsales mktval beta roe
		global firmcontrols "$firmcontrols L(1/$lags ).`v'"
	}
	
foreach sn in mps broad{ //narrow unconv conv LPR MLF LR RRR MPR

	global shock="`sn'"  // baseline PCA daily shock 
	cap gen l1`sn'=l1.$shock 
	
	forv h=0/$horizon{
		
		qui{
			gen invest=f`h'.inv1 //-l.inv1
			gen invt=f`h'.inv2K_1ws //-l.inv2K_1ws
			gen empl=f`h'.logemp  //-l.logemp
			
			gen Debt=f`h'.lndebt //-l.lndebt
			//gen Ldebt=f`h'.lnldebt-l.lnldebt 
			//gen Sdebt=f`h'.lnsdebt-l.lnsdebt 
			
			//gen Q=f`h'.tobinq-l.tobinq 
			//gen Sales=f`h'.logsale-l.logsale
			gen Sales=f`h'.sale2K_1ws //-l.sale2K_1ws
			gen ROE=f`h'.roe //-l.roe
		}
		
		
		foreach yv in invest empl Sales Debt{ // invt Ldebt Sdebt ROE
			est clear 
			
			qui forv k=1/3{  // 3 specifications
				foreach mm in _b u1 l1 u2 l2{
					foreach esm in iv ols{
						cap gen `yv'`sn'm`k'`esm'`mm'=. 
					}
				}
			}
			local kn = `h'+1	
			if "`xm'"=="ols"{
			
			** (1) no controls 
			cap newey `yv' $shock , lag(`kn') force
			if _rc==0{
			qui ereturn display,level($ci1 )
			mat define results=r(table)  
			qui replace `yv'`sn'm1ols_b=results[1,1] if horizon==`h'
			qui replace `yv'`sn'm1olsl1=results[5,1] if horizon==`h' // lower bound 
			qui replace `yv'`sn'm1olsu1=results[6,1] if horizon==`h' // upper bound 
			
			qui ereturn display,level($ci2 )
			mat define results=r(table)  
			qui replace `yv'`sn'm1olsl2=results[5,1] if horizon==`h' // lower bound 
			qui replace `yv'`sn'm1olsu2=results[6,1] if horizon==`h' // upper bound 
			
			
			** (2) +Firm FEs
			cap newey `yv' $shock i.fid, lag(`kn') force
			if _rc==0{
			qui ereturn display,level($ci1 )
			mat define results=r(table)  
			qui replace `yv'`sn'm2ols_b=results[1,1] if horizon==`h'
			qui replace `yv'`sn'm2olsl1=results[5,1] if horizon==`h' // lower bound 
			qui replace `yv'`sn'm2olsu1=results[6,1] if horizon==`h' // upper bound 
			
			qui ereturn display,level($ci2 )
			mat define results=r(table)  
			qui replace `yv'`sn'm2olsl2=results[5,1] if horizon==`h' // lower bound 
			qui replace `yv'`sn'm2olsu2=results[6,1] if horizon==`h' // upper bound 
			}
			
			** (3) +Firm Controls 
			cap newey `yv' $shock L(1/$lags ).$shock $firmcontrols i.fid, lag(`kn') force
			if _rc==0{
			qui ereturn display,level($ci1 )
			mat define results=r(table)  
			qui replace `yv'`sn'm3ols_b=results[1,1] if horizon==`h'
			qui replace `yv'`sn'm3olsl1=results[5,1] if horizon==`h' // lower bound 
			qui replace `yv'`sn'm3olsu1=results[6,1] if horizon==`h' // upper bound 
			
			qui ereturn display,level($ci2 )
			mat define results=r(table)  
			qui replace `yv'`sn'm3olsl2=results[5,1] if horizon==`h' // lower bound 
			qui replace `yv'`sn'm3olsu2=results[6,1] if horizon==`h' // upper bound 
			}
			
			}
			}
			
			
			if "`xm'"=="iv"{
			  
			//instead of run first stage at each horizon, run once and plug in 
			//ivreghdfe `yv' (l1dr007=l1`sn'),a(fid) gmm2s kernel(bar) robust bw(`kn')
			qui reg dr007 $shock , r //but don't you think you should run this on time series only?
			qui predict shat1,xb 
			
			cap newey shat1, lag(`kn') force
			if _rc==0{
					ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm1iv_b=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm1ivl1=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm1ivu1=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm1ivl2=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm1ivu2=results[6,1] if horizon==`h' // upper bound 
			
			
			//cap ivreghdfe `yv' (l1dr007=l1`sn'),a(fid) gmm2s kernel(bar) robust bw(`kn')
			qui reghdfe dr007 $shock , a(fid)
			qui predict shat2,xb 
			
			cap newey shat2 i.fid, lag(`kn') force
			if _rc==0{
				qui ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm2iv_b=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm2ivl1=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm2ivu1=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm2ivl2=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm2ivu2=results[6,1] if horizon==`h' // upper bound 
			}
				
			//cap ivreghdfe `yv' $firmcontrols (l1dr007=l1`sn'),a(fid) gmm2s kernel(bar) robust bw(`kn')
			qui reghdfe dr007 $shock $firmcontrols L(1/$lags ).$shock , a(fid)
			qui predict shat3,xb 
			
			cap newey shat3 $firmcontrols L(1/$lags ).$shock i.fid, lag(`kn') force
			if _rc==0{
				qui ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm3iv_b=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm3ivl1=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm3ivu1=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm3ivl2=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm3ivu2=results[6,1] if horizon==`h' // upper bound 
			}
			}
			drop shat1 shat2 shat3 
			}
		}
		drop invest invt empl Sales ROE Debt
		}      
	}
} 
}
save OutPut/LP/lpfirm_quarterly,replace 
 use OutPut/LP/lpfirm_quarterly,replace 

gen zero = 0 
global tic "0(2)6"
global asp 0.45


global invest_name "Log Asset Growth"
global invt_name "Investment / Fixed Assets"
global empl_name "Employment"
global Debt_name "Total Debt"
global Ldebt_name "Long-term Debt"
global Sdebt_name "Short-term Debt"
global Q_name "Tobin's Q"
global Sales_name "Sales / Fixed Assets"
global ROE_name "ROE"


keep if horizon!=. 
keep if horizon<=6
keep horizon *m*ols* *m*iv*
gen zero=0

* for various FE models
gen hor=3*horizon 
gen h2=hor+.3
gen h3=hor+.6
gen h4=hor+.9

tostring horizon, replace 

local hand "label define datelabs "
local obs=_N
forv nn =1/`obs'{
    local dn=hor[`nn']
	local cnt=horizon[`nn']
	local hand `"`hand'`dn' "{stSerif:`cnt'}" "' // font=Times New Roman
}
`hand',replace 
lab values hor datelabs
destring horizon,replace



/* Y_{t+h}-Y_{t-1} = a+ b1 MPS_1 + b2 BC_1 x MPS_1 + et,h */
foreach mm in iv{  //ols
    foreach ii in broad{ // broad narrow
		
	cap efolder `ii',cd("$root/OutPut\LP\Quarterly")
	cap efolder `mm',cd("$root/OutPut\LP\Quarterly/`ii'")
	
	foreach yv in ROE Sales{ //invt invest  ROE  empl Debt
		su hor
		local ma=r(max)
		local mi=r(min)
		
		forv kk=1/3{
			foreach xx in _b u1 l1{
				replace `yv'`ii'm`kk'`mm'`xx'=`yv'`ii'm`kk'`mm'`xx'/100 // 这个单位还要再确认一下
			}
		}
		
		tw (scatter `yv'`ii'm1`mm'_b hor,mc(green)) (scatter `yv'`ii'm2`mm'_b h2,mc(maroon)) ///
		   (scatter `yv'`ii'm3`mm'_b h3 ,mc(navy )) ///
		   (rcap `yv'`ii'm1`mm'u1 `yv'`ii'm1`mm'l1 hor,lc(green))  ///
		   (rcap `yv'`ii'm2`mm'u1 `yv'`ii'm2`mm'l1 h2,lc(maroon))  ///
		   (rcap `yv'`ii'm3`mm'u1 `yv'`ii'm3`mm'l1 h3,lc(navy )),  ///
		   xti("{stSerif:Years Forward}") name(`mm'`yv',replace) yline(0,lc(red) lp(dash))  ///
		   leg(order(4 "{stSerif:No controls}" 5 "{stSerif:Firm FEs}" 6 "{stSerif:+ Controls}") r(1)) ///
		   ti("{stSerif:$`yv'_name}",c(black) siz(25pt)) ylab(,labs(20pt) format(%2.1fc) angle(horizontal)) ///
		   xlab(`mi'(3)`ma',valuelabel labs(20pt)) graphr(c(white)) plotr(c(white))
		gr export `yv'_lag1.png, replace
	}
	}
}


cd $root
use TempFiles/lpfirm_quarterly,replace 
drop *_b *u1 *u2 *l1 *l2

// foreach xx in SOE group{
// 	bys fid (qdate):replace `xx'=`xx'[_n-1] if `xx'==.
// }


forv ll=1/1{
	
	global lags `ll'
	
	global firmcontrols ""
	qui foreach v in logasset networth tobinq{  // logliability logsales mktval beta roe
		global firmcontrols "$firmcontrols L(1/$lags ).`v'"
	}
	
foreach sn in mps broad{ //narrow unconv conv LPR MLF LR RRR MPR

foreach xm in ols iv{ //

	global shock="`sn'"  // baseline PCA daily shock 
	cap gen l1`sn'=l1.$shock 
	
	foreach yv in invest invt empl Debt Sales ROE{ //  Ldebt Sdebt
		est clear 
		
		qui forv model=1/3{  // 3 specifications
			foreach mm in _b u1 l1 u2 l2{
-






































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































-
-
-
-
-
-
-
-
+---				foreach esm in iv ols{
		00000000000000000000000000000000.................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................222222222222222222222222222222222222222
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222..........................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................forv pp=0/1{
						cap gen `yv'`sn'm`model'`esm'`mm'soe`pp'=. 
					}
				}
			}
		}
		
		forv h=0/$horizon{
			qui{
				gen invest=f`h'.inv1-l.inv1
				gen invt=f`h'.inv2K_1ws-l.inv2K_1ws
				gen empl=f`h'.logemp-l.logemp
				
				gen Debt=f`h'.lndebt-l.lndebt
				gen Sales=f`h'.sale2K_1ws-l.sale2K_1ws
				gen ROE=f`h'.roe-l.roe
			}
			
			local kn = `h'+1	
		
			if "`xm'"=="ols"{
						
				forv k=0/1{  // 民营 vs 国企 子样本
				est clear 
				
				** (1) No Controls
				cap newey `yv' L(1/$lags ).$shock if SOE==`k', lag(`kn') force
				if _rc==0{
					ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm1ols_bsoe`k'=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm1olsl1soe`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm1olsu1soe`k'=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm1olsl2soe`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm1olsu2soe`k'=results[6,1] if horizon==`h' // upper bound 
				
				** (2) +Firm FEs
				cap newey `yv' L(1/$lags ).$shock i.fid if SOE==`k', lag(`kn') force
				if _rc==0{
				qui ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm2ols_bsoe`k'=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm2olsl1soe`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm2olsu1soe`k'=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm2olsl2soe`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm2olsu2soe`k'=results[6,1] if horizon==`h' // upper bound 
				}
				
				** (3) +Firm Controls
				cap newey `yv' L(1/$lags ).$shock $firmcontrols i.fid if SOE==`k', lag(`kn') force
				if _rc==0{
				qui ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm3ols_bsoe`k'=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm3olsl1soe`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm3olsu1soe`k'=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm3olsl2soe`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm3olsu2soe`k'=results[6,1] if horizon==`h' // upper bound 
				}
				}
				}
			}
			if "`xm'"=="iv"{
						
				forv k=0/1{  // 民营 vs 国企 子样本
				est clear 
				
				cap ivreghdfe `yv' (l1dr007=l1`sn') if SOE==`k', gmm2s kernel(bar) robust bw(`kn')
				if _rc==0{
						ereturn display,level($ci1 )
					mat define results=r(table)  
					qui replace `yv'`sn'm1iv_bsoe`k'=results[1,1] if horizon==`h'
					qui replace `yv'`sn'm1ivl1soe`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm1ivu1soe`k'=results[6,1] if horizon==`h' // upper bound 
					
					qui ereturn display,level($ci2 )
					mat define results=r(table)  
					qui replace `yv'`sn'm1ivl2soe`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm1ivu2soe`k'=results[6,1] if horizon==`h' // upper bound 
				
				cap ivreghdfe `yv' (l1dr007=l1`sn') if SOE==`k',a(fid) gmm2s kernel(bar) robust bw(`kn')
				if _rc==0{
					qui ereturn display,level($ci1 )
					mat define results=r(table)  
					qui replace `yv'`sn'm2iv_bsoe`k'=results[1,1] if horizon==`h'
					qui replace `yv'`sn'm2ivl1soe`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm2ivu1soe`k'=results[6,1] if horizon==`h' // upper bound 
					
					qui ereturn display,level($ci2 )
					mat define results=r(table)  
					qui replace `yv'`sn'm2ivl2soe`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm2ivu2soe`k'=results[6,1] if horizon==`h' // upper bound 
				}
					
				cap ivreghdfe `yv' $firmcontrols (l1dr007=l1`sn') if SOE==`k',a(fid) gmm2s kernel(bar) robust bw(`kn')
				if _rc==0{
					qui ereturn display,level($ci1 )
					mat define results=r(table)  
					qui replace `yv'`sn'm3iv_bsoe`k'=results[1,1] if horizon==`h'
					qui replace `yv'`sn'm3ivl1soe`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm3ivu1soe`k'=results[6,1] if horizon==`h' // upper bound 
					
					qui ereturn display,level($ci2 )
					mat define results=r(table)  
					qui replace `yv'`sn'm3ivl2soe`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm3ivu2soe`k'=results[6,1] if horizon==`h' // upper bound 
				}
				}
				}
			}
			drop invest invt empl Sales ROE Debt
		}
	}
}
}
}

foreach xm in ols iv{ // SA groupings
forv ll=1/1{
	
	global lags `ll'
	
	global firmcontrols ""
	qui foreach v in logasset networth tobinq{  // logliability logsales mktval beta roe
		global firmcontrols "$firmcontrols L(1/$lags ).`v'"
	}
	
	
foreach sn in mps broad{ //narrow unconv conv LPR MLF LR RRR MPR

	global shock="`sn'"  // baseline PCA daily shock 
	cap gen l1`sn'=l1.$shock 
	
	foreach yv in invest invt empl Sales ROE Debt{ //  Ldebt Sdebt
		est clear 
		
		qui forv spec=1/3{  // 3 specifications
			foreach mm in _b u1 l1 u2 l2{
				foreach esm in iv ols{
				    forv pp=1/4{
						gen `yv'`sn'm`spec'`esm'`mm'grp`pp'=. 
					}
				}
			}
		}
		
		forv h=0/$horizon{
			
			qui{
				gen invest=f`h'.inv1-l.inv1
				gen invt=f`h'.inv2K_1ws-l.inv2K_1ws
				gen empl=f`h'.logemp-l.logemp
				
				gen Debt=f`h'.lndebt-l.lndebt
				gen Sales=f`h'.sale2K_1ws-l.sale2K_1ws
				gen ROE=f`h'.roe-l.roe
			}
			
			
			local kn = `h'+1	
		
		if "`xm'"=="ols"{
						
			forv k=1/4{  // 民营 vs 国企 子样本
				est clear 
				
				** (1) No Controls
				cap newey `yv' L(1/$lags ).$shock if group==`k', lag(`kn') force
				if _rc==0{
					ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm1ols_bgrp`k'=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm1olsl1grp`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm1olsu1grp`k'=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm1olsl2grp`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm1olsu2grp`k'=results[6,1] if horizon==`h' // upper bound 
				
				
				** (2) +Firm FEs
				cap newey `yv' L(1/$lags ).$shock i.fid if group==`k', lag(`kn') force
				if _rc==0{
				qui ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm2ols_bgrp`k'=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm2olsl1grp`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm2olsu1grp`k'=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm2olsl2grp`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm2olsu2grp`k'=results[6,1] if horizon==`h' // upper bound 
				}
				
				** (3) +Firm Controls
				cap newey `yv' L(1/$lags ).$shock $firmcontrols i.fid if group==`k', lag(`kn') force
				if _rc==0{
				qui ereturn display,level($ci1 )
				mat define results=r(table)  
				qui replace `yv'`sn'm3ols_bgrp`k'=results[1,1] if horizon==`h'
				qui replace `yv'`sn'm3olsl1grp`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm3olsu1grp`k'=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level($ci2 )
				mat define results=r(table)  
				qui replace `yv'`sn'm3olsl2grp`k'=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'm3olsu2grp`k'=results[6,1] if horizon==`h' // upper bound 
				}
				}
			}
		}
		if "`xm'"=="iv"{
						
			forv k=1/4{  // 民营 vs 国企 子样本
				est clear 
				
				cap ivreghdfe `yv' (l1dr007=l1`sn') if group==`k', gmm2s kernel(bar) robust bw(`kn')
				if _rc==0{
						ereturn display,level($ci1 )
					mat define results=r(table)  
					qui replace `yv'`sn'm1iv_bgrp`k'=results[1,1] if horizon==`h'
					qui replace `yv'`sn'm1ivl1grp`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm1ivu1grp`k'=results[6,1] if horizon==`h' // upper bound 
					
					qui ereturn display,level($ci2 )
					mat define results=r(table)  
					qui replace `yv'`sn'm1ivl2grp`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm1ivu2grp`k'=results[6,1] if horizon==`h' // upper bound 
				
				cap ivreghdfe `yv' (l1dr007=l1`sn') if group==`k',a(fid) gmm2s kernel(bar) robust bw(`kn')
				if _rc==0{
						ereturn display,level($ci1 )
					mat define results=r(table)  
					qui replace `yv'`sn'm2iv_bgrp`k'=results[1,1] if horizon==`h'
					qui replace `yv'`sn'm2ivl1grp`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm2ivu1grp`k'=results[6,1] if horizon==`h' // upper bound 
					
					qui ereturn display,level($ci2 )
					mat define results=r(table)  
					qui replace `yv'`sn'm2ivl2grp`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm2ivu2grp`k'=results[6,1] if horizon==`h' // upper bound 
				}
					
				cap ivreghdfe `yv' $firmcontrols (l1dr007=l1`sn') if group==`k',a(fid) gmm2s kernel(bar) robust bw(`kn')
				if _rc==0{
					qui ereturn display,level($ci1 )
					mat define results=r(table)  
					qui replace `yv'`sn'm3iv_bgrp`k'=results[1,1] if horizon==`h'
					qui replace `yv'`sn'm3ivl1grp`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm3ivu1grp`k'=results[6,1] if horizon==`h' // upper bound 
					
					qui ereturn display,level($ci2 )
					mat define results=r(table)  
					qui replace `yv'`sn'm3ivl2grp`k'=results[5,1] if horizon==`h' // lower bound 
					qui replace `yv'`sn'm3ivu2grp`k'=results[6,1] if horizon==`h' // upper bound 
				}
				}
			}
		}
		drop invest invt empl Sales ROE Debt
		}
	}
}
}
}

save TempFiles/lpfirm_quarterly_groupings,replace



*------------------------------- Figures --------------------------------*

xtset fid qdate,q 
cap drop inv1 inv2 empl  Debt Ldebt Sdebt Q Sales ROE  //levr OptLev
forv ll=1/4{
	global lag `ll'
	global firmcontrols ""
	global indcontrols ""
	// currently no city controls?/macro controls? (absorbed by Time FEs)
	qui foreach v in logasset logliability{  // logsales mktval beta 
		global firmcontrols "$firmcontrols L(1/$lags ).`v'"
	}
	qui foreach v in totalsales hhi{  // logsales mktval beta 
		global indcontrols "$indcontrols L(1/$lags ).`v'"
	} 
	
foreach sn in pboc unconv conv LPR MLF LR RRR MPR{ //  

	global shock="pca`sn'" 
	
	forv h=0/$horizon{
		
		gen inv1=f`h'.inv3-l.inv3
		gen inv2=f`h'.logasset-l.logasset
		gen empl=f`h'.logemp-l.logemp
		gen Debt=f`h'.lndebt-l.lndebt
		gen Ldebt=f`h'.lnldebt-l.lnldebt 
		gen Sdebt=f`h'.lnsdebt-l.lnsdebt 
		gen Q=f`h'.tobinq-l.tobinq 
		gen Sales=f`h'.logsale-l.logsale
		gen ROE=f`h'.roe-l.roe
		
		foreach yv in inv1 inv2 empl Debt Ldebt Sdebt Q Sales ROE{
			
			forv grm=1/4{
				foreach mm in _b u1 u2 l1 l2{
				cap gen `yv'`sn'gr`grm'ols`mm'=. 
				cap gen `yv'`sn'gr`grm'iv`mm'=. 
				}
			}
			
			forv k=1/4{  // Size-Age subgroups 
				est clear 
				cap reghdfe `yv' L(1/$lags ).$shock L(1/$lags ).`yv' if group==`k',a(fid) vce(cluster fid)
				if _rc==0{
				qui ereturn display,level(90)
				mat define results=r(table)  
				qui replace `yv'`sn'gr`k'ols_b=results[1,1] if horizon==`h'
				qui replace `yv'`sn'gr`k'olsl1=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'gr`k'olsu1=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level(68)
				mat define results=r(table)  
				qui replace `yv'`sn'gr`k'olsl2=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'gr`k'olsu2=results[6,1] if horizon==`h' // upper bound 
				}
				
				cap ivreghdfe `yv' L(1/$lags ).dr007 L(1/$lags ).`yv' (dr007 = pca`sn'narrowr0071y) if group==`k',a(fid) vce(cluster fid)
				if _rc==0{
				qui ereturn display,level(90)
				mat define results=r(table)  
				qui replace `yv'`sn'gr`k'iv_b=results[1,1] if horizon==`h'
				qui replace `yv'`sn'gr`k'ivl1=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'gr`k'ivu1=results[6,1] if horizon==`h' // upper bound 
				
				qui ereturn display,level(68)
				mat define results=r(table)  
				qui replace `yv'`sn'gr`k'ivl2=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'gr`k'ivu2=results[6,1] if horizon==`h' // upper bound 
				}
			}
		}
		drop inv1 inv2 empl Debt Ldebt Sdebt Q Sales ROE
	}
} 
}

save TempFiles/lp_results_full_Jul12,replace



su leverage , d 
gen highlev =leverage > r(p50)
*replace highlev = 0 if leverage == .  // a half?? then no estimates for the low group!

xtset fid year
cap drop inv1 inv2 empl levr Debt Ldebt Sdebt Q OptLev Sales ROE
foreach sn in pboc{ //   unconv conv LPR MLF LR RRR MPR
/*
global shock="pca`sn'" 
	

	forv h=0/$horizon{
		
	gen inv1=f`h'.inv3-l.inv3
		gen inv2=f`h'.logasset-l.logasset
		gen empl=f`h'.logemp-l.logemp
		gen Debt=f`h'.lndebt-l.lndebt
		gen Ldebt=f`h'.lnldebt-l.lnldebt 
		gen Sdebt=f`h'.lnsdebt-l.lnsdebt 
		gen Q=f`h'.tobinq-l.tobinq 
		gen Sales=f`h'.logsale-l.logsale
		gen ROE=f`h'.roe-l.roe
		
		foreach yv in inv1 inv2 empl Debt Ldebt Sdebt Q Sales ROE{
			
			forv k=0/1{
				cap gen `yv'`sn'lev`k'_b=. 
				cap gen `yv'`sn'lev`k'u1=. 
				cap gen `yv'`sn'lev`k'l1=. 
				cap gen `yv'`sn'lev`k'u2=. 
				cap gen `yv'`sn'lev`k'l2=. 
			}
			
			forv k=0/1{  // Size-Age subgroups 
				est clear 
				** (2) +Firm FEs
				cap eststo `yv'm1:reghdfe `yv' l.$shock if leverage==`k',a(fid) level(68) vce(cluster fid)
				if _rc == 0{
				mat define results=r(table)  
				qui replace `yv'`sn'lev`k'_b=results[1,1] if horizon==`h'
				qui replace `yv'`sn'lev`k'l1=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'lev`k'u1=results[6,1] if horizon==`h' // upper bound 
				
				eststo `yv'm2:reghdfe `yv' l.$shock if leverage==`k',a(fid) level(90) vce(cluster fid)
				mat define results=r(table)  
				qui replace `yv'`sn'lev`k'l2=results[5,1] if horizon==`h' // lower bound 
				qui replace `yv'`sn'lev`k'u2=results[6,1] if horizon==`h' // upper bound 
				}
				}
		}
				 drop inv1 inv2 empl Debt Ldebt Sdebt Q Sales ROE
	}
} 
save TempFiles/lp_results_full_Jul12,replace

 use TempFiles/lp_results_full_Jul12,replace
 
keep if horizon!=. 
keep if horizon<=12
keep horizon *m*_b* *m*u1 *m*l1 *m*u2 *m*l2 *m*soe0 *m*soe1 *gr* *lev* 
gen zero=0

* for various FE models
gen hor=3*horizon 
gen h2=hor+.3
gen h3=hor+.6
gen h4=hor+.9

tostring horizon, replace 

local hand "label define datelabs "
local obs=_N
forv nn =1/`obs'{
    local dn=hor[`nn']
	local cnt=horizon[`nn']
	local hand `"`hand'`dn' "{stSerif:`cnt'}" "' // font=Times New Roman
}
`hand',replace 
lab values hor datelabs

global inv1_name "Investment over Fixed Assets"
global inv2_name "Total Asset Growth"
global empl_name "Employment"
global Debt_name "Total Debt"
global Ldebt_name "Long-term Debt"
global Sdebt_name "Short-term Debt"
global Q_name "Tobin's Q"
global Sales_name "Total Sales"
global ROE_name "ROE"

global pboc_name "Baseline"
global unconv_name "Innovative"
global conv_name "Conventional"
foreach mp in LPR MLF LR RRR MPR{
	global `mp'_name "`mp'"
}

/* first order IRFs */
foreach ii in pboc unconv conv LPR MLF LR RRR MPR{  // >>> No Control is problematic 
	
	foreach yv in inv1 inv2 empl Debt Ldebt Sdebt Q Sales ROE{
		su hor
		local ma=r(max)
		local mi=r(min)
		tw (scatter `yv'`ii'm1_b hor,mc(green)) (scatter `yv'`ii'm2_b h2,mc(maroon)) ///
		   (scatter `yv'`ii'm3_b h3 ,mc(navy )) ///
		   (rcap `yv'`ii'm1u1 `yv'`ii'm1l1 hor,lc(green))  ///
		   (rcap `yv'`ii'm2u1 `yv'`ii'm2l1 h2,lc(maroon))  ///
		   (rcap `yv'`ii'm3u1 `yv'`ii'm3l1 h3,lc(navy )),  ///
		   xti("{stSerif:Years Forward}") name(`ii'`yv',replace) yline(0,lc(red) lp(dash))  ///
		   leg(order(6 "{stSerif:No controls}" 7 "{stSerif:Firm FEs}" 8 ///
		   "{stSerif:+ Controls}") r(1)) ///
		   ti("{stSerif:$`yv'_name Responses to $`sn'_name MPS}",c(black) siz(18pt)) ///
		   xlab(`mi'(3)`ma',valuelabel) graphr(c(white)) plotr(c(white))
		gr export OutPuts\LP\FirmLevel\Full/`yv'`ii'.png,width(2400) replace
	}
}


/*  Size-Age dichotomy */ 
foreach ii in pboc unconv conv LPR MLF LR RRR MPR{
	
	foreach yv in inv1 inv2 empl Debt Ldebt Sdebt Q Sales ROE{
		
		forv k=1/4{
			local name1 "Large & Old"
			local name2 "Large & Young"
			local name3 "Small & Old"
			local name4 "Small & Young"

		su hor
		local ma=r(max)
		local mi=r(min)
		tw (scatter `yv'`ii'gr`k'_b hor,mc(maroon)) ///
		   (rcap `yv'`ii'gr`k'u1 `yv'`ii'gr`k'l1 hor,lc(gs10)),  ///
		   xti("") name(`ii'`yv'`k',replace) yline(0,lc(red) lp(dash))  ///
		   leg(off) ylab(,format(%4.2fc)) ///
		   ti("{stSerif:`name`k''}",c(black) siz(36pt)) ///
		   xlab(`mi'(3)`ma',valuelabel) graphr(c(white)) plotr(c(white))
		}
		
		gr combine `ii'`yv'1 `ii'`yv'2 `ii'`yv'3 `ii'`yv'4,imargin(zero) c(2) ///
		 graphr(c(white)) plotr(c(white)) xsiz(20) ysiz(16) ti( ///
		 "{stSerif:$`yv'_name Responses to MPS $`ii'_name}", c(black) size(40pt))
		gr export OutPut/LP/FirmLevel/SizeAge/`yv'`ii'.png,width(2400) replace
	}
}


/*  High vs Low Leverage --- not working by now */ 
foreach ii in pboc unconv conv LPR MLF LR RRR MPR{
	
	foreach yv in inv1 inv2 empl Debt Ldebt Sdebt Q Sales ROE{
		
		su hor
		local ma=r(max)
		local mi=r(min)
		tw (scatter `yv'`ii'lev0_b hor,mc(maroon)) ///
		   (rcap `yv'`ii'lev0u1 `yv'`ii'lev0l1 hor,lc(maroon)) ///
		   (scatter `yv'`ii'lev1_b hor,mc(navy)) ///
		   (rcap `yv'`ii'lev1u1 `yv'`ii'lev1l1 hor,lc(navy)),  ///
		   xti("") name(`ii'`yv',replace) yline(0,lc(red) lp(dash)) ///
		   ylab(,format(%4.2fc)) leg(order(2 "{stSerif:Low Leverage}" ///
		   4 "{stSerif:High Leverage}")) /// ti("{stSerif:`name`k''}",c(black) siz(18pt)) ///
		   xlab(`mi'(3)`ma',valuelabel) graphr(c(white)) plotr(c(white))
		 gr export OutPut/LP/FirmLevel/HighLowLev/`yv'`ii'.png,width(2400) replace  
	}
}




/*  SOE vs POE */ 
foreach ii in pboc unconv conv LPR MLF LR RRR MPR{
	
	foreach yv in inv1 inv2 empl Debt Ldebt Sdebt Q Sales ROE{
		
		
		su hor
		local ma=r(max)
		local mi=r(min)
		tw (scatter `yv'`ii'm2_bsoe0 hor,mc(maroon)) ///
		   (rcap `yv'`ii'm2u1soe0 `yv'`ii'm2l1soe0 hor,lc(maroon)) ///
		   (scatter `yv'`ii'm2_bsoe1 hor,mc(navy)) ///
		   (rcap `yv'`ii'm2u1soe1 `yv'`ii'm2l1soe1 hor,lc(navy)),  ///
		   xti("") name(`ii'`yv',replace) yline(0,lc(red) lp(dash)) ///
		   ylab(,format(%4.2fc)) leg(order(2 "{stSerif:POE}" ///
		   4 "{stSerif:SOE}")) ti("{stSerif:$`yv'_name}",c(black) siz(18pt)) ///
		   xlab(`mi'(3)`ma',valuelabel) graphr(c(white)) plotr(c(white))
		  
		 gr export OutPut/LP/FirmLevel/POESOE/`ii'`yv'.png,width(2400) replace  
	
	}
}



