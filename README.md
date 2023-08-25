# Chinese Monetary Policy Shock Identification and Local Projection - Instrumental Variable Estimation


## Section 1. Shock Construction


### 1.1 Monetary Events

Scrape the exact timing (minute level) of monetary events from the [official website](http://www.pbc.gov.cn/zhengcehuobisi/125207/125213/125431/125475/index.html) of People's Bank of China (PBOC). 

We in particular emphasize the distinction between conventional monetary tools and "innovative" ones like MLF and SLF in China. The final sample includes 181 days, among which 116 are innovative days, and 67 are conventional days, with 2 days overlap. Label them as the "PBOC days".

Code: See Python script in **ScrapeMPTiming.py**. 

> raw data not available right now
> run ``pip install -r requirements.txt`` before running


### 1.2 Underlying Assumption and High Frequency Financial Prices

Follow the monetary policy shock identification literature to adopt the event study approach in shock identification.

In particular, include interest rate swap rate for 7-day repo at one year tenor (IRSR007 1Y), for 3-Month Shibor at 6M/9M/1Y tenor (IRS 3M-SHIBOR 6M/9M/1Y) as underlying financial market prices.

The identification assumption is that innovations in financial market prices within a narrow window surrounding monetary policy events only reflect information related to monetary policy surprises. 

The monetary policy shock ($MPS$) is then constructed as the first principle component of high frequency changes of the above mentioned four prices, in a 60-min window around each event. [1]

Considering data insufficiency issue for intraday transaction data, this baseline method would only allow for more recently developed tools. We thus construct a supplementary version using daily window. This measure also includes daily changes in R007 in the underlying surprises (hence is constructed as a PC1 of 5 series).

Data is collected from WIND API. See pre-processing in **PreProcessRate.do**.


### 1.3 Dimensionality Reduction

Imputed PCA (R package `missMDA') is used. Code: **imputePCA.R**.
> The potential bias led by the imputation is further discussed in the paper appendix).




## Section 2. Macro-Level Analysis -- Yield Curve Responses

To see the first-order impact of the constructed monetary policy shocks on macro borrowing costs, we first estimate the responses of treasury yields using both OLS and the heteroskedasticity-based method of Rigobon and Sack (2004).

> OLS specification: $\Delta Y_{m,t} = \alpha_m +\beta_m MPS_t +\varepsilon_{m,t}$. So the dependent variable is _changes_ in treasury yields at various maturities. The observations are those event days.
> Rigobon method: PBOC days as treatment, all non-PBOC days as controls. Weak-IV robust confidence interval is contructed using bootstrapping. Code provided by Nakamura and Steisson (2018)

The Rigobon method serves are an additional test on background noise. We unsurprisingly document that shocks of the high frequency window include much less noise than a daily window, as in Nakamura and Steisson (2018).

Code: **YieldCurve.do** and **Rigobon.m**.



## Section 3. Micro-Level Analysis -- Bond Spread Responses

We use the most extensive dataset of bond transaction in Chinese capital market, ranging from 2006-2022. (the period is restricted by availability of IRS data). We include various ratings and bond types including enterprise bonds, corporate bonds, financial bonds, MTN, SCP and government bonds.

We firstly regress daily changes in bond yields on the economy-level $MPS$ and document a significantly positive responses. As a normalization, we construct bond spreads as the differences between bond yields and the yield of a representative bond with matched maturity. We define a representative bond as the median of AAA bonds. All regressions are run on PBOC days only. (otherwise the sample would be too large to be possibly estimated!)

For robustness, two econometric specifications are used. (1) OLS; (2) 2SLS with the regressor = daily changes in R007, IV = the contructed MPS shock (could be high-frequency or daily version). The response is highly significant and positive with reasonable magnitudes across various subsamples and different specifications.


Code: **BondReg.do**



## Section 4. Firm-Level Responses

### 4.1 Direct Impacts

We then evaluate the impact of monetary policy shocks on firm activities. A local-projection setting is used. To be consistent with the bond-level analysis, we run both the LP-OLS and LP-IV with the same regressors and IVs. The sample is quarterly. For balance sheet items only available at annual frequency, extended to quarterly using linear interpolation.

Details on the LP specification:

> endogenous variables: _InvestmentRatio_ (=total asset changes over fixed assets lagged 1 period), _AssetGrowth_ (=d.ln(Total Assets)), _DebtGrowth_ (=d.ln(interest bearing borrowings), _SalesGrowth_ (= d.ln(Total Sales)), _ROE_ (=ROE)
> lag-augmented structure: one period lag of all endogenous variables as suggested by AIC/BIC (Montiel Olea and Plagborg‐Møller, 2021)
> fixed-effect model is not used. Instead, apply half-panel jackknife method to adjust for the implicit Nickell bias (Mei, Sheng and Shi, 2023)

Code: **LPFirm.R**. The `pLP.R' project is imported from [Github](https://github.com/zhentaoshi/panel-local-projection.git).


### 4.2 Borrowing Constraint Heterogeneity

We then interact the MPS with a proxy for firm financial constraint to evaluate the role of borrowing constraints on monetary policy transmission to firms. 

The proxy include: (i) Rajan-Zingales (1998)'s EFD measure (ii) SA index of Hadlock and Pierce (2010) (iii) net worth and dividend dummy in Cloyne et al.(2023) (iv) leverage ratio as in Bahaj et al. (2022)

> a separate project looks at the role of excess bond premium: [Chinese EBP](https://github.com/QuinnHLIU/China-Excess-Bond-Premium).

Subsample exercise by SA / SOE-POE groupings is also conducted.




Footnotes: 
[1]. 30-min window is not used due to data sufficiency. For events out of the daily trading window, cross-day window might be included (e.g. a shock on Friday night means the last 15-min on Friday + the first 45-min on next Monday).

%**References**:



