


* Do-file to compute upstreamness by industry
* input file: IOUseDetail.dta

*----------------------------------
* Prepare USE table      
*----------------------------------

set matsize 11000
use IOUseDetail2007.dta, clear
sort Commodity Industry
rename Commodity io_input
rename Industry io_output
rename CommodityDes io_input_name
rename IndustryDes io_output_name

* These are commodities that are inputs to other industries, but never outputs
drop if io_input=="S00401" | io_input=="S00402" | io_input=="S00300" | io_input=="S00900"
* These four commodities are:
* S00300 Noncomparable imports
* S00401 Scrap
* S00402 Used and Secondhand goods
* S00900 Rest of the World Adjustment

save IOUsetemp, replace



*----------------------------------
* Generate lists of industries      
*----------------------------------

use IOUsetemp, clear
keep io_input*
duplicates drop
rename io_input io_industry
rename io_input_name io_industry_name
save io_newind_list, replace

use IOUsetemp, clear
keep io_output*
duplicates drop
rename io_output io_industry
rename io_output_name io_industry_name

merge 1:1 io_industry using io_newind_list
* two industries (S00201 and S00202) are not listed as inputs --> To be added to get a square matrix!
drop _merge

drop if substr(io_industry,1,1) == "F" | substr(io_industry,1,1) == "V"
* drop io_industry_name
sort io_industry
gen newind = _n
sort newind
save io_newind_list, replace



*----------------------------------
* Create a square (use) matrix      
*----------------------------------

use io_newind_list, clear
rename newind newoutput
rename io_industry io_output
rename io_industry_name io_output_name
cross using io_newind_list 
rename newind newinput
rename io_industry io_input
rename io_industry_name io_input_name

merge 1:1 io_input io_output using IOUsetemp
drop if substr(io_output,1,1) == "F"
drop if substr(io_input,1,1) == "V"
tab _merge
replace ProVal = 0 if _merge == 1

drop _merge
save IOUseFullTemp, replace



*----------------------------------
* Extracting Absorption 
*----------------------------------

use IOUsetemp, clear

drop if substr(io_input,1,1)=="V"
* drop exports and imports:
drop if io_output =="F04000" | io_output =="F05000"
* drop changes in inventories:
drop if io_output =="F03000"

collapse (sum) absorption = ProVal, by(io_input*)

drop if absorption == 0

rename io_input io_industry
rename io_input_name io_industry_name
sort io_industry
save Absorption, replace



*----------------------------------
* Compute Delta matrix      
*----------------------------------

use IOUseFullTemp, clear

* get absorption:
rename io_input io_industry
merge m:1 io_industry using Absorption
drop io_industry_name _merge
rename io_industry io_input

gen delta = ProVal / absorption
replace delta = 0 if delta == .

keep  io_input io_input_name newinput io_output io_output_name newoutput delta 
order io_input io_input_name newinput io_output io_output_name newoutput delta 
sort io_input io_output
save Delta_matrix, replace




*-------------------------------------------
* Calculation of Upstreamness
*-------------------------------------------


use Delta_matrix, clear

gen invcoeff = delta

replace invcoeff = -invcoeff
replace invcoeff = 1+invcoeff if newinput == newoutput
keep newinput newoutput invcoeff
sort newinput newoutput

reshape wide invcoeff, i(newinput) j(newoutput) 

mkmat invcoeff*, matrix(IObis) 
drop invcoeff*

matrix InvIObis = inv(IObis)

svmat InvIObis, names(matcol)

count if newinput != _n

reshape long InvIObisr, i(newinput) j(newoutput) 
rename InvIObisr invindex

collapse (sum) invindex, by(newinput)

rename invindex upstreamness
sort newinput
save upstreamness_by_industry, replace

order newinput upstreamness
sort newinput


*-------------------------------------------
* Merging with industry names 
*-------------------------------------------


rename newinput newind
merge 1:1 newind using io_newind_list
drop _merge newind

order io*
sort io_industry
drop io_industry_name
save upstreamness_by_industry2007, replace




