# concordance 2.0.0

## Major improvements

* `concord()` is now a wrapper function for various concordance 
sub-functions: `concord_hs_naics`, `concord_hs_sitc`, `concord_sitc_naics`, 
`concord_hs`, and `concord_sitc`. The compartmentalizing of the original 
function allows the package to incorporate new product classification systems 
and versions more easily. Additionally, users can now choose (1) whether they 
want to return all matched outputs for each input, and (2) the preferred number 
of digits for outputs.

* All functions in the package are now vectorized with significant speed 
improvements.

## Minor improvements

* `get_sigma` subsumes `getSigma` and `listSigma`. It now allows users to (1) 
choose the country for which to return import demand elasticities, and (2) 
use 5-digit SITC3 estimates for the United States (instead of the 3-digit HS0 
default).

* For replication purposes, raw concordance data, along with R scripts that 
clean them, are now stored under 
<https://github.com/insongkim/concordance/tree/master/data-raw>.

## Deleted Functions and Data

### Functions
* `extend_concordance` has been removed as it is no longer needed in 
the main `concord` function.

* `concord_test` has been removed. Instead, see the various test scripts 
under <https://github.com/insongkim/concordance/tree/master/data-raw>, e.g., 
`test-concord.R`.

* `getRauch` and `proddiff` have been subsumed by `get_proddiff`.

* `getSigma` and `listSigma` have been subsumed by `get_sigma`.

* `desc` has been renamed as `get_desc` to improve the consistency 
in function names in the package.

### Data

* Description data `codedesc`, `code_lengths`, and `desclen` have been 
removed. Instead, a separate description table for each classification and 
version is now visible in the package and can be loaded, e.g., 
`data(sitc4_desc)`, `data(hs5_desc)`, etc.

* Concordance data `concord_data`, `concord_long`, `hs2sitc`, and `long_codes` 
have been removed. Instead, a separate and cleaned concordance table for each 
pair of classification systems is now visible in the package and can be loaded, 
e.g., `data(hs5_sitc4)`, `data(hs5_naics)`, etc.

* Elasticity data `BWsigma`, `sigma`, and `sigmatab` have been removed. 
Instead, see `data(sigma_hs0)` and `data(sigma_sitc3)`.

* Product differentiation data `Rauch` has been removed. Instead, see 
`data(sitc2_rauch)`.