# concordance: Product Concordance
[![Build Status](https://travis-ci.org/insongkim/concordance.svg?branch=master)](https://travis-ci.org/insongkim/concordance)

Authors: Steven Liao (steven.liao@ucr.edu), In Song Kim (insong@mit.edu), Sayumi Miyano (smiyano@princeton.edu), Feng Zhu (zucxjo@gmail.com)

This R package provides a set of utilities for matching products in different 
classification codes used in international trade research. It currently supports 
concordance between the classifications below:

- "HS" (HS combined)
- "HS0" (1988/92)
- "HS1" (1996) 
- "HS2" (2002) 
- "HS3" (2007)
- "HS4" (2012) 
- "HS5" (2017)
- "SITC1" (1950)
- "SITC2" (1974) 
- "SITC3" (1985) 
- "SITC4" (2006)
- "NAICS" (combined). 

Support between the above and BEC, ISIC, and SIC classifications will be offered 
soon. 

Additionally, the package provides functions for: 

- Code nomenclature / descriptions look-up
- Rauch classification (product differentiation) look-up (via concordance to SITC2)
- Trade elasticity look-up (via concordance to HS0 or SITC3 codes).


Installation Instructions
-------------------------

`concordance` is available on CRAN and can be installed using:

``` r
install.packages("concordance")
```

You can install the most recent development version of `concordance` using the `devtools` package. First you have to install `devtools` using the following code. Note that you only have to do this once:

``` r
if(!require(devtools)) install.packages("devtools")
```

Then, load `devtools` and use the function `install_github()` to install `concordance`:

``` r
library(devtools)
install_github("insongkim/concordance", dependencies=TRUE)
```

Usage Examples
-------------------------

### Getting Product Description
Users can look up the product description of different classification 
codes using the `get_desc` function. The example below focuses on HS codes.

```r
# load package
library(concordance)

# get product description
get_desc(sourcevar = c("120600", "854690"), origin = "HS5")
```
```
[1] "Oil seeds; sunflower seeds, whether or not broken" "Electrical insulators; other than of glass and ceramics"
```

Users can also input codes with different digits. For HS codes, 2, 4, 6-digits 
are supported. Note that users should always include leading zeroes in 
the codes (e.g. use HS code 010110 instead of 10110) -- results may be buggy otherwise.

```r
# get product description
get_desc(sourcevar = c("1206", "8546"), origin = "HS5")
```
```
[1] "Sunflower seeds; whether or not broken" "Electrical insulators of any material"
```

```r
# get product description
get_desc(sourcevar = c("12", "85"), origin = "HS5")
```
```
[1] "Oil seeds and oleaginous fruits; miscellaneous grains, seeds and fruit, industrial or medicinal plants; straw and fodder"     
[2] "Electrical machinery and equipment and parts thereof; sound recorders and reproducers; television image and sound recorders and reproducers, parts and accessories of such articles"
```

### Concording Different Classification Codes

Users can concord between different classification codes using the `concord` 
function. The example below converts HS5 to NAICS codes. 

Users can choose to retain all matches for each input by setting `all = TRUE`. 
This option will also return the share of occurrences for each matched output 
among all matched outputs.

```r
# HS to NAICS
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS",
        dest.digit = 6, all = TRUE)
```
```
$`120600`
$`120600`$match
[1] "111120"

$`120600`$weight
[1] 1


$`854690`
$`854690`$match
[1] "326199" "335932"

$`854690`$weight
[1] 0.5 0.5
```

Alternatively, users can simply obtain the matched output with the largest 
share of occurrences (the mode match) with `all = FALSE` (default). If the 
mode consists of multiple matches, the function will return the first matched 
output.

```r
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS",
        dest.digit = 6, all = FALSE)
```
```
[1] "111120" "326199"
```

Users can double-check the validity of the matches with `get_desc`.

```r
# get product description of NAICS ouput
get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2017")
```
```
[1] "Oilseed (except Soybean) Farming" "All Other Plastics Product Manufacturing"
```

### Getting Product Differentiation

Rauch (1999) classifies each SITC Rev. 2 industry according to three possible types: 

- Differentiated products ("n")
- Reference priced ("r")
- Homogeneous goods traded on an organized exchange ("w")

The `get_proddiff` function concords users' input codes to SITC2 codes and then 
extracts the corresponding Rauch classifications.

There are two main options. First, users can set `prop = "n"`, `prop = "r"`, 
or `prop = "w"`, in which case the function will return the proportion of "w", 
"r", or "n" in the resulting vector of Rauch indices.

```r
# get the proportion of "r" products
get_proddiff(sourcevar = c("120600", "854690"), origin = "HS5", prop = "r")
```
```
120600 854690 
     1      0
```

If prop is not set to any of these, then the function returns, for each input 
code, a dataframe that summarizes all the frequencies and proportions of 
"w", "r", and "n". 

```r
get_proddiff(sourcevar = c("120600", "854690"), origin = "HS5", prop = "")
```
```
$`120600`
  rauch freq proportion
1     w    0          0
2     r    1          1
3     n    0          0

$`854690`
  rauch freq proportion
1     w    0          0
2     r    0          0
3     n    1          1
```

Second, users can choose Rauch's conservative classification with 
`setting = CON` (default). `setting = LIB` returns Rauch's liberal 
classification. 

```r
get_proddiff(sourcevar = c("120600", "854690"), origin = "HS5", setting = "LIB", prop = "")
```
```
$`120600`
  rauch freq proportion
1     w    1          1
2     r    0          0
3     n    0          0

$`854690`
  rauch freq proportion
1     w    0          0
2     r    0          0
3     n    1          1
```

### Getting Product Elasticity
Broda and Weinstein (2006) estimate product-level import demand elasticities 
for 73 countries using HS0 3-digit codes. 

The `get_sigma` function concords users' input codes to 3-digit HS0 codes and 
then extracts the corresponding product-level elasticities in the country 
selected by the user. 

There are two main options. First, when `give_avg = TRUE` (default), each 
output element will be a simple average of all elasticities (of matched codes) 
in the corresponding vector.

```r
get_sigma(sourcevar = c("120600", "854690"), origin = "HS5",
          country = "USA", give_avg = TRUE)
```
```
[1] 3.733456 1.233216
```

Users can also set `give_avg = FALSE` to obtain the full vector of elasticities 
for all matching codes of each element in the input vector. In this case, there 
were only one matches per input.

```r
get_sigma(sourcevar = c("120600", "854690"), origin = "HS5",
          country = "USA", give_avg = FALSE)
```
```
$`120600`
$`120600`$elasticity
[1] 3.733456


$`854690`
$`854690`$elasticity
[1] 1.233216
```

Second, for the United States (only), Broda and Weinstein (2006) have also 
estimated elasticities based on more fine-grained 5-digit SITC3 codes. Users 
can obtain elasticities in the United States via this method 
with `use_SITC = TRUE`.

```r
get_sigma(sourcevar = c("120600", "854690"), origin = "HS5",
          country = "USA", use_SITC = TRUE, give_avg = TRUE)
```
```
[1] 2.562991 1.345522
```

References
-------------------------

- Broda, Christian, and David E. Weinstein. 2006. "Globalization and the Gains from Variety," Quarterly Journal of Economics, 121(2): 541--585.
- Rauch, James E. 1999. "Networks Versus Markets in International Trade," Journal of International Economics 48(1): 7--35.