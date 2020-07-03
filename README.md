# concordance: Product Concordance
[![R build
status](https://github.com/r-lib/usethis/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/usethis/actions) [![Build Status](https://travis-ci.org/insongkim/concordance.svg?branch=master)](https://travis-ci.org/insongkim/concordance) ![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/concordance)  [![CRAN status](https://www.r-pkg.org/badges/version/concordance)](https://CRAN.R-project.org/package=concordance)

Authors: Steven Liao (steven.liao@ucr.edu), In Song Kim (insong@mit.edu), Sayumi Miyano (smiyano@princeton.edu), Hao Zhang (hzhang3@mit.edu)

This R package provides a set of utilities for matching products in different 
classification codes used in international trade research. It currently supports 
concordance between the classifications below:

- Harmonized System
  * "HS0" (1988/92)
  * "HS1" (1996) 
  * "HS2" (2002) 
  * "HS3" (2007)
  * "HS4" (2012) 
  * "HS5" (2017)
  * "HS" (HS combined)
- Standard International Trade Classification
  * "SITC1" (1950)
  * "SITC2" (1974) 
  * "SITC3" (1985) 
  * "SITC4" (2006)
- North American Industry Classification System
  * "NAICS2002"
  * "NAICS2007" 
  * "NAICS2012" 
  * "NAICS2017"
  * "NAICS" (combined)
- International Standard Industrial Classification
  * "ISIC2" (1968)
  * "ISIC3" (1989)
  * "ISIC3.1" (2002)
  * "ISIC4" (2008)

Support between the above and the below classifications will be offered 
soon:

- Broad Economic Categories (BEC)
- Standard Industrial Classification (SIC)

Additionally, the package provides functions for: 

- Code nomenclature/descriptions look-up (for HS, SITC, NAICS, ISIC, BEC classification codes)
- Product code look-up based on user-specified keywords
- Rauch classification (product differentiation) look-up (via concordance to SITC2)
- Trade elasticity look-up (via concordance to HS0 or SITC3 codes)
- Industry upstreamness/downstreamness look-up (via concordance to ISIC3 codes)
- Industry intermediateness look-up (via product descriptions)


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

Citation
-------------------------

To cite `concordance` in publications use:

```
  Steven Liao, In Song Kim, Sayumi Miyano, Feng Zhu (2020). concordance: Product Concordance. 
  R package version 2.0.0. https://CRAN.R-project.org/package=concordance
```
  
A BibTeX entry for LaTeX users is:

```
  @Manual{,
    title = {concordance: Product Concordance},
    author = {Steven Liao and In Song Kim and Sayumi Miyano and Feng Zhu},
    year = {2020},
    note = {R package version 2.0.0},
    url = {https://CRAN.R-project.org/package=concordance},
  }
```

Usage Examples
-------------------------

### Getting Product Description
The `get_desc` function allows users to look up the product description of 
different classification codes. The example below focuses on HS codes.

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
get_desc(sourcevar = c("1206", "8546"), origin = "HS5")
```
```
[1] "Sunflower seeds; whether or not broken" "Electrical insulators of any material"
```

```r
get_desc(sourcevar = c("12", "85"), origin = "HS5")
```
```
[1] "Oil seeds and oleaginous fruits; miscellaneous grains, seeds and fruit, industrial or medicinal plants; straw and fodder"     
[2] "Electrical machinery and equipment and parts thereof; sound recorders and reproducers; television image and sound recorders and reproducers, parts and accessories of such articles"
```

### Getting Product Codes By Keywords
The `get_product` function allows users to look up product codes for which 
descriptions match user-specified keywords. 

The function utilizes the function stringr::str_detect for pattern detection. 
The argument ``pattern`` takes specific string patterns to search for,
``origin`` indicates the classification system of focus, ``digits`` sets the 
number of digits of the output codes, ``type`` sets the type of pattern 
interpretation (e.g., "regex", "fixed", "coll", see ``?str_detect`` for further 
details), and ``ignore.case`` decides whether to ignore case differences (TRUE 
by default). The example below returns manufacture-related NAICS codes.

```r
manu.vec <- get_product(pattern = "manu", origin = "NAICS2017", digits = 4,
                        type = "regex", ignore.case = TRUE)
manu.vec
```
```
[1] "3111" "3113" "3114" "3115" "3118" "3119" "3121" "3122" "3152" "3159" "3162" "3169" "3212" "3219" "3222" "3241" "3251" "3252" "3253" "3254" "3255" "3256" "3259"
[24] "3261" "3262" "3271" "3272" "3273" "3274" "3279" "3311" "3312" "3322" "3323" "3324" "3325" "3326" "3327" "3329" "3331" "3332" "3333" "3334" "3335" "3336" "3339"
[47] "3341" "3342" "3343" "3344" "3345" "3346" "3351" "3352" "3353" "3359" "3361" "3362" "3363" "3364" "3365" "3369" "3371" "3372" "3379" "3391" "3399"
```

Users can double-check the product descriptions with ``get_desc``.

```r
get_desc(manu.vec, origin = "NAICS2017")
```
```
[1] "Animal Food Manufacturing"                                                                   
[2] "Sugar and Confectionery Product Manufacturing"                                               
[3] "Fruit and Vegetable Preserving and Specialty Food Manufacturing"                             
[4] "Dairy Product Manufacturing"                                                                 
[5] "Bakeries and Tortilla Manufacturing"                                                         
[6] "Other Food Manufacturing"                                                                    
[7] "Beverage Manufacturing"                                                                      
[8] "Tobacco Manufacturing"                                                                       
[9] "Cut and Sew Apparel Manufacturing"                                                           
[10] "Apparel Accessories and Other Apparel Manufacturing"
...
```

### Concording Different Classification Codes
The `concord` function allows users to concord between different classification 
codes. The example below converts HS5 to NAICS2017 codes. 

Users can choose to retain all matches for each input by setting `all = TRUE`. 
This option will also return the share of occurrences for each matched output 
among all matched outputs at the user-specified digit level.

```r
# HS to NAICS
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2017",
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
mode consists of multiple matches, the function will return the first matched output.

```r
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2017",
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

More technically, the function works by matching an input code to the most fine-grained 
level of destination codes in our package (e.g., the 6-digit NAICS codes 
above) and then calculates the occurrence share of each matched code at the 
user-specified digit-level. Mode(s) can occur when users choose destination 
codes at a more aggregated level and multiple finer-grained matched codes 
belong to certain groups at that level. 

We illustrate the above mechanics using HS5 code "8546" as an example. When 
users ask for 6-digit NAICS codes (the most fine-grained level available), HS5 
code "8546" is matched to five NAICS codes: "327212", "327113", "327110", "326199", 
and "335932", with weights of 0.2 (1/5) each.

```r
concord(sourcevar = "8546",
        origin = "HS5", destination = "NAICS",
        dest.digit = 6, all = TRUE)
```
```
$`8546`
$`8546`$match
[1] "327212" "327113" "327110" "326199" "335932"

$`8546`$weight
[1] 0.2 0.2 0.2 0.2 0.2
```

Instead, when users ask for 4-digit NAICS codes, HS5 code "8546" is matched 
to four NAICS codes: "3271", "3272", "3261", "3359". NAICS code "3271" gets a 
weight of 0.4 since it consists of two finer-grained matches "327113" and 
"327110" out of the 5 total matches (2/5).

```r
concord(sourcevar = "8546",
        origin = "HS5", destination = "NAICS",
        dest.digit = 4, all = TRUE)
```
```
$`8546`
$`8546`$match
[1] "3271" "3272" "3261" "3359"

$`8546`$weight
[1] 0.4 0.2 0.2 0.2
```

Thus, when `all = FALSE`, the function will retain the matched code with the largest 
weight "3271".

```r
concord(sourcevar = "8546",
        origin = "HS5", destination = "NAICS",
        dest.digit = 4, all = FALSE)
```
```
[1] "3271"
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
# get the proportion of type "r"
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

### Getting Industry Upstreamness/Downstreamness
Antras and Chor (2018) estimate industry-level upstreamness/downstreamness for 
2-digit ISIC3 codes in 40 countries (+ Rest of 
the World, RoW) between 1995 and 2011. 

The `get_upstream` function concords users' input codes to 2-digit ISIC3 codes 
and then extracts the corresponding industry-level upstreamness/downstreamness 
in the country and year selected by the user. 

The argument ``sourcevar`` sets the industry codes to look up, ``origin`` 
indicates the classification system of the input codes, ``country`` 
takes ISO 3-letter codes, ``year`` takes an integer between 1995 and 2011, 
and ``setting`` accepts one of the four available measures as defined in 
Antras and Chor (2018):

- `"GVC_Ui"`: Upstreamness (net inventories correction). This is the defult measure. Larger values are associated with higher levels of upstreamness.
- `"GVC_FUGOi"`: Final-use to gross-output (net inventories correction). Lower values are associated with higher levels of upstreamness.
- `"GVC_Di"`: Downstreamness (net inventories correction). Larger values are associated with higher levels of downstreamness.
- `"GVC_VAGOi"`: Value-added to gross-output (net inventories correction). Lower values are associated with higher levels of downstreamnes

The example below returns the upstreamness ("GVC_Ui") of HS5 industries in 
the United States in 2011.

```r
get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
             country = "USA", year = "2011",
             setting = "GVC_Ui")
```
```
[1] 2.595109 2.595109 2.563818 1.795285
```

### Getting Industry Intermediateness
The `get_intermediate` function calculates and returns the proportion
of intermediate goods production in an industry based on product descriptions. 

The function uses keywords ("part(s)", "intermediate", and "component") to 
identify intermediate-goods producing industries (at the most disaggregated 
level in the description data), and then calculates and returns the proportion 
these industries occupy among each input code. Larger values indicate higher 
levels of intermediateness in an industry.

For example, users can get the level/proportion of intermediate goods 
production in the 4-digit NAICS2017 industries below.
```r
get_intermediate(sourcevar = c("3131", "3363"), origin = "NAICS2017")
```
```
[1] 0.0 0.5
```

Or the level/proportion of intermediate goods 
production in the 2-digit HS5 industries below.
```r
get_intermediate(sourcevar = c("03", "84"), origin = "HS5")
```
```
[1] 0.0000000 0.1937984
```

References
-------------------------

- Antras, Pol, and Davin Chor. 2018. "On the Measurement of Upstreamness and Downstreamness in Global Value Chains." World Trade Evolution: Growth, Productivity and Employment, 126-194. Taylor & Francis Group.
- Broda, Christian, and David E. Weinstein. 2006. "Globalization and the Gains from Variety," Quarterly Journal of Economics, 121(2): 541--585.
- Rauch, James E. 1999. "Networks Versus Markets in International Trade," Journal of International Economics 48(1): 7--35.

