US State Organic Ag
===================

-   Trends of USA Organic Agriculture trends on farm numbers, hectares,
    and sales over the years.
-   panel data analysis?
-   goal: predict farm number, area, and sales for next three census
    (2022, 2027, 2032)

Repository Content
------------------

### Raw Data

**Preprocessing:** Data were copied from TXT and PDF files provided by
the USDA Organic Production Surveys:
<a href="https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Organic_Production/index.php" class="uri">https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Organic_Production/index.php</a>.
Units were converted from acres to hectares.

**Variables:**

-   `state`

-   `year`

-   `farm_number`: number of farms

-   `farm_ha`: area of farms in hectares (ha) converted from reported
    areas using a conversion factor of 2.47 ha/a

-   `farm_sales`: total sales (USD); adjusted to 2020 dollars by
    dividing sales for each year by the adjustment factor provided by
    the [U.S. Bureau of Labor Statistics CPI Inflation
    Calculator](https://www.bls.gov/data/inflation_calculator.htm)
    comparing from July in year Y to July in 2020.

    | Year | **CPI**<sub>*Y*/2020</sub> |
    |------|----------------------------|
    | 2008 | 1.18                       |
    | 2014 | 1.09                       |
    | 2016 | 1.08                       |
    | 2019 | 1.01                       |

-   `[variable]_cv`<sup>\*</sup>: coefficient of variance for
    `[variable]`

<sup>\*</sup>defined as “statistical precision estimates for the number
of farms and acres and the total value of sales for the United States
and for each state.” collected for 2016, 2019; also available for
[2014](https://www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Organics/ORGANICS.txt),
[2015](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/pr76f6075/hx11xj08s/OrganicProduction-09-15-2016.txt)

**Sources of data:**

| census year **<sup>⋆</sup> | survey year **<sup>⋆</sup> | publication year | farms | area | sales | source      | raw data                                                                                                                                                                           | file                                      | used                                   | QA$^\\S$ |
|----------------------------|----------------------------|------------------|-------|------|-------|-------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------|----------------------------------------|----------|
| 1997                       |                            | 2001             | X     |      |       | Greene 2001 | [PDF](raw_data/Greene_2001.pdf)                                                                                                                                                    | 1997\_area.csv                            | No; not comparable (pre certification) |          |
| 2007                       | **2008**                   | 2010             | X     | X    | X     | USDA 2010   | Table 1**<sup>†</sup> [TXT](https://www.nass.usda.gov/Publications/AgCensus/2007/Online_Highlights/Organics/ORGANICS.txt)                                                          | 2008\_2014\_2017\_number\_area\_sales.csv | Yes                                    | Yes      |
| NA                         | 2011                       | 2012             | X     | X    | X     | USDA 2012   | Table 1 [TXT](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/8623j1717/4b29b896g/OrganicProduction-10-04-2012.txt)                                          | 2011\_number\_area\_sales.csv             | No**<sup>‡</sup>                       | Yes      |
| 2012                       | **2014**                   | 2016             | X     | X    |       | USDA 2016   | Table 1**<sup>†</sup> [TXT](https://www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Organics/ORGANICS.txt)                                                           | 2008\_2014\_2017\_number\_area\_sales.csv | Yes                                    | Yes      |
| NA                         | 2015                       | 2016             | X     | X    |       | USDA 2016   | [PDF](raw_data/OrganicProduction-09-15-2016.pdf) [TXT](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/pr76f6075/hx11xj08s/OrganicProduction-09-15-2016.txt) |                                           | No**<sup>‡</sup>                       |          |
| NA                         | **2016**                   | 2017             | X     | X    | X     | USDA 2017a  | Table 1**<sup>†</sup> [TXT](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/70795b52w/6q182n781/OrganicProduction-09-20-2017_correction.txt)                 | 2016\_number\_area\_sales.csv             | Yes, used in fig below**<sup>‡</sup>   | Yes      |
| 2017                       | **2019**                   | 2020             | X     | X    | X     | USDA 2020   | Table 1**<sup>†</sup> [TXT](https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/Organics/ORGANICS.txt)                                                           | 2008\_2014\_2017\_number\_area\_sales.csv | Yes                                    | Yes      |

-   **<sup>⋆</sup> “census year” is the year of the USDA Census; “survey
    year” is the year data was collected (different because the organic
    census is a follow on to the overall census.
-   **<sup>⋆</sup> only surveys conducted in 2008, 2012, 2016, and 2019
    are used in the present analysis
-   **<sup>‡</sup> “Data users should allow for differences when
    comparing data between the 2011 COPS and the 2008 Organic
    Production” USDA 2010 Greene, Catherine R. (2001) US organic farming
    emerges in the 1990s: adoption of certified systems.
    No. 1474-2016-120887. 2001.
-   **<sup>†</sup>number of farms from first line of table - previously
    the second line was used for 2008 and 2012 and first in 2016 and
    2019
-   $^\\S$ csv files cross referenced w/ original data source at state
    level and for US totals

**Note:**

> “The 2014 and 2015 Organic Certifier Survey data is a tabulation of
> USDA-accredited organic certifiers’ acreage and livestock data
> received. The data underwent editing, summarization, and disclosure
> programming prior to publishing. The 2008, 2011, 2014, and 2015
> organic producer surveys as well as the 2007 and 2012 Census of
> Agriculture collected and published data on organic operations which
> had production in the reference year. The data from these programs
> were adjusted for non-response, misclassification, and coverage.
> Therefore, the certifier survey data and producer survey/census data
> are not comparable and data users should account for these differences
> whenreviewing the data. Data users should allow for differences when
> comparing the data between datasets including reference periods,
> organic definitions, and differing survey methodologies.” - USDA 2016

### Analysis

#### Code: in `code/` folder.

-   `01-merge_data.R` combines files from `raw_data` folder to generate
    `all_transformed.R`
-   `02-exploratory_analysis.R` some plots and a simple linear
    regression
-   `03-regression.R` initial modeling using `brms` for multivariate
    Generalized Linear Model with missing data

#### Bayesian Multivariate Regression Model

The cleaned and derived data of farm number, area, and sales are modeled
as a multivariate lognormal likelihood where the expected values are a
linear regression on time (year). The multivariate formulation allows for
estimates of covariance among the response variables. For each variable, state-specific slopes 
were modeled hierarchically as belonging to an national-level slope, while state-level
intercepts were modeled as random effects. Intercept random effects were modeled
as a normal distribution centered at zero and a precision drawn from a
folded-t distribution. Post-sweeping was employed to maintain
identifiability between random effects and the intercepts and impose a
sum-to-zero constraint on the random effects. The precision matrix was
drawn from a Wishart distribution, from which standard deviations and
correlation coefficients among the response variables were monitored.

The model was run in OpenBUGS using the `R2OpenBUGS` package. Three
chains of 3000 samples were monitored at a thinning interval of 20 to
reduce auto-correlation and storage requirements. Convergence was
assessed visually and confirmed with the Gelman-Rubin diagnostic. Posterior chains were summarized as mean
and central 95% credible interval (CI). State-level slopes indicate the annual percent rate of change (in farm number, area, and sales, respectively) and differed significantly
from the national slope when its 95% CI does not contain the posterior mean of the national
slope, and vice versa. 

#### Found in `/code/BUGS/mod3` folder.

-   `mod_3a.R` contains a multivariate normal Bayesian model code
-   `01_run_model.R` modifies `all_transformed.csv`, models data with
    `mod_3a.R`, and produces a coda object
-   `02_check_convergence.R` visually and algorithmically inspects for
    convergence; produces starting values
-   `03_plot_parameters.R` plots the slope and intercept, random
    effects, variance of the slope and RE terms, and variance and covariance terms
-   `04_assess_fit.R` runs model for replicated data and assesses model
    fit
-   `fig_3a/` contains generated plots
-   `params_3a.csv` contains summarized model parameters
-   `predicted_3a.csv` contains input data in long format (with missing
    values) and predicted values with the central 95% credible interval
    (complete)
-   `inits/` contains initial values to re-run the model
-   `coda/` contains raw mcmc chains for the parameters `coda_out_3a.Rdata` and for the replicated data `coda_rep_3a.Rdata`

#### Run the model

``` r
source('01_run_model.R')
```

### Derived Data

### TODO

-   move mod3 model output to `derived_data/ folder`
-   document unused models code/03-regression.R, code/BUGS/mod1
-   This figure from the “[Organic Farming: Results from 2019 Organic
    Survey](https://www.nass.usda.gov/Publications/Highlights/2020/census-organics.pdf)”
    and tables in the [“2019 Organic Survey Data Release”
    presentation](https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Organic_Production/pdf/2019_Organic_Executive_Briefing.pdf)
    implies that USDA has reliable, consistent data for 2008, 2014,
    2016, 2019
    -   However, the available data for, e.g., 2008 (first cell of Table
        1 from [USDA
        2010](https://www.nass.usda.gov/Publications/AgCensus/2007/Online_Highlights/Organics/ORGANICS.txt))
    -   total organic farms in US is 14,540; much higher than in the
        figure …
    -   Have reviewed, and will assume figure is in error. Email sent to
        NASS on 2021-05-12
-   ![](figures/usda_census_highlights_fig1.png)

**Update from USDA**

I think the difference in numbers you are citing is whether the estimate
is for all organic farms (including exempt from certification farms) or
just certified organic farms.

The best place to retrieve the organic data is from our Quickstats
database.

This query should get you there, but some of it is difficult to view
over time because of the change in the scope of the survey. For example,
the 2008 and 2014 surveys included all organic farms (certified and
exempt), but the 2011 and 2019 survey was only for certified farms.

<a href="https://quickstats.nass.usda.gov/results/12827E57-2B20-368C-A91F-1B97FF8F7B35#E2E0BF24-9F88-35DB-8526-8E47D941FDBF" class="uri">https://quickstats.nass.usda.gov/results/12827E57-2B20-368C-A91F-1B97FF8F7B35#E2E0BF24-9F88-35DB-8526-8E47D941FDBF</a>

### Contact Info

Project Lead: Dr. Isaac Mpanga Area Associate Agent Commercial
Horticulture and Small Acreage 2830 N Commonwealth Dr, Camp Verde, AZ
86322 email: <mpangai@arizona.edu>

Analysis in this repository: David LeBauer (<dlebauer@arizona.edu>) and
Jessica Guo (<jessicaguo@arizona.edu>)

References
----------

USDA (2010) “2008 Organic Production Survey”
[TXT](https://www.nass.usda.gov/Publications/AgCensus/2007/Online_Highlights/Organics/ORGANICS.txt)

USDA (2012) “2011 Certified Organic Production Survey”
[TXT](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/8623j1717/4b29b896g/OrganicProduction-10-04-2012.txt)

USDA (2016) “2014 and 2015 Organic Certifier Data”
[pdf](https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Organic_Production/Organic_Certifiers/2016/USDA_Accredited_Certifying_Agent_Certified_Organic_Data_2014_2015.pdf).
(Raw data from
[2014](https://www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Organics/ORGANICS.txt),
[2015](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/pr76f6075/hx11xj08s/OrganicProduction-09-15-2016.txt))

USDA (2017) “2016 Certified Organic Survey”
[TXT](https://downloads.usda.library.cornell.edu/usda-esmis/files/zg64tk92g/70795b52w/6q182n781/OrganicProduction-09-20-2017_correction.txt)

USDA (2020) “2019 Certified Organic Survey” Volume 3 Special Studies
Part 4. AC-17-SS-4:
[TXT](https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/Organics/ORGANICS.txt)

Software Dependencies
---------------------

``` r
devtools::session_info()
```

    ## - Session info ---------------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 4.0.2 (2020-06-22)
    ##  os       Windows 10 x64              
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_United States.1252  
    ##  ctype    English_United States.1252  
    ##  tz       America/Phoenix             
    ##  date     2021-05-24                  
    ## 
    ## - Packages -------------------------------------------------------------------
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
    ##  cachem        1.0.4   2021-02-13 [1] CRAN (R 4.0.4)
    ##  callr         3.5.1   2020-10-13 [1] CRAN (R 4.0.3)
    ##  cli           2.3.1   2021-02-23 [1] CRAN (R 4.0.4)
    ##  crayon        1.4.1   2021-02-08 [1] CRAN (R 4.0.2)
    ##  desc          1.3.0   2021-03-05 [1] CRAN (R 4.0.4)
    ##  devtools      2.3.2   2020-09-18 [1] CRAN (R 4.0.2)
    ##  digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.3)
    ##  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.2)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.2)
    ##  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.0.3)
    ##  fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.3)
    ##  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
    ##  htmltools     0.5.1.1 2021-01-22 [1] CRAN (R 4.0.3)
    ##  knitr         1.31    2021-01-27 [1] CRAN (R 4.0.3)
    ##  lifecycle     1.0.0   2021-02-15 [1] CRAN (R 4.0.4)
    ##  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.3)
    ##  memoise       2.0.0   2021-01-26 [1] CRAN (R 4.0.3)
    ##  pkgbuild      1.2.0   2020-12-15 [1] CRAN (R 4.0.3)
    ##  pkgload       1.2.0   2021-02-23 [1] CRAN (R 4.0.4)
    ##  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.0.2)
    ##  processx      3.4.5   2020-11-30 [1] CRAN (R 4.0.3)
    ##  ps            1.6.0   2021-02-28 [1] CRAN (R 4.0.4)
    ##  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
    ##  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.3)
    ##  remotes       2.2.0   2020-07-21 [1] CRAN (R 4.0.2)
    ##  rlang         0.4.10  2020-12-30 [1] CRAN (R 4.0.3)
    ##  rmarkdown     2.7     2021-02-19 [1] CRAN (R 4.0.4)
    ##  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.3)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
    ##  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.3)
    ##  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
    ##  testthat      3.0.2   2021-02-14 [1] CRAN (R 4.0.4)
    ##  usethis       2.0.1   2021-02-10 [1] CRAN (R 4.0.2)
    ##  withr         2.4.1   2021-01-26 [1] CRAN (R 4.0.3)
    ##  xfun          0.20    2021-01-06 [1] CRAN (R 4.0.3)
    ##  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.2)
    ## 
    ## [1] C:/Users/David/Documents/lib/R
    ## [2] C:/Program Files/R/R-4.0.2/library
