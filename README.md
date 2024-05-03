# tmt <img src="man/figures/tmt.png" width="120" align="right" alt=""/>

<!-- README.md is generated from README.Rmd-->

[![R-CMD-check](https://github.com/jansteinfeld/tmt/actions/workflows/check-full.yaml/badge.svg)](https://github.com/jansteinfeld/tmt/actions/workflows/check-full.yaml)
[![GitHub
test-coverage](https://github.com/jansteinfeld/tmt/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jansteinfeld/tmt/actions/workflows/test-coverage.yaml)
[![GitHub
pages-build](https://github.com/jansteinfeld/tmt/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/jansteinfeld/tmt/actions/workflows/pages/pages-build-deployment)
[![GitHub
version](https://img.shields.io/github/r-package/v/jansteinfeld/tmt?label=version&logo=github)](https://github.com/jansteinfeld/tmt/)
[![GitHub
release](https://img.shields.io/github/v/release/jansteinfeld/tmt?label=release&logo=github)](https://github.com/jansteinfeld/tmt/)
[![GitHub pull
requests](https://img.shields.io/github/issues-pr/jansteinfeld/tmt?label=pull%20requests&logo=github)](https://github.com/jansteinfeld/tmt/pulls)
[![GitHub
issues](https://img.shields.io/github/issues-raw/jansteinfeld/tmt?label=issues&logo=github)](https://github.com/jansteinfeld/tmt/issues)

[![codecov](https://codecov.io/gh/jansteinfeld/tmt/branch/master/graph/badge.svg?token=11lw4stBoI)](https://app.codecov.io/gh/jansteinfeld/tmt)
[![CRAN
version](https://img.shields.io/cran/v/tmt?label=CRAN%20version)](https://cran.r-project.org/package=tmt)
[![CRAN
checks](https://badges.cranchecks.info/summary/tmt.svg)](https://cran.r-project.org/web/checks/check_results_tmt.html)
[![downloads](http://cranlogs.r-pkg.org/badges/last-month/tmt?color=blue)](https://cran.r-project.org/package=tmt)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://github.com/jansteinfeld/tmt)
[![License](https://img.shields.io/cran/l/tmt)](https://opensource.org/license/GPL-3.0)

The *tmt* Package provides conditional maximum likelihood (CML) item
parameter estimation of sequential as well as cumulative deterministic
multistage (MST) designs (Zwitser & Maris, 2015,
[\<10.1007/s11336-013-9369-6\>](https://doi.org/10.1007/s11336-013-9369-6))
as well as probabilistic sequential and cumulative multistage designs
(Steinfeld & Robitzsch, 2021,
[\<10.31234/osf.io/ew27f\>](https://doi.org/10.31234/osf.io/ew27f)).
Supports CML item parameter estimation of conventional linear designs
and additional functions for the likelihood ratio test (Andersen, 1973,
[\<10.1007/BF02291180\>](https://doi.org/10.1007/BF02291180)) as well as
functions for the simulation of several kinds of multistage designs.

## Installation

To install the latest (development) version of the *tmt* package, please
copy the following commands in your R console:

``` r
# Install release version from CRAN
install.packages("tmt")
# Install development version from GitHub
devtools::install_github("jansteinfeld/tmt")
```

## Usage

The application of the *tmt* package is illustrated below. Further
examples of different MST designs can be found in the associated
vignette of the package. To apply the package and the CML method, it is
first necessary to specify the MST design. For this purpose, a model
language has been developed, which is illustrated in the first part of
the example below. First, each module of the design needs to be
specified. The following deterministic sequential MST design consists of
six modules, four paths and three stages. In the first part, the modules
of the MST design are defined (basically the allocation of items).
Different methods are available, the user can either use the R function
*paste*, but also address the elements manually as vectors. It is
important that the names of the specified elements in the modules match
those in the data. To illustrate the application, some data is then
simulated based on the specified MST design. In this example, a seed has
been set to make the results easier to compare and follow. The *tmt_rm*
function is available for the actual estimation of the item parameters.
If the data has been generated with the *tmt_sim* function, it would be
sufficient to export the data generated with this function as part of
the MST design. If the data has not been generated synthetically with
this function, it is necessary to specify the MST design.

A detailed description of the package (such as sequential cumulative and
probabilistic MST designs) can be found in the vignette.

``` r
library(tmt)

# spezification of the mst design
mstdesign <- "
    M1 =~ paste0('i',1:5)
    M2 =~ c(i6, i7, i8, i9, i10)
    M3 =~ c(i11, i12, i13, i14, i15)
    M4 =~ c(i16, i17, i18, i19, i20)
    M5 =~ c(i21, i22, i23, i24, i25)
    M6 =~ c(i26, i27, i28, i29, i30)

    # define branches
    p1 := M4(0,2) + M2(0,2) + M1(0,5)
    p2 := M4(0,2) + M2(3,5) + M3(0,5)
    p3 := M4(3,5) + M5(0,2) + M3(0,5)
    p4 := M4(3,5) + M5(3,5) + M6(0,5)
  "

# application of the simulation function to generate som synthetic data
  items <- seq(-2,2,length.out=30)
  names(items) <- paste0("i",1:30)
  
  dat_mst <- tmt_sim(mstdesign = mstdesign,
        items = items,
        persons = 500,
        seed = 1111)

# estimate the item parameters
mod1 <- tmt_rm(dat_mst, mstdesign = mstdesign)


summary(mod1)
#> 
#> Call:
#>   tmt_rm(dat = dat_mst, mstdesign = mstdesign)
#> 
#> 
#> Results of Rasch model (mst) estimation: 
#> 
#> Difficulty parameters: 
#>              est.b_i1   est.b_i2   est.b_i3   est.b_i4   est.b_i5   est.b_i6   est.b_i7   est.b_i8   est.b_i9
#> Estimate   -2.3490510 -1.7664751 -2.1028057 -1.9872254 -1.4511516 -1.3097642 -1.2542426 -0.8697938 -0.7865043
#> Std. Error  0.2700265  0.2498593  0.2594456  0.2555346  0.2456593  0.1556009  0.1546331  0.1491392  0.1482092
#>             est.b_i10  est.b_i11  est.b_i12  est.b_i13   est.b_i14  est.b_i15  est.b_i16 est.b_i17 est.b_i18
#> Estimate   -0.6227596 -0.6582138 -0.2153395 -0.2016450 -0.07909168 0.06922466 0.04382588 0.2171609 0.2750574
#> Std. Error  0.1466760  0.1346944  0.1297456  0.1296568  0.12903128 0.12868102 0.11184169 0.1120871 0.1122291
#>            est.b_i19 est.b_i20 est.b_i21 est.b_i22 est.b_i23 est.b_i24 est.b_i25 est.b_i26 est.b_i27 est.b_i28
#> Estimate   0.5483552 0.5681985 0.6245467 0.9624785 1.0910584 1.2884075 1.5650529 1.4674082 1.5247222 1.5824928
#> Std. Error 0.1132990 0.1134044 0.1663841 0.1683083 0.1695429 0.1719177 0.1763681 0.2582108 0.2588762 0.2596804
#>            est.b_i29 est.b_i30
#> Estimate   2.0063785  1.819695
#> Std. Error 0.2698234  0.264414
#> 
#> CLL: -3179.989 
#> Number of iterations: 60 
#> Number of parameters: 30
```

### Outlook

The following features are planned for future releases:

- the partial credit model for multistage designs
- missing values in multistage designs
- improving the speed of the package
- plots of the multistage design

## References

- Glas, C. A. W. (1988). The Rasch Model and Multistage Testing.
  *Journal of Educational Statistics, 13*(1), 45. doi: 10.2307/1164950
- Steinfeld, J., & Robitzsch, A. (accepted). Conditional maximum
  likelihood estimation in probability-based multistage designs.
  *Behaviormetrika, xx*(x), xxx-xxx.
- Steinfeld, J., Robitzsch, A. (2023). Estimating item parameters in
  multistage designs with the tmt package in R. *Quantitative and
  Computational Methods in Behavioral Science, 3*, e10087.
  <https://doi.org/10.5964/qcmb.10087>
- Steinfeld, J., & Robitzsch, A. (2021). Item parameter estimation in
  multistage designs: A comparison of different estimation approaches
  for the Rasch model. *Psych, 3*(3), 279-307.
  <https://doi.org/10.3390/psych3030022>
- Zwitser, R. J., & Maris, G. (2013). Conditional statistical inference
  with multistage testing designs. *Psychometrika, 80*(1), 65-84. doi:
  10.1007/s11336-013-9369-6
