# tmt <img src="man/figures/tmt.png" width="120" align="right" alt=""/>

<!-- README.md is generated from README.Rmd-->

[![Build
Status](https://travis-ci.com/jansteinfeld/tmt.svg?branch=master)](https://travis-ci.com/jansteinfeld/tmt)
[![codecov](https://codecov.io/gh/jansteinfeld/tmt/branch/master/graph/badge.svg?token=UwLlcu9JXp)](https://codecov.io/gh/jansteinfeld/tmt)
[![GPL
Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)](https://opensource.org/licenses/GPL-3.0/)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://github.com/jansteinfeld/tmt/commits)
[![GitHub last
commit](https://img.shields.io/github/last-commit/jansteinfeld/tmt.svg)](https://github.com/jansteinfeld/tmt)
[![GitHub pull
requests](https://img.shields.io/github/issues-pr/jansteinfeld/tmt.svg)](https://github.com/jansteinfeld/tmt/pulls)
[![GitHub
issues](https://img.shields.io/github/issues-raw/jansteinfeld/tmt.svg)](https://github.com/jansteinfeld/tmt/issues)
[![GitHub
version](https://img.shields.io/badge/github%20version-0.1.9--3-orange.svg)](https://github.com/jansteinfeld/tmt)
[![CRAN](http://www.r-pkg.org/badges/version/tmt)](https://cran.r-project.org/package=tmt)
[![CRAN/Downloads](http://cranlogs.r-pkg.org/badges/tmt?color=brightgreen)](http://www.r-pkg.org/pkg/tmt)

The *tmt* Package provides conditional maximum likelihood (CML)
estimation of item parameters in multistage designs (Zwitser & Maris,
2013, <https://doi.org/10.1007/s11336-013-9369-6>) and CML estimation
for conventional designs. Additional features are the Likelihood Ratio
test (Andersen, 1973, <https://doi.org/10.1007/BF02291180>) and
simulation of multistage designs.

## Installation

To install the development version of the *tmt* package, please copy
this commands in your R console:

``` r
# Install release version from CRAN
install.packages("tmt")
# Install development version from GitHub
devtools::install_github("jansteinfeld/tmt")
```

## Usage

You will find a detailed description of the package in the vignette:

``` r
library(tmt)

mstdesign <- "
    M1 =~ paste0('i',1:5)
    M2 =~ c(i6, i7, i8, i9, i10)
    M3 =~ c(i11, i12, i13, i14, i15)
    M4 =~ c(i16, i17, i18, i19, i20)
    M5 =~ c(i21, i22, i23, i24, i25)
    M6 =~ c(i26, i27, i28, i29, i30)

    # define starting module
    Start == M4

    # define branches
    p1 := Start(0,2) + M2(0,2) + M1(0,5)
    p2 := Start(0,2) + M2(3,5) + M3(0,5)
    p3 := Start(3,5) + M5(0,2) + M3(0,5)
    p4 := Start(3,5) + M5(3,5) + M6(0,5)
  "
# simulate some data
  items <- seq(-2,2,length.out=30)
  names(items) <- c(paste0("i",1:30))
  set.seed(1111)
  dat_mst <- tmt_sim(mstdesign = mstdesign,
        items = items,
        persons = 500,
        mean = 0,
        sd = 1)
# estimate

mod1 <- tmt_rm(dat_mst,mstdesign = mstdesign)

summary(mod1)
#> 
#> Call:
#>   tmt_rm(dat = dat_mst, mstdesign = mstdesign)
#> 
#> 
#> Results of Rasch model (mst) estimation: 
#> 
#> Difficulty parameters: 
#>             est.b_i1   est.b_i2  est.b_i3   est.b_i4   est.b_i5   est.b_i6
#> Estimate   -1.715056 -1.6100686 -1.508108 -1.6100686 -1.3113453 -1.0468543
#> Std. Error  0.247141  0.2439233  0.241220  0.2439233  0.2371614  0.1520214
#>              est.b_i7   est.b_i8   est.b_i9  est.b_i10  est.b_i11
#> Estimate   -1.1508557 -0.9958313 -0.6997328 -0.4607495 -0.3807713
#> Std. Error  0.1535503  0.1513333  0.1480823  0.1464329  0.1326975
#>             est.b_i12 est.b_i13  est.b_i14   est.b_i15  est.b_i16
#> Estimate   -0.6500487 -0.222511 -0.2082786 -0.05286967 0.07542945
#> Std. Error  0.1359553  0.131522  0.1314425  0.13085158 0.11053665
#>            est.b_i17 est.b_i18 est.b_i19 est.b_i20 est.b_i21 est.b_i22
#> Estimate   0.2472522 0.3046942 0.5762328 0.5959613 0.7193856 0.5738666
#> Std. Error 0.1108281 0.1109871 0.1121476 0.1122602 0.1668250 0.1662375
#>            est.b_i23 est.b_i24 est.b_i25 est.b_i26 est.b_i27 est.b_i28
#> Estimate   0.8659746 1.0363980 1.1895060  1.401457  1.401457 1.2465019
#> Std. Error 0.1677771 0.1693014 0.1710693  0.245725  0.245725 0.2450162
#>            est.b_i29 est.b_i30
#> Estimate   1.5586564  1.830376
#> Std. Error 0.2475203  0.253225
#> 
#> CLL: -3222.077 
#> Number of iterations: 59 
#> Number of parameters: 30
```

### Outlook

The following features are planned for future releases:

  - the partial credit model for multistage designs
  - missing values in multistage designs
  - improving the speed of the package
  - plots of the multistage design
