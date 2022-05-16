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
checks](https://cranchecks.info/badges/summary/tmt)](https://cran.r-project.org/web/checks/check_results_tmt.html)
[![package
dependencies](https://tinyverse.netlify.com/badge/tmt)](https://cran.r-project.org/package=tmt)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://github.com/jansteinfeld/tmt)
[![License](https://img.shields.io/cran/l/tmt)](https://opensource.org/licenses/GPL-3.0/)

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

The application of the package *tmt* is illustrated below. Further,
examples of different MST designs can be found in the associated
vignette of the package. For the application of the package and the CML
method, it is necessary first to specify the MST design. For this
purpose, a model language was developed, which is illustrated in the
first section of the example. First, each module of the design must be
specified. The following deterministic sequential MST design consists of
six modules, four paths’ and three stages. In the first part, the
modules of the MST designs are defined (basically the allocation of
items). Here different methods are available, the user can apply either
the R function *paste*, but also address the items manually by hand as
vector. It is important that the names of the specified items in the
modules match those in the data. For the application illustration, some
data is subsequently simulated on the basis of the specified MST design.
In this example, a seed was set so that the results are easier to
compare and to follow this example. For the actual estimation of the
item parameters, the function *tmt_rm* is available. If data has been
created with the *tmt_sim* function, it would be enough to hand over the
data generated with this function as part of the export is also the MST
design. If the data has not been synthetically generated with this
function, it is necessary to specify the MST design.

You will find a detailed description of the package in the vignette
(like sequential cumulative as well as probabilistic MST designs).

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
#>              est.b_i1   est.b_i2   est.b_i3  est.b_i4   est.b_i5  est.b_i6
#> Estimate   -1.8723315 -2.0958741 -1.6615261 -1.713300 -1.8186340 -1.158071
#> Std. Error  0.2491036  0.2565554  0.2440581  0.245125  0.2476405  0.150556
#>              est.b_i7   est.b_i8   est.b_i9  est.b_i10  est.b_i11  est.b_i12
#> Estimate   -0.9886683 -0.8420062 -0.9228962 -0.7308394 -0.6285880 -0.2757812
#> Std. Error  0.1479676  0.1460532  0.1470717  0.1448089  0.1332405  0.1294712
#>             est.b_i13   est.b_i14  est.b_i15  est.b_i16 est.b_i17 est.b_i18
#> Estimate   -0.3722209 -0.08624961 0.07430272 0.01757996 0.2588665 0.2297421
#> Std. Error  0.1302474  0.12848723 0.12820874 0.11110163 0.1117521 0.1116458
#>            est.b_i19 est.b_i20 est.b_i21 est.b_i22 est.b_i23 est.b_i24
#> Estimate   0.5151070 0.5050877 0.7641251 0.8298368 1.1410283 1.1865533
#> Std. Error 0.1129994 0.1129397 0.1709856 0.1713395 0.1740409 0.1745734
#>            est.b_i25 est.b_i26 est.b_i27 est.b_i28 est.b_i29 est.b_i30
#> Estimate   1.1865533 1.9094270 1.5464586 1.5464586 1.5464586 1.9094009
#> Std. Error 0.1745734 0.2681366 0.2600917 0.2600917 0.2600917 0.2681357
#> 
#> CLL: -3219.822 
#> Number of iterations: 44 
#> Number of parameters: 30
```

### Outlook

The following features are planned for future releases:

-   the partial credit model for multistage designs
-   missing values in multistage designs
-   improving the speed of the package
-   plots of the multistage design

## References

-   Glas, C. A. W. (1988). The Rasch Model and Multistage Testing.
    *Journal of Educational Statistics*, 13(1), 45. doi: 10.2307/1164950
-   Steinfeld, J., & Robitzsch, A. (2021). Conditional maximum
    likelihood estimation in probability-branched multistage designs.
    *PsyArXiv*. 20 March 2021. doi: 10.31234/osf.io/ew27f
-   Zwitser, R. J., & Maris, G. (2013). Conditional statistical
    inference with multistage testing designs. *Psychometrika*, 80(1),
    65–84. doi: 10.1007/s11336-013-9369-6
