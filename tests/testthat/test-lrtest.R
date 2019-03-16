mstdesign <- "
    B1 =~ c(i1, i2, i3, i4, i5)
    B2 =~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)
    B4 =~ c(i16, i17, i18, i19, i20)
    B5 =~ c(i21, i22, i23, i24, i25)
    B6 =~ c(i26, i27, i28, i29, i30)

    # define starting Block
    Start == B4

    # define branches
    b1 := Start(0,2) + B2(0,2) + B1(0,5)
    b2 := Start(0,2) + B2(3,5) + B3(0,5)
    b3 := Start(3,5) + B5(0,2) + B3(0,5)
    b4 := Start(3,5) + B5(3,5) + B6(0,5)
  "

dat <- tmt:::sim.rm(100,5,1111)
datna <- dat
datna[sample(1:length(datna),50,replace = FALSE)] <- NA
datrm_1 <- tmt_rm(dat, optimization="optim")

items <- seq(-2,2,length.out=30)
names(items) <- c(paste0("i",1:30))
set.seed(1111)
dat_mst <- tmt_sim(mstdesign = mstdesign,
			items = items,
			persons = 500,
			mean = 0,
			sd = 1)
datrm_1 <- tmt_rm(dat, optimization="optim")
datrm_1na <- tmt_rm(datna, optimization="optim")
datrm_2 <- tmt_rm(dat_mst$data,mstdesign=mstdesign, optimization="optim")

datlrt_1 <- tmt_lrtest(datrm_1, optimization="optim")
datlrt_1na <- tmt_lrtest(datrm_1na, optimization="optim")
datlrt_2 <- tmt_lrtest(datrm_2, optimization="optim")

if(parallel::detectCores() >=2 ){
  datlrt_1p <- tmt_lrtest(datrm_1, cores = 2, optimization="optim")
  datlrt_2p <- tmt_lrtest(datrm_2, cores = 2, optimization="optim")
}
# -----------------------------------------------------------------
context("test-lrtest")
# -----------------------------------------------------------------
  test_that("tmt_lrtest classes", {
    expect_s3_class(datlrt_1,"lrtest_nmst") 
    expect_s3_class(datlrt_1na,"lrtest_nmst") 
    expect_s3_class(datlrt_2,"lrtest_mst") 
    }
  )
# -----------------------------------------------------------------
context("test-lrtest lrtest.nmst")
# -----------------------------------------------------------------
  if(parallel::detectCores() >=2 ){
    expect_that(datlrt_1$LRvalue, is_equivalent_to(datlrt_1p$LRvalue))
  }
  test_that("tmt_lrtest split = mean", {
    split1 <- "mean"
    set.seed(1111)
    split2 <- sample(x = c(1,2), size = 100, replace = TRUE)
    set.seed(1111)
    split2mst <- sample(x = c(1,2), size = 500, replace = TRUE)
    expect_type(tmt_lrtest(datrm_1, split = split1, optimization="optim"),"list")
    expect_type(tmt_lrtest(datrm_1, split = split2, optimization="optim"),"list")
    expect_type(tmt_lrtest(datrm_1na, split = split1, optimization="optim"),"list")
    expect_type(tmt_lrtest(datrm_1na, split = split2, optimization="optim"),"list")
    expect_type(tmt_lrtest(datrm_2, split = split1, optimization="optim"),"list")
    expect_type(tmt_lrtest(datrm_2, split = split2mst, optimization="optim"),"list")
  })


  if(parallel::detectCores() >=2 ){
    # -----------------------------------------------------------------
    context("test-lrtest lrtest.mst parallel")
    # -----------------------------------------------------------------
    expect_that(datlrt_2$LRvalue, is_equivalent_to(datlrt_2p$LRvalue))
  }

# -----------------------------------------------------------------
context("test-lrtest lrtest errors")
# -----------------------------------------------------------------
  test_that("tmt_lrtest split poly", {
    split4a <- sample(x = c(1,2,3), size = 100, replace = TRUE)
    split4b <- sample(x = c(1,2,3), size = 500, replace = TRUE)
    expect_that(tmt_lrtest(datrm_1, split = split4a, optimization="optim"), throws_error())
    expect_that(tmt_lrtest(datrm_1na, split = split4a, optimization="optim"), throws_error())
    expect_that(tmt_lrtest(datrm_2, split = split4b, optimization="optim"), throws_error())
  })
  
  test_that("tmt_lrtest split to small", {
    split5 <- sample(x = c(1,2), size = 100-10, replace = TRUE)
    expect_that(tmt_lrtest(datrm_1, split = split5, optimization="optim"), throws_error())
    expect_that(tmt_lrtest(datrm_1na, split = split5, optimization="optim"), throws_error())
    expect_that(tmt_lrtest(datrm_2, split = split5, optimization="optim"), throws_error())
  })

  test_that("tmt_lrtest to much cores", {
    expect_that(tmt_lrtest(datrm_1, cores = 100, optimization="optim"), throws_error())
    expect_that(tmt_lrtest(datrm_1na, cores = 100, optimization="optim"), throws_error())
    expect_that(tmt_lrtest(datrm_2, cores = 100, optimization="optim"), throws_error())
  })

  test_that("tmt_lrtest wrong class", {
    datrm_2 <- datrm_1
    class(datrm_2) <- "RM"
    expect_that(tmt_lrtest(datrm_2, optimization="optim"), throws_error())
  })

  test_that("tmt_lrtest data_check", {
    datrm_1db <- datrm_1da <- datrm_1
    datrm_1dbna <- datrm_1dana <- datrm_1na
    datrm_2db <- datrm_2da <- datrm_2
    split5 <- rep(c(1,2),50)
    split6a <- rep(c(1,2),250)
    datrm_1da$data[split5==1,] <- datrm_1da$data[split5==1,]*0
    datrm_1dana$data[split5==1,] <- datrm_1dana$data[split5==1,]*0
    datrm_1db$data[split5==1,1] <- 0
    datrm_1dbna$data[split5==1,1] <- 0

    datrm_2da$data[split6a==1,] <- datrm_2da$data[split6a==1,]*0
    datrm_2db$data[split6a==1,1] <- 0
      if(.Platform$OS.type=="windows"){
        sink("NUL")
      } else {
        sink("/dev/null")
      }
      expect_that(suppressWarnings(tmt_lrtest(datrm_1da, split = split5, optimization="optim")), throws_error())
      expect_that(tmt_lrtest(datrm_1db, split = split5, optimization="optim"), gives_warning())
      expect_that(suppressWarnings(tmt_lrtest(datrm_1dana, split = split5, optimization="optim")), throws_error())
      expect_that(tmt_lrtest(datrm_1dbna, split = split5, optimization="optim"), gives_warning())
      expect_that(suppressWarnings(tmt_lrtest(datrm_2da, split = split6a, optimization="optim")), throws_error())
      expect_that(suppressWarnings(tmt_lrtest(datrm_2db, split = split6a, optimization="optim")), throws_error())
    sink()
  })
