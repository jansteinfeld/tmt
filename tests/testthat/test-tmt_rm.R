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
# Vorbereitung
set.seed(1111)
dat <- tmt:::sim.rm(theta = 100, b = 5)
datna <- dat
datna[sample(1:length(datna),10,replace = FALSE)] <- NA

items <- seq(-2,2,length.out=30)
names(items) <- c(paste0("i",1:30))
set.seed(1111)
dat_mst <- tmt_sim(mstdesign = mstdesign,
			items = items,
			persons = 500,
			mean = 0,
			sd = 1)

datrm_1 <- tmt_rm(dat, optimization="optim")

dat_nlm_se <- tmt_rm(dat = dat, optimization = "nlminb", se = TRUE)
dat_opt_se <- tmt_rm(dat = dat, optimization = "optim", se = TRUE)
dat_nlm <- tmt_rm(dat = dat, optimization = "nlminb", se = FALSE)
dat_opt <- tmt_rm(dat = dat, optimization = "optim", se = FALSE)

datna_nlm_se <- tmt_rm(dat = datna, optimization = "nlminb", se = TRUE)
datna_opt_se <- tmt_rm(dat = datna, optimization = "optim", se = TRUE)
datna_nlm <- tmt_rm(dat = datna, optimization = "nlminb", se = FALSE)
datna_opt <- tmt_rm(dat = datna, optimization = "optim", se = FALSE)

datmst_opt_se <- tmt_rm(dat_mst, optimization="optim", se = TRUE)
datmst_nlm_se <- tmt_rm(dat_mst, optimization="nlminb", se = TRUE)

datmst_opt <- tmt_rm(dat_mst, optimization="optim", se = FALSE)
datmst_nlm <- tmt_rm(dat_mst, optimization="nlminb", se = FALSE)

datmst_opt_0 <- tmt_rm(dat_mst, optimization="optim", sum0 = FALSE)
datmst_nlm_0 <- tmt_rm(dat_mst, optimization="nlminb", sum0 = FALSE)

dat_nlm_0 <- tmt_rm(dat = dat, optimization = "nlminb", sum0 = FALSE)
dat_opt_0 <- tmt_rm(dat = dat, optimization = "optim", sum0 = FALSE)
datna_nlm_0 <- tmt_rm(dat = datna, optimization = "nlminb", sum0 = FALSE)
datna_opt_0 <- tmt_rm(dat = datna, optimization = "optim", sum0 = FALSE)


# -----------------------------------------------------------------
context("test-tmt_rm")
# -----------------------------------------------------------------
  test_that("tmt_rm data structure", {
    expect_is(dat_nlm_se,"nmst")
    expect_is(dat_opt_se,"nmst")
    expect_is(datna_nlm,"nmst")
    expect_is(datna_opt,"nmst")
    expect_is(datmst_nlm_se,"mst")
    expect_is(datmst_opt_se,"mst")
    expect_is(datmst_nlm,"mst")
    expect_is(datmst_opt,"mst")
  })
  test_that("tmt_rm content", {
    expect_that(tmt_rm(as.data.frame(dat_mst$data), mstdesign = mstdesign, optimization="optim")$betapar, 
        is_identical_to(tmt_rm(as.matrix(dat_mst$data), mstdesign = mstdesign, optimization="optim")$betapar))
  })

  test_that("tmt_rm se", {
    expect_that(dat_nlm_se$betapar, 
     is_identical_to(dat_nlm$betapar))
    expect_that(datna_nlm_se$betapar, 
     is_identical_to(datna_nlm$betapar))
    expect_that(dat_opt_se$betapar, 
     is_identical_to(dat_opt$betapar))
    expect_that(datna_opt_se$betapar, 
     is_identical_to(datna_opt$betapar))
    expect_that(datmst_nlm_se$betapar, 
     is_identical_to(datmst_nlm$betapar))
    expect_that(datmst_opt_se$betapar, 
     is_identical_to(datmst_opt$betapar))
  })


  test_that("tmt_rm sum0 = FALSE", {
    expect_equal(
        dat_nlm_0$betapar,
        dat_opt_0$betapar, tolerance = 0.001)
    expect_equal(
        datna_nlm_0$betapar,
        datna_opt_0$betapar, tolerance = 0.001) 
    expect_equal(
        datmst_opt_0$betapar,
        datmst_nlm_0$betapar, tolerance = 0.001)    
  })


  test_that("tmt_rm structure", {
    expect_s3_class(dat_nlm_se,"nmst")
    expect_s3_class(dat_opt_se,"nmst")
    expect_s3_class(datna_nlm,"nmst")
    expect_s3_class(datna_opt,"nmst")
    expect_s3_class(datmst_opt_se,"mst")
    expect_s3_class(datmst_opt_se,"mst")
    expect_s3_class(datmst_opt,"mst")
    expect_s3_class(datmst_opt,"mst")
  })

  test_that("tmt_rm structure", {
    expect_s3_class(dat_nlm_se,"nmst")
    expect_s3_class(dat_opt_se,"nmst")
    expect_s3_class(datna_nlm,"nmst")
    expect_s3_class(datna_opt,"nmst")
    expect_s3_class(datmst_opt_se,"mst")
    expect_s3_class(datmst_opt_se,"mst")
    expect_s3_class(datmst_opt,"mst")
    expect_s3_class(datmst_opt,"mst")
  })

# -----------------------------------------------------------------
context("test-tmt_rm warning and error")
# -----------------------------------------------------------------
  test_that("tmt_rm data structure", {
    expect_that(raschmodel.nmst(list(dat), optimization="optim"),
        throws_error()
        )
     expect_that(raschmodel.nmst(list(dat), optimization="nlminb"),
        throws_error()
        )
  })

