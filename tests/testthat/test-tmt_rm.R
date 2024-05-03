mstdesign <- "
    B1 =~ c(i1, i2, i3, i4, i5)
    B2 =~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)
    B4 =~ c(i16, i17, i18, i19, i20)
    B5 =~ c(i21, i22, i23, i24, i25)
    B6 =~ c(i26, i27, i28, i29, i30)

    # define branches
    b1 := B4(0,2) + B2(0,2) + B1(0,5)
    b2 := B4(0,2) + B2(3,5) + B3(0,5)
    b3 := B4(3,5) + B5(0,2) + B3(0,5)
    b4 := B4(3,5) + B5(3,5) + B6(0,5)
  "

mstdesign_wrong <- "
    B1 =~ c(ii1, i2, i3, i4, i5)
    B2 =~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)
    B4 =~ c(i16, i17, i18, i19, i20)
    B5 =~ c(i21, i22, i23, i24, i25)
    B6 =~ c(i26, i27, i28, i29, i30)

    # define branches
    b1 := B4(0,2) + B2(0,2) + B1(0,5)
    b2 := B4(0,2) + B2(3,5) + B3(0,5)
    b3 := B4(3,5) + B5(0,2) + B3(0,5)
    b4 := B4(3,5) + B5(3,5) + B6(0,5)
  "
mstdesign_precon <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)

  # define constraints
  xcat == data$xcat

  # define routing criteria
  r1 = c(0.35,0.30,0.25,0.90,0.85,0.80,0.90,0.85,0.80,0.75,0.70,0.65,0.60,0.55)
  r2 = c(0.65,0.70,0.75,0.10,0.15,0.20,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45)
  r3 = c(0.30,0.25,0.15,0.95,0.93,0.90,0.95,0.93,0.90,0.85,0.83,0.80,0.75,0.73,0.70,0.68,0.63,0.60,0.58,0.53,0.50,0.48,0.43,0.40)
  r4 = c(0.70,0.75,0.85,0.05,0.07,0.10,0.05,0.07,0.10,0.15,0.17,0.20,0.25,0.27,0.30,0.32,0.37,0.40,0.42,0.47,0.50,0.52,0.57,0.60)

  # define path
  p1:= xcat(1:4) += B4(r1) += B2(r3) += B1
  p2:= xcat(1:4) += B4(r1) += B2(r4) += B3
  p3:= xcat(1:4) += B4(r2) += B5(r3) += B3
  p4:= xcat(1:4) += B4(r2) += B5(r4) += B6
"

mstdesign_small_v1 <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7)
  B3 =~ c(i8, i9, i10, i11, i12)

  # define branches
  b1 := B2(0,0) + B1
  b2 := B2(1,2) + B3
 "

mstdesign_small_v2 <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7, i8)
  B3 =~ c(i9, i10, i11, i12,i13)

  # define branches
  b1 := B2(0,1) + B1
  b2 := B2(2,3) + B3
 "


mstdesign_small_v3 <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7)
  B3 =~ c(i8, i9, i10, i11, i12)
  
  r1 = c(0.9,0.1,0.1)
  r2 = c(0.1,0.9,0.9)

  # define branches
  b1 := B2(r1) + B1
  b2 := B2(r2) + B3
"

# Vorbereitung

dat <- tmt:::sim.rm(theta = 100, b = 5, c(1111,1112))
datna <- dat
datna[sample(seq_len(length(datna)),10,replace = FALSE)] <- NA

items <- seq(-2,2,length.out=30)
names(items) <- c(paste0("i",1:30))

items_wrong <- items
names(items_wrong)[1] <- "ii1"

items_precon <- seq(-2,2,length.out = 60)
names(items_precon) <- c(paste0("i", 1:60))

items_small_v1 <- seq(-2,2,length.out = 12)
names(items_small_v1) <- c(paste0("i", 1:12))

items_small_v2 <- seq(-2,2,length.out = 13)
names(items_small_v2) <- c(paste0("i", 1:13))

dat_mst <- tmt_sim(mstdesign = mstdesign,
			items = items,
			persons = 500,
			seed = 1111)

dat_precon1 <- tmt_sim(mstdesign = mstdesign_precon,
			items = items_precon,
			persons = 500,
      preconditions = 0.4,
			seed = 1111)

dat_small_v1 <- tmt_sim(mstdesign = mstdesign_small_v1,
			items = items_small_v1,
			persons = 1500,
			seed = 1111)

dat_small_v2 <- tmt_sim(mstdesign = mstdesign_small_v2,
			items = items_small_v2,
			persons = 1500,
			seed = 1111)

dat_mst_wrong <- dat_mst
colnames(dat_mst_wrong$data) <- names(items_wrong)

datrm_1 <- tmt_rm(dat, optimization = "optim")

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

datrm_precon_nlm_0 <- tmt_rm(dat = dat_precon1, optimization = "nlminb", sum0 = FALSE)
datrm_precon_opt_0 <- tmt_rm(dat = dat_precon1, optimization = "optim", sum0 = FALSE)


datrm_precon_nlm_se <- tmt_rm(dat = dat_precon1, optimization = "nlminb", se = TRUE)
datrm_precon_opt_se <- tmt_rm(dat = dat_precon1, optimization = "optim", se = TRUE)
datrm_precon_nlm <- tmt_rm(dat = dat_precon1, optimization = "nlminb", se = FALSE)
datrm_precon_opt <- tmt_rm(dat = dat_precon1, optimization = "optim", se = FALSE)



datrm_smallmst_v1_nlm <- tmt_rm(dat = dat_small_v1, optimization = "nlminb", se = FALSE)
datrm_smallmst_v1_opt <- tmt_rm(dat = dat_small_v1, optimization = "optim", se = FALSE)



datrm_smallmst_v2_nlm_se <- tmt_rm(dat = dat_small_v2, optimization = "nlminb", se = TRUE)
datrm_smallmst_v2_opt_se <- tmt_rm(dat = dat_small_v2, optimization = "optim", se = TRUE)
datrm_smallmst_v2_nlm <- tmt_rm(dat = dat_small_v2, optimization = "nlminb", se = FALSE)
datrm_smallmst_v2_opt <- tmt_rm(dat = dat_small_v2, optimization = "optim", se = FALSE)


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
    expect_is(datrm_precon_nlm_se,"mst")
    expect_is(datrm_precon_opt_se,"mst")
    expect_is(datrm_precon_nlm,"mst")
    expect_is(datrm_precon_opt,"mst")
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
    expect_that(datrm_precon_nlm_se$betapar, 
     is_identical_to(datrm_precon_nlm$betapar))
    expect_that(datrm_precon_opt_se$betapar, 
     is_identical_to(datrm_precon_opt$betapar))
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
    expect_equal(
        datrm_precon_opt_0$betapar,
        datrm_precon_opt_0$betapar, tolerance = 0.001)
  })


test_that("tmt_rm sum0 = TRUE", {
    expect_equal(
      datrm_smallmst_v1_nlm$betapar,
      datrm_smallmst_v1_opt$betapar, tolerance = 0.001)
    
    expect_equal(
      datrm_smallmst_v2_nlm_se$betapar,
      datrm_smallmst_v2_opt_se$betapar, tolerance = 0.001)
    expect_equal(
      datrm_smallmst_v2_nlm$betapar,
      datrm_smallmst_v2_opt$betapar, tolerance = 0.001)
  })


  test_that("tmt_rm structure", {
    expect_s3_class(dat_nlm_se,"nmst")
    expect_s3_class(dat_opt_se,"nmst")
    expect_s3_class(datna_nlm,"nmst")
    expect_s3_class(datna_opt,"nmst")
    expect_s3_class(datmst_opt_se,"mst")
    expect_s3_class(datmst_nlm_se,"mst")
    expect_s3_class(datmst_opt,"mst")
    expect_s3_class(datmst_nlm,"mst")
    expect_s3_class(datrm_precon_opt,"mst")
    expect_s3_class(datrm_precon_nlm,"mst")
  })

  test_that("tmt_rm preconditions", {

  tmp <- split(dat_precon1$persons,dat_precon1$preconpar[[1]]) 



    expect_is(dat_precon1,"list")
    expect_false(is.null(dat_precon1$preconpar))
    expect_length(dat_precon1$preconpar,1)
    expect_length(dat_precon1$preconpar[[1]],nrow(dat_precon1$data_mst))
    
    expect_that(datmst_nlm_se$betapar, 
     is_identical_to(datmst_nlm$betapar))
    
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
     expect_that(suppressWarnings(tmt_rm(dat_mst$data, mstdesign = mstdesign_wrong)),
        throws_error()
        )
    expect_that(suppressWarnings(tmt_rm(dat_mst_wrong$data, mstdesign = mstdesign)),
        throws_error()
        )

    expect_that(tmt_rm(dat = dat_small_v1, optimization = "nlminb", se = TRUE),
        throws_error()
        )
    # expect_that(tmt_rm(dat = dat_small_v1, optimization = "optim", se = TRUE),
    #     gives_warning()
    #     )
  })
