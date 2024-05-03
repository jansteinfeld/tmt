
mstdesign <- "
    B1 =~ paste0('i',1:5)
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
# Vorbereitung
dat <- tmt:::sim.rm(100, 5, c(1111,1112))
colnames(dat) <- paste0("i",seq_len(ncol(dat)))
datna <- dat
datna[sample(seq_len(length(datna)),50,replace = FALSE)] <- NA
datrm_1 <- tmt_rm(dat, optimization="optim")

items <- seq(-2,2, length.out = 30)
names(items) <- c(paste0("i",seq_len(30)))
dat_mst <- tmt_sim(mstdesign = mstdesign,
			items = items,
			persons = 500,
			seed = 1111)
datrm_1a <- tmt_rm(dat, optimization = "optim")
datrm_1b <- tmt_rm(data.frame(dat), optimization = "optim")
datrm_1c <- tmt_rm(dat, optimization = "nlminb")
datrm_2a <- tmt_rm(dat_mst$data, mstdesign = mstdesign, optimization = "optim")
datrm_2b <- tmt_rm(dat_mst, optimization = "optim")
datrm_2c <- tmt_rm(dat_mst, optimization = "nlminb")



# -----------------------------------------------------------------
context("test-raschmodel")
# -----------------------------------------------------------------
  test_that("raschmodel.mst data structure", {
    expect_is(datrm_2a,"mst")
    expect_equal(datrm_1a$betapar,datrm_1b$betapar)
    expect_equal(datrm_1a$se.beta,datrm_1b$se.beta)
    expect_equal(datrm_2a$betapar,datrm_2b$betapar)
    expect_equal(datrm_2a$se.beta,datrm_2b$se.beta)
  })

  test_that("raschmodel se", {
    expect_that(tmt_rm(dat, se = FALSE, optimization = "optim")$se.beta, equals(NULL))
    expect_that(tmt_rm(dat, se = FALSE, optimization = "nlminb")$se.beta, equals(NULL))
    expect_that(tmt_rm(datna, se = FALSE, optimization = "optim")$se.beta, equals(NULL))
    expect_that(tmt_rm(datna, se = FALSE, optimization = "nlminb")$se.beta, equals(NULL))
    expect_that(tmt_rm(dat_mst, se = FALSE, optimization = "optim")$se.beta, equals(NULL))
    expect_that(tmt_rm(dat_mst, se = FALSE, optimization = "nlminb")$se.beta, equals(NULL))
  })

  test_that("raschmodel compare results nlminb and optim", {
    expect_equal(
      tmt_rm(dat, optimization = "optim")$betapar,
      tmt_rm(dat, optimization = "nlminb")$betapar, tolerance = 0.001)
    expect_equal(
      tmt_rm(datna, optimization = "optim")$betapar,
      tmt_rm(datna, optimization = "nlminb")$betapar, tolerance = 0.001)
    expect_equal(
      tmt_rm(dat_mst, optimization = "optim")$betapar,
      tmt_rm(dat_mst, optimization = "nlminb")$betapar, tolerance = 0.01)
    expect_equal(
      tmt_rm(dat, optimization = "optim")$se.beta,
      tmt_rm(dat, optimization = "nlminb")$se.beta, tolerance = 0.001)
    expect_equal(
      tmt_rm(dat_mst, optimization = "optim")$se.beta,
      tmt_rm(dat_mst, optimization = "nlminb")$se.beta, tolerance = 0.001)
  })

# -----------------------------------------------------------------
context("test-raschmodel check warnings")
# -----------------------------------------------------------------
test_that("error raschmodel.mst",{
		expect_that(tmt_rm(list(dat_mst$data),mstdesign=mstdesign, optimization="optim"),throws_error())
    expect_that(tmt_rm(list(dat_mst$data),mstdesign=mstdesign), throws_error())
    expect_that(tmt_rm(dat_mst,mstdesign=mstdesign, start = rep(0,10)), throws_error())
    expect_that(raschmodel.mst(dat_mst$data,mstdesign=NULL), throws_error())
})
test_that("error raschmodel.nmst",{
    expect_that(suppressWarnings(raschmodel.nmst(datrm_1a, start = rep(0,10))), throws_error())
    expect_that(suppressWarnings(raschmodel.nmst(dat, start = rep(0,10))), throws_error())
	})

# -----------------------------------------------------------------
context("test-raschmodel check errors")
# -----------------------------------------------------------------
 
  test_that("test-raschmodel mst wrong item names", {
    dat_mst_5 <- dat_mst_4 <- dat_mst_3 <- dat_mst
    dat_mst_4$data[!is.na(dat_mst_4$data[,1]),1] <- 0
    dat_mst_5$data[3:500,] <- 0
    colnames(dat_mst_3$data)[1] <- "ii1"
    expect_that(raschmodel.mst(dat_mst_3, optimization="optim"), throws_error())
    expect_that(tmt_rm(dat, optimization="optimo"), throws_error())
    expect_that(tmt_rm(dat_mst, optimization="optimo"), throws_error())
    expect_that(suppressWarnings(tmt_rm(dat_mst_4)), throws_error())
    expect_that(suppressWarnings(tmt_rm(dat_mst_5)), throws_error())
  })

# Checks einbauen
# Abfrage der Parameter, die geschätzt werden
# Abfrage der Struktur
# Checks checken

# expect_equal() is equal within small numerical tolerance?
# expect_identical() is exactly equal?
# expect_match() matches specified string or regular expression? expect_output() prints specified output?
# expect_message() displays specified message?
# expect_warning() displays specified warning?
# expect_error() throws specified error?
# expect_is() output inherits from certain class?
# expect_false() returns FALSE?
# expect_true() returns TRUE?
