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
# Vorbereitung
items <- seq(-2,2,length.out=30)
persons <- 500

dat <- tmt:::sim.rm(persons,items,c(1111,1112))
datna <- dat
datna[sample(seq_len(length(datna)),50,replace = FALSE)] <- NA
datrm_1 <- tmt_rm(dat, optimization="optim")


names(items) <- c(paste0("i",1:30))

dat_mst <- tmt_sim(mstdesign = mstdesign,
			items = items,
			persons = persons,
			seed = c(1111,1112))
datrm_1p <- tmt_rm(dat, optimization="optim")
datrm_2p <- tmt_rm(dat_mst$data,mstdesign=mstdesign, optimization="optim")

datrm_3p <- tmt_rm(dat, sum0 = FALSE)
datrm_4p <- tmt_rm(dat_mst$data,mstdesign=mstdesign, sum0 = FALSE)

datrm_5p <- tmt_rm(dat, se = FALSE)
datrm_6p <- tmt_rm(dat_mst$data,mstdesign=mstdesign, se = FALSE)

datlrt_1p <- tmt_lrtest(datrm_1p, optimization="optim")
datlrt_2p <- tmt_lrtest(datrm_2p, optimization="optim")

datrm_1p.out <- capture.output(print(datrm_1p))
datrm_2p.out <- capture.output(print(datrm_2p))
datrm_3p.out <- capture.output(print(datrm_3p))
datrm_4p.out <- capture.output(print(datrm_4p))

datrm_5p.out <- capture.output(print(datrm_5p))
datrm_6p.out <- capture.output(print(datrm_6p))

datlrt_1p.out <- capture.output(print(datlrt_1p))
datlrt_2p.out <- capture.output(print(datlrt_2p))

# -----------------------------------------------------------------
context("test-print functions raschmodel")
# -----------------------------------------------------------------
test_that("print functions raschmodel nmst", {
  expect_that(datrm_1p.out[2], equals(c("Results of Rasch model (nmst) estimation: ")))
  expect_that(datrm_1p.out[13], equals(c("item parameters (difficulty): ")))
  expect_that(datrm_3p.out[2], equals(c("Results of Rasch model (nmst) estimation: ")))
  expect_that(datrm_3p.out[13], equals(c("item parameters (difficulty): ")))
  expect_that(datrm_5p.out[2], equals(c("Results of Rasch model (nmst) estimation: ")))
  expect_that(datrm_5p.out[13], equals(c("item parameters (difficulty): ")))
  expect_that(grep("Std. Error", datrm_3p.out[16]), equals(1))

})

test_that("print functions raschmodel mst", {
  expect_that(datrm_2p.out[2], equals(c("Results of Rasch model (mst) estimation: ")))
  expect_that(datrm_2p.out[12], equals(c("used mstdesign:")))
  expect_that(datrm_4p.out[2], equals(c("Results of Rasch model (mst) estimation: ")))
  expect_that(datrm_4p.out[12], equals(c("used mstdesign:")))
  expect_that(datrm_6p.out[2], equals(c("Results of Rasch model (mst) estimation: ")))
  expect_that(datrm_6p.out[12], equals(c("used mstdesign:")))
  expect_false(length(grep("Std. Error",datrm_4p.out))==0)
})


# -----------------------------------------------------------------
context("test-print functions lrtest")
# -----------------------------------------------------------------
test_that("print functions lrtest nmst", {
  expect_that(datlrt_1p.out[2], 
  	equals(c("Likelihood ratio test results of Rasch model (nmst) estimation: ")))
})

test_that("print functions lrtest mst", {
  expect_that(datlrt_2p.out[2], 
    equals(c("Likelihood ratio test results of Rasch model (mst) estimation: ")))
})
