mstdesign <- "
    B1 =~ c(i1, i2, i3, i4, i5)
    B2 =~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)
    B4 =~ c(i16, i17, i18, i19, i20)
    B5 =~ c(i21, i22, i23, i24, i25)
    B6 =~ c(i26, i27, i28, i29, i30)

    # define starting module
    Start == B4

    # define branches
    b1 := Start(0,2) + B2(0,2) + B1(0,5)
    b2 := Start(0,2) + B2(3,5) + B3(0,5)
    b3 := Start(3,5) + B5(0,2) + B3(0,5)
    b4 := Start(3,5) + B5(3,5) + B6(0,5)
  "

mstdesign_ms1 <- "
    B1 ~=~ c(i1, i2, i3, i4, i5)
    B2 ~~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)
    B4 =~ c(i16, i17, i18, i19, i20)
    B5 =~ c(i21, i22, i23, i24, i25)
    B6 =~ c(i26, i27, i28, i29, i30)

    # define starting module
    Start == B4

    # define branches
    b1 := Start(0,2) + B2(0,2) + B1(0,5)
    b2 := Start(0,2) + B2(3,5) + B3(0,5)
    b3 := Start(3,5) + B5(0,2) + B3(0,5)
    b4 := Start(3,5) + B5(3,5) + B6(0,5)
  "
mstdesign_ms2 <- "
    B1 =~ c(i1, i2, i3, i4; i5)
    B2 =~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)
    B4 =~ c(i16, i17, i18, i19, i20)
    B5 =~ c(i21, i22, i23, i24, i25)
    B6 =~ c(i26, i27, i28, i29, i30)

    # define starting module
    Start == B4

    # define branches
    b1 := Start(0,2) + B2(0,2) + B1(0,5)
    b2 := Start(0,2) + B2(3,5) + B3(0,5)
    b3 := Start(3,5) + B5(0,2) + B3(0,5)
    b4 := Start(3,5) + B5(3,5) + B6(0,5)
  "
mstdesign_ms4 <- "
     B1 =~ c(i1, i2, i3, i4, i5)
     B2 =~ c(i6, i7, i8, i9, i10)
     B3 =~ c(i11, i12, i13, i14, i15)

     # define starting module
     Start == B2

     # define branches
     b1 := Start(0,2) + B10(0,5)
     b2 := Start(3,5) + B3(0,5)
 "
# -----------------------------------------------------------------
context("test-tmt_mstdesign")
# -----------------------------------------------------------------
  test_that("tmt_mstdesign data structure", {
    tmp <- tmt_mstdesign(mstdesign = mstdesign)
    expect_is(tmp,"list")
    expect_that(length(tmp), equals(5))
    expect_named(tmp, c("modules", "simulation", "design", "items","start"))
  })

# -----------------------------------------------------------------
context("test-helperfunctions check warnings")
# -----------------------------------------------------------------
	

# -----------------------------------------------------------------
context("test-helperfunctions check errors")
# -----------------------------------------------------------------
  test_that("tmt_mstdesign wrong sign design error", {
    expect_that(tmt_mstdesign(mstdesign = mstdesign_ms1), throws_error())
    expect_that(tmt_mstdesign(mstdesign = mstdesign_ms4), throws_error())
  })

  test_that("tmt_mstdesign semicolon instead of comma", {
    expect_that(tmt_mstdesign(mstdesign = mstdesign_ms2), throws_error())
  })

  test_that("tmt_mstdesign input check", {
    expect_that(tmt_mstdesign(mstdesign = mstdesign, options = ""), throws_error())
    expect_that(tmt_mstdesign(mstdesign = mstdesign, options = "ITEMS"), throws_error())
    expect_that(tmt_mstdesign(mstdesign = matrix(0,nrow=10,ncol=10)), throws_error())
  })
