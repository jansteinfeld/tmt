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

mstdesign_ms1 <- "
    B1 ~=~ c(i1, i2, i3, i4, i5)
    B2 ~~ c(i6, i7, i8, i9, i10)
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
mstdesign_ms2 <- "
    B1 =~ c(i1, i2, i3, i4; i5)
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
mstdesign_ms4 <- "
     B1 =~ c(i1, i2, i3, i4, i5)
     B2 =~ c(i6, i7, i8, i9, i10)
     B3 =~ c(i11, i12, i13, i14, i15)

     # define branches
     b1 := B2(0,2) + B10(0,5)
     b2 := B2(3,5) + B3(0,5)
 "
 
mstdesign_cum_new <- "
  B1 =~ c(i1, i2, i3, i4)
  B2 =~ c(i5, i6, i7)
  B3 =~ c(i8, i9, i10, i11)
  
  r1 = c(0.5,0.5,0.5,0.5)
  r2 = c(0.5,0.5,0.5,0.5)

  # define branches
  b1 := B2(r1) ++ B1
  b2 := B2(r2) ++ B3
 "

mstdesign_cum_old <- "
  B1 =~ c(i1, i2, i3, i4)
  B2 =~ c(i5, i6, i7)
  B3 =~ c(i8, i9, i10, i11)
  
  r1 = c(0.5,0.5,0.5,0.5)
  r2 = c(0.5,0.5,0.5,0.5)

  # define branches
  b1 := B2(r1) += B1
  b2 := B2(r2) += B3
 "

mstdesign_cum_det_old <- "
  M1  =~ paste0('i',21:30)
  M2  =~ paste0('i',11:20)
  M3  =~ paste0('i', 1:10)
  M4  =~ paste0('i',31:40)
  M5  =~ paste0('i',41:50)
  M6  =~ paste0('i',51:60)

  # define path
  p1 := M1(0, 5) += M2( 0,10) += M3
  p2 := M1(0, 5) += M2(11,15) += M4
  p3 := M1(6,10) += M5( 6,15) += M4
  p4 := M1(6,10) += M5(16,20) += M6
"

mstdesign_cum_det_new <- "
  M1  =~ paste0('i',21:30)
  M2  =~ paste0('i',11:20)
  M3  =~ paste0('i', 1:10)
  M4  =~ paste0('i',31:40)
  M5  =~ paste0('i',41:50)
  M6  =~ paste0('i',51:60)

  # define path
  p1 := M1(0, 5) ++ M2( 0,10) ++ M3
  p2 := M1(0, 5) ++ M2(11,15) ++ M4
  p3 := M1(6,10) ++ M5( 6,15) ++ M4
  p4 := M1(6,10) ++ M5(16,20) ++ M6
"

mstdesign_precon_det_new <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)

  # define constraints
  xcat == data$xcat

  # define path
  p1:= xcat(0,3) ++ B4(0,8) + B2(0,5) + B1
  p2:= xcat(0,3) ++ B4(0,8) + B2(6,10) + B3
  p3:= xcat(3,6) ++ B4(9,16) + B5(0,5) + B3
  p4:= xcat(3,6) ++ B4(9,16) + B5(6,10) + B5
"

mstdesign_precon_det_new_mixed <- " 
  B1 =~ paste0('i', 1:10) 
  B2 =~ paste0('i',11:20) 
  B3 =~ paste0('i',21:30) 
  B4 =~ paste0('i',31:40) 
  B5 =~ paste0('i',41:50) 
  B6 =~ paste0('i',51:60) 
  
  # define constraints 
  xcat == data$xcat 
  
  # define path 
  p1:= xcat(0,3) ++ B4(0,8) + B2(0,5) + B1 
  p2:= xcat(0,3) + B4(0,5) + B2(6,10) + B3 
  p3:= xcat(3,6) += B4(9,16) + B5(0,5) + B3 
  p4:= xcat(3,6) + B4(6,10) + B5(6,10) + B5 
"    

# mstdesign_precon_det_new_mixed_sep <- " 
#   B1 =~ paste0('i', 1:10) 
#   B2 =~ paste0('i',11:20) 
#   B3 =~ paste0('i',21:30) 
#   B4 =~ paste0('i',31:40) 
#   B5 =~ paste0('i',41:50) 
#   B6 =~ paste0('i',51:60) 
  
#   # define constraints 
#   xcat == data$xcat 

#   r1 = c(0,3)
#   r2 = c(3,6)
#   r3 = c(0,8)
#   r4 = c(0,5)
#   r5 = c(6,10)
#   r6 = c(9,16)

#   # define path 
#   p1:= xcat(r1) ++ B4(r3) + B2(r4) + B1 
#   p2:= xcat(r1) +  B4(r4) + B2(r5) + B3 
#   p3:= xcat(r2) += B4(r6) + B5(r4) + B3 
#   p4:= xcat(r2) +  B4(r5) + B5(r5) + B5 
# "    

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

mstdesign_precon_det <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)

  # define constraints
  xcat == data$xcat

  # define path
  p1:= xcat(0,3) += B4(0,8) + B2(0,5) + B1
  p2:= xcat(0,3) += B4(0,8) + B2(6,10) + B3
  p3:= xcat(3,6) += B4(9,16) + B5(0,5) + B3
  p4:= xcat(3,6) += B4(9,16) + B5(6,10) + B5
"

mstdesign_precon_det2 <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)

  # define constraints
  xcat == data$xcat

  # define path
  p1:= xcat(0,3) + B4(0,5) + B2(0,5) + B1
  p2:= xcat(0,3) + B4(0,5) + B2(6,10) + B3
  p3:= xcat(3,6) + B4(6,10) + B5(0,5) + B3
  p4:= xcat(3,6) + B4(6,10) + B5(6,10) + B5
"

mstdesign_precon_wrong1 <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)

  # define constraints
  xcat == data$xcat
  start == B1

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

mstdesign_precon_wrong2 <- "
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
  p1:= xcat(1:4) - B4(r1) += B2(r3) += B1
  p2:= xcat(1:4) - B4(r1) += B2(r4) += B3
  p3:= xcat(1:4) - B4(r2) += B5(r3) += B3
  p4:= xcat(1:4) - B4(r2) += B5(r4) += B6
"

# -----------------------------------------------------------------
context("test-tmt_mstdesign")
# -----------------------------------------------------------------
  test_that("tmt_mstdesign data structure", {
    tmp <- tmt_mstdesign(mstdesign = mstdesign)
    tmpstart <- tmt_mstdesign(mstdesign = mstdesign,options = c("design", "simulation", "modules", "items","start"))
    tmp_precon_cum <- tmt_mstdesign(mstdesign = mstdesign_precon)
    tmp_precon_det <- tmt_mstdesign(mstdesign = mstdesign_precon_det)

    expect_true(is.null(tmp$start))
    expect_false(is.null(tmpstart$start))
    expect_is(tmp,"list")
    expect_that(length(tmp), equals(6))
    expect_named(tmp, c("modules", "simulation", "design", "items", "preconditions", "start"))
    expect_is(tmp_precon_cum$preconditions,"list")
    expect_that(length(tmp_precon_cum$preconditions), equals(7))
    expect_named(tmp_precon_cum$preconditions, c("original","tmtd","modules","rules","paths","preconditions","precondition_matrix"))

    expect_is(tmp_precon_det$preconditions,"list")
    expect_that(length(tmp_precon_det$preconditions), equals(6))
    expect_named(tmp_precon_det$preconditions, c("original","tmtd","modules","paths","preconditions","precondition_matrix"))

    expect_identical(tmt_mstdesign(mstdesign_cum_old,"design")$design,tmt_mstdesign(mstdesign_cum_new,"design")$design)
    expect_identical(tmt_mstdesign(mstdesign_cum_old,"simulation")$simulation,tmt_mstdesign(mstdesign_cum_new,"simulation")$simulation)

    expect_identical(tmt_mstdesign(mstdesign_cum_det_old),tmt_mstdesign(mstdesign_cum_det_new))

    expect_identical(tmt_mstdesign(mstdesign_precon_det)$preconditions$modules,tmt_mstdesign(mstdesign_precon_det_new)$preconditions$modules)
    expect_identical(tmt_mstdesign(mstdesign_precon_det)$preconditions$preconditions,tmt_mstdesign(mstdesign_precon_det_new)$preconditions$preconditions)
    expect_identical(tmt_mstdesign(mstdesign_precon_det)$preconditions$precondition_matrix,tmt_mstdesign(mstdesign_precon_det_new)$preconditions$precondition_matrix)
    expect_false(isTRUE(all.equal(tmt_mstdesign(mstdesign_precon_det)$preconditions$paths, tmt_mstdesign(mstdesign_precon_det_new)$preconditions$paths)))

    expect_that(tmt_mstdesign(mstdesign_precon_det_new_mixed,"simulation")$simulation[[2]][,"maxSolved"],equals(c("8","5","16","10")))
    expect_that(tmt_mstdesign(mstdesign_precon_det_new_mixed,"simulation")$simulation[[2]][,"minSolved"],equals(c("0","0","9","6")))
    expect_that(tmt_mstdesign(mstdesign_precon_det_new_mixed)$preconditions$paths[6,"rule_1"], equals(c("(0,5)")))

    expect_identical(tmt_mstdesign(mstdesign,c("simulation","start"))$start,tmt_mstdesign(mstdesign,c("start"))$start)
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
    expect_that(tmt_mstdesign(mstdesign = mstdesign_precon_wrong1), throws_error())
    expect_that(tmt_mstdesign(mstdesign = mstdesign_precon_wrong2), throws_error())
  })

# -----------------------------------------------------------------
context("tmt_mstdesign check for expected s3 classes")
# -----------------------------------------------------------------
  test_that("check for expected S3 classes", {
    expect_s3_class(tmt_mstdesign(mstdesign_precon_det2)$simulation,"sequential")
    expect_s3_class(tmt_mstdesign(mstdesign,"simulation")$simulation,"sequential")
    expect_s3_class(tmt_mstdesign(mstdesign_precon,"simulation")$simulation,"cumulative")
    expect_s3_class(tmt_mstdesign(mstdesign_precon_det,"simulation")$simulation,"sequential")
    expect_s3_class(tmt_mstdesign(mstdesign_precon_det2,"simulation")$simulation,"sequential")
  })
  
