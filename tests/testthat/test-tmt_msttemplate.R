formula1 <- "start(start) += S1(B1,B2,B3) += S2(B4,B5,B6,B7)"
formula2 <- "nativ(no,yes) ~ education(low,medium,heigh) ~ 
              CBM(3:6) += S1(B1,B2,B3) += S2(B4,B5,B6,B7)"
formula3 <- "start(1:2) += B1(1:3) += S2(B4,B5,B6,B7)"
formula4 <- "start(1:4) ~ start2(low,medium,heigh)"
formula5 <- "start(start) ~ S1"

test_that("test function to create mstdesign",{
  out1 <- capture.output(tmt_msttemplate(formula1, full = TRUE, eval = TRUE))
  out2 <- capture.output(tmt_msttemplate(formula1, full = TRUE, eval = FALSE))
  out3 <- capture.output(tmt_msttemplate(formula2, full = TRUE, eval = TRUE))
  out4 <- capture.output(tmt_msttemplate(formula2, full = TRUE, eval = FALSE))
  out5 <- capture.output(tmt_msttemplate(formula2, full = FALSE, eval = FALSE))

  expect_that(out1,equals(out2))
  expect_false(isTRUE(all.equal(out3,out4)))
  expect_that(length(out1),equals(39))
  expect_that(length(out2),equals(39))
  expect_that(length(out3),equals(315))
  expect_that(length(out4),equals(99))

  out6 <- capture.output(tmt_msttemplate(formula = formula3, full = TRUE, eval = TRUE))
  out7 <- capture.output(tmt_msttemplate(formula = formula4, full = TRUE, eval = TRUE))
  out8 <- capture.output(tmt_msttemplate(formula = formula4, full = FALSE, eval = TRUE))
  out9 <- capture.output(tmt_msttemplate(formula = formula5, full = FALSE, eval = TRUE))
  
  expect_that(length(out6),equals(48))
  expect_that(length(out7),equals(28))
  expect_that(length(out8),equals(28))
  expect_that(length(out9),equals(17))
})

test_that("test function to create matrix",{  
  expect_that(colnames(tmt_msttemplate()),equals(c("mst", "minSolved", "maxSolved", "items", "minSolved_stage", "maxSolved_stage", "probability")))
  expect_that(nrow(tmt_msttemplate()),equals(2))
  expect_that(ncol(tmt_msttemplate()),equals(7))
})
