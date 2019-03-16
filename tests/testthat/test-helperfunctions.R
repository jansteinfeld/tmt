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

mstdesign_miss <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7, i8, i9, i10)
  B3 =~ c(i11, i12, i13, i14, i15)
  B4 =~ c(i16, i17, i18, i19, i20)
  B5 =~ c(i21, i22, i23, i24, i25)
  B6 =~ c(i26, i27, i28, i29, i30)

  # define starting Block
  Start1 == B4
  Start2 == B5

  # define branches
  b1 := Start(0,2) + B2(0,2) + B1(0,5)
  b2 := Start(0,2) + B2(3,5) + B3(0,5)
  b3 := Start(3,5) + B5(0,2) + B3(0,5)
  b4 := Start(3,5) + B5(3,5) + B6(0,5)
"

mstdesign_paste <- "
  B1 =~ paste0('i',1:5)
  B2 =~ paste0('i',6:10)
  B3 =~ paste0('i',11:15)
  B4 =~ paste0('i',16:20)
  B5 =~ paste0('i',21:25)
  B6 =~ paste0('i',26:30)

  # define starting Block
  Start == B4

  # define branches
  b1 := Start(0,2) + B2(0,2) + B1
  b2 := Start(0,2) + B2(3,5) + B3
  b3 := Start(3,5) + B5(0,2) + B3
  b4 := Start(3,5) + B5(3,5) + B6
"

  asciiart <- capture.output(tmt_ascii())
  # clean mstdesign input:
  tmt.syntax <- mstdesign
  tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
  tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
  tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
  tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
  tmtd <- unlist( strsplit(tmt.syntax, "\n") )
  tmtd <- tmtd[tmtd!=""]
  undefined <- which(!grepl("[~=:]", tmtd))

  if (length(undefined) > 0) {
    cat("The given multistage design is not correct specified:\n")
    print(tmtd[undefined])
    stop("Pleas correct the syntax and start again. \n")
  }

  # create list for the simulation function
  # -----------------------------------
  # number of blocks & branches
  n.blocks <- length(grep("=~",tmtd, fixed = TRUE))
  n.start <- length(grep("==",tmtd, fixed = TRUE))
  n.branches <- length(grep(":=",tmtd, fixed = TRUE))
  l.stages <- nchar(as.character(tmtd[grepl(":=",tmtd, fixed = TRUE)])) -
                nchar( gsub("\\+", "", tmtd[grepl(":=",tmtd, perl = TRUE)]))
  n.stages <- max(l.stages)
blocks <- tmt:::hfun.blocks(tmtd = tmtd, 
                  n.branches = n.branches, 
                  n.blocks = n.blocks, 
                  n.start = n.start)

# -----------------------------------------------------------------
context("test-helperfunctions tmt_mstdesign")
# -----------------------------------------------------------------
  test_that("hfun.blocks", {
    tmp <- tmt:::hfun.blocks(tmtd = tmtd, 
                  n.branches = n.branches, 
                  n.blocks = n.blocks, 
                  n.start = n.start)
    expect_that(nrow(tmp), equals(n.blocks + n.start))
  })

  test_that("hfun_simulation", {
    tmp <- tmt:::hfun.simulation(blocks = blocks,
                  tmtd = tmtd, 
                  n.stages = n.stages)
    expect_that(length(tmp), equals(max(l.stages) + 1))

  })

  test_that("hfun.design", {
    tmp <- tmt:::hfun.design(blocks = blocks,
                  tmtd = tmtd, 
                  n.branches = n.branches)
    expect_that(nrow(tmp), equals(n.branches))
    expect_that(ncol(tmp), equals(4))
    expect_named(tmp)
    expect_named(tmp, c("mst","minSolved","maxSolved","items"))
  })

  test_that("hfun.items", {
    tmp <- tmt:::hfun.items(blocks = blocks)
    expect_is(tmp,"character")
  })

  test_that("ascii art", {
    expect_is(asciiart,"character")
    expect_that(length(asciiart),equals(7))
  })


# -----------------------------------------------------------------
context("test-helperfunctions check data")
# -----------------------------------------------------------------
dat <- tmt:::sim.rm(100,10,1111)
dat2 <- dat
dat2[,1] <- 0
dat6 <- dat
colnames(dat6) <- paste0("i",1:ncol(dat6))

  test_that("data_check", {
    tmp <- tmt:::data_check(dat)
    tmp2 <- suppressWarnings(tmt:::data_check(dat2))
    tmp3 <- tmt:::data_check(dat6)
    expect_is(tmp,"list")
    expect_is(tmp2,"list")
    expect_that(tmp$status, equals(NULL))
    expect_failure(expect_that(dim(tmp2), equals(dim(dat2))))
    expect_that(dim(tmp$dat), equals(dim(dat)))
    expect_that(length(tmp2$status), equals(1))
    expect_that(data_check(dat[1,]), throws_error())
    expect_that(colnames(tmp3$dat), equals(colnames(dat6)))
  })

# -----------------------------------------------------------------
context("test-helperfunctions grafics")
# -----------------------------------------------------------------
  test_that("draw_ellipse", {
    set.seed(1111)
    tmp <- tmt:::draw_ellipse()
    expect_is(tmp,"data.frame")
    expect_that(nrow(tmp), equals(301))
    expect_named(tmp, c("x","y"))
  })

# -----------------------------------------------------------------
context("test-mstdesign paste and leaving last minSolved and maxSolved")
# -----------------------------------------------------------------
  test_that("paste", {
  expect_true(identical(tmt_mstdesign(mstdesign,"design")$design, tmt_mstdesign(mstdesign_paste,"design")$design))
  })
# .................................................................
# .................................................................
context("test-helperfunctions check errors")

dat5 <- dat4 <- dat
dat3 <- dat[1,]
dat4[,4] <- 1
dat5[,6] <- NA

expect_that(data_check(dat2), gives_warning())
expect_that(data_check(dat3), throws_error())
expect_that(data_check(dat4), gives_warning())
expect_that(data_check(dat5), gives_warning())

  test_that("un-specified blocks", {
    expect_that(tmt_mstdesign(mstdesign_miss), 
      throws_error())
  })

