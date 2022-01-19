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

mstdesign_miss <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7, i8, i9, i10)
  B3 =~ c(i11, i12, i13, i14, i15)
  B4 =~ c(i16, i17, i18, i19, i20)
  B5 =~ c(i21, i22, i23, i24, i25)
  B6 =~ c(i26, i27, i28, i29, i30)

  # define branches
  b1 := Start(0,2) + B2(0,2) + B1(0,5)
  b2 := Start(0,2) + B2(3,5) + B3(0,5)
  b3 := Start(3,5) + B5(0,2) + B3(0,5)
  b4 := Start(3,5) + B5(3,5) + B6(0,5)
"

mstdesign_miss2 <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7, i8, i9, i10)
  B3 =~ c(i11, i12, i13, i14, i15)
  B4 =~ c(i16, i17, i18, i19, i20)
  B5 =~ c(i21, i22, i23, i24, i25)
  B6 =~ c(i26, i27, i28, i29, i30)

  # define branches
  b1 := B4(0,2) + B2(0,2) + B1
  b2 := B4(0,2) + B2(3,5) + B3
  b3 := B4(3,5) + B5(0,2) + B3
  b4 := B4(3,5) + B5(3,5) + B7
"

mstdesign_paste <- "
  B1 =~ paste0('i',1:5)
  B2 =~ paste0('i',6:10)
  B3 =~ paste0('i',11:15)
  B4 =~ paste0('i',16:20)
  B5 =~ paste0('i',21:25)
  B6 =~ paste0('i',26:30)

  # define branches
  b1 := B4(0,2) + B2(0,2) + B1
  b2 := B4(0,2) + B2(3,5) + B3
  b3 := B4(3,5) + B5(0,2) + B3
  b4 := B4(3,5) + B5(3,5) + B6
"

mstdesign_kumulativ <- "
  M1  =~ paste0('i',21:30)
  M2  =~ paste0('i',11:20)
  M3  =~ paste0('i', 1:10)
  M4  =~ paste0('i',31:40)
  M5  =~ paste0('i',41:50)
  M6  =~ paste0('i',51:60)

  # define branches
  b1 := M1(0, 5) += M2( 0,10) += M3
  b2 := M1(0, 5) += M2(11,15) += M4
  b3 := M1(6,10) += M5( 6,15) += M4
  b4 := M1(6,10) += M5(16,20) += M6
"

mstdesign_kumulativ_miss <- "
  M1  =~ paste0('i',21:30)
  M2  =~ paste0('i',11:20)
  M3  =~ paste0('i', 1:10)
  M4  =~ paste0('i',31:40)
  M5  =~ paste0('i',41:50)
  M6  =~ paste0('i',51:60)
  M7  =~ paste0('i',61:65)

  # define starting module
  Start == M1

  # define branches
  b1 := M1(0, 5) += M2( 0,10) += M3
  b2 := M1(0, 5) += M2(11,15) += M4
  b3 := M1(6,10) += M5( 6,15) += M4
  b4 := M1(6,10) += M5(16,20) += M6
"

mod_preconditions1 <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)

  # define constraints
  xcat == data$xcat

  r1 = c(0,5)
  r2 = c(6,16)
  r3 = c(0,8)
  r4 = c(9,15)
  r5 = c(16,26)

  # define path  
  p1:= xcat(0,6) += B4(r1) += B2(r3) += B1
  p2:= xcat(0,6) += B4(r1) += B2(r4) += B3
  p3:= xcat(0,6) += B4(r2) += B5(r4) += B3
  p4:= xcat(0,6) += B4(r2) += B5(r5) += B5
"

mod_preconditions2 <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)
  B7 =~ paste0('i',61:70)
  B8 =~ paste0('i',71:80)
  B9 =~ paste0('i',81:90)

  # define constraints
  xcat == data$xcat

  # define routing criteria
  r1 = c(0.66,0.19,0.58,0.56,0.94,0.44,0.42,0.25,0.19,0.52)
  r2 = c(0.34,0.81,0.42,0.44,0.06,0.56,0.58,0.75,0.81,0.48)
  r3 = c(0.66,0.19,0.58,0.56,0.94,0.44,0.42,0.25,0.19,0.52)
  r4 = c(0.34,0.81,0.42,0.44,0.06,0.56,0.58,0.75,0.81,0.48)

  # define pathes
  p1 := xcat(1:3) + B3(r1) + B2(r3) + B1
  p2 := xcat(1:3) + B3(r1) + B2(r4) + B5
  p3 := xcat(1:3) + B3(r2) + B4(r3) + B5
  p4 := xcat(1:3) + B3(r2) + B4(r4) + B6
  p5 := xcat(3:6) + B7(r1) + B4(r3) + B5
  p6 := xcat(3:6) + B7(r1) + B4(r4) + B6
  p7 := xcat(3:6) + B7(r2) + B8(r3) + B6
  p8 := xcat(3:6) + B7(r2) + B8(r4) + B9
"

mod_preconditions3 <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)
  B7 =~ paste0('i',61:70)
  B8 =~ paste0('i',71:80)
  B9 =~ paste0('i',81:90)

  # define constraints
  xcat == data$xcat

  # define routing criteria
  r1 = c(0.5,0.5,0.5,0.66,0.19,0.58,0.56,0.94,0.44,0.42,0.25,0.19,0.52)
  r2 = c(0.5,0.5,0.5,0.34,0.81,0.42,0.44,0.06,0.56,0.58,0.75,0.81,0.48)
  r3 = c(0.66,0.19,0.58,0.56,0.94,0.44,0.42,0.25,0.19,0.52)
  r4 = c(0.34,0.81,0.42,0.44,0.06,0.56,0.58,0.75,0.81,0.48)

  # define pathes
  p1 := xcat(0:2) += B3(r1) + B2(r3) + B1
  p2 := xcat(0:2) += B3(r1) + B2(r4) + B5
  p3 := xcat(0:2) += B3(r2) + B4(r3) + B5
  p4 := xcat(0:2) += B3(r2) + B4(r4) + B6
  p5 := xcat(0:2) += B7(r1) + B4(r3) + B5
  p6 := xcat(0:2) += B7(r1) + B4(r4) + B6
  p7 := xcat(0:2) += B7(r2) + B8(r3) + B6
  p8 := xcat(0:2) += B7(r2) + B8(r4) + B9
"

mod_preconditions4 <- "
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

mstdesign_prob <- "
  B1 =~ paste0('i',1:5)
  B2 =~ paste0('i',6:10)
  B3 =~ paste0('i',11:15)
  B4 =~ paste0('i',16:20)
  B5 =~ paste0('i',21:25)
  B6 =~ paste0('i',26:30)

  # define routing criteria
  r1 = c(0.9,0.9,0.7,0.5,0.2,0.1)
  r2 = c(0.2,0.1,0.3,0.5,0.8,0.9)

  # define path
  p1 := B4(r1) + B2(r1) + B1
  p2 := B4(r1) + B2(r2) + B3
  p3 := B4(r2) + B5(r1) + B3
  p4 := B4(r2) + B5(r2) + B6
"

mstdesign_prob_cum <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)

  # define routing criteria
  r1 = c(0.90,0.85,0.80,0.90,0.85,0.80,0.75,0.70,0.65,0.60,0.55)
  r2 = c(0.10,0.15,0.20,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45)
  r3 = c(0.95,0.93,0.90,0.95,0.93,0.90,0.85,0.83,0.80,0.75,0.73,0.70,0.68,0.63,0.60,0.58,0.53,0.50,0.48,0.43,0.40)
  r4 = c(0.05,0.07,0.10,0.05,0.07,0.10,0.15,0.17,0.20,0.25,0.27,0.30,0.32,0.37,0.40,0.42,0.47,0.50,0.52,0.57,0.60)

  # define path
  p1:= B4(r1) += B2(r3) += B1
  p2:= B4(r1) += B2(r4) += B3
  p3:= B4(r2) += B5(r3) += B3
  p4:= B4(r2) += B5(r4) += B6
"

mstdesign_small_v1 <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7)
  B3 =~ c(i8, i9, i10, i11, i12)

  # define branches
  b1 := B2(0,0) + B1(0,5)
  b2 := B2(1,2) + B3(0,5)
 "

mstdesign_small_v2 <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7)
  B3 =~ c(i8, i9, i10, i11, i12)
  
  r1 = c(0,0)
  r2 = c(1,2)

  # define branches
  b1 := B2(r1) + B1
  b2 := B2(r2) + B3
"

mstdesign_small_v3 <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7)
  B3 =~ c(i8, i9, i10, i11, i12)

  # define branches
  b1 := B2(0.9,0.1,0.1) + B1
  b2 := B2(0.1,0.9,0.9) + B3
"

mstdesign_small_v4 <- "
  B1 =~ c(i1, i2, i3, i4, i5)
  B2 =~ c(i6, i7)
  B3 =~ c(i8, i9, i10, i11, i12)
  
  r1 = c(0.9,0.1,0.1)
  r2 = c(0.1,0.9,0.9)

  # define branches
  b1 := B2(r1) + B1
  b2 := B2(r2) + B3
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
  # number of modules & branches
  n.modules <- length(grep("=~",tmtd, fixed = TRUE))
  n.branches <- length(grep(":=",tmtd, fixed = TRUE))
  l.stages <- nchar(as.character(tmtd[grepl(":=",tmtd, fixed = TRUE)])) - nchar( gsub("\\+", "", tmtd[grepl(":=",tmtd, perl = TRUE)]))
  n.stages <- max(l.stages)
  modules <- tmt:::hfun.modules(tmtd = tmtd, 
                  n.branches = n.branches, 
                  n.modules = n.modules)
# cumulative version
  tmt.syntax <- mstdesign_kumulativ
  tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
  tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
  tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
  tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
  tmtd_cum <- unlist( strsplit(tmt.syntax, "\n") )
  tmtd_cum <- tmtd_cum[tmtd_cum!=""]
  n.modules_cum <- length(grep("=~",tmtd_cum, fixed = TRUE))
  n.branches_cum <- length(grep(":=",tmtd_cum, fixed = TRUE))
  l.stages_cum <- nchar(as.character(tmtd_cum[grepl(":=",tmtd_cum, fixed = TRUE)])) - nchar( gsub("\\+", "", tmtd_cum[grepl(":=",tmtd_cum, perl = TRUE)]))
  n.stages_cum <- max(l.stages_cum)

  modules_cum <- tmt:::hfun.modules(tmtd = tmtd_cum, 
                  n.branches = n.branches_cum, 
                  n.modules = n.modules_cum)

# -----------------------------------------------------------------
context("test-helperfunctions tmt_mstdesign")
# -----------------------------------------------------------------
  test_that("hfun.modules", {
    tmp <- tmt:::hfun.modules(tmtd = tmtd, 
                  n.branches = n.branches, 
                  n.modules = n.modules)
    expect_that(nrow(tmp), equals(n.modules))
  })
 test_that("hfun.modules_cum", {
    tmp <- tmt:::hfun.modules(tmtd = tmtd_cum, 
                  n.branches = n.branches_cum, 
                  n.modules = n.modules_cum)
    expect_that(nrow(tmp), equals(n.modules_cum))
  })

  test_that("hfun_simulation", {
    tmp <- tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = NULL)
    expect_that(length(tmp), equals(max(l.stages) + 1))
  })

  test_that("hfun_simulation_cum", {
    tmp <- tmt:::hfun.simulation(modules = modules_cum,
                  tmtd = tmtd_cum,
                  preconditions = NULL)
    expect_that(length(tmp), equals(max(l.stages_cum) + 1))
  })

  test_that("hfun_simulation probs in path", {
      tmtd[7] <- gsub("0,2","0.3,0.2",tmtd[7])
    expect_s3_class(tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = NULL), "sequential")
  })

  test_that("hfun_simulation probs", {
    tmt.syntax <- mstdesign_prob
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    # n.start <- length(grep("==",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))

    modules <- tmt:::hfun.modules(tmtd = tmtd, 
                              n.branches = n.branches, 
                              n.modules = n.modules)

    tmtd <- gsub("r1","1",tmtd)
    expect_that(tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = NULL), throws_error())


    tmt.syntax <- mstdesign_small_v3
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    # n.start <- length(grep("==",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))

    modules <- tmt:::hfun.modules(tmtd = tmtd, 
                  n.branches = n.branches, 
                  n.modules = n.modules)
    mod_small_v3 <- tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = NULL)

    tmt.syntax <- mstdesign_small_v4
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    # n.start <- length(grep("==",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))

    modules <- tmt:::hfun.modules(tmtd = tmtd, 
                  n.branches = n.branches, 
                  n.modules = n.modules)
    mod_small_v4 <- tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = NULL)
    mod_small_v4_design <- tmt:::hfun.design(modules = modules,
                  tmtd = tmtd,
                  n.branches = n.branches)
    expect_that(mod_small_v3,equals(mod_small_v4))
    expect_that(mod_small_v4_design[1,"probability"], equals(c("0.9,0.1,0.1;1,1,1,1,1,1")))
  })

test_that("hfun_simulation probs cum", {
    tmt.syntax <- mstdesign_prob_cum
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    # n.start <- length(grep("==",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))

    modules <- tmt:::hfun.modules(tmtd = tmtd, 
                              n.branches = n.branches, 
                              n.modules = n.modules)

    expect_s3_class(tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = NULL), "cumulative")
  })

  test_that("hfun_simulation probs precon cum", {
    tmt.syntax <- mod_preconditions1
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))
    n.preconditions <- length(grep("==",tmtd)) # grep only '~'
    c.preconditions <- grep("==",tmtd, value = TRUE)
    c.preconditions <- strsplit(c.preconditions,"==")
    v.preconditions <- sapply(c.preconditions,"[[",1)
    a.preconditions <- sapply(c.preconditions,"[[",2)
    av.preconditions <- list("name" = v.preconditions, "value" = a.preconditions)

    modules <- hfun.modules(tmtd = tmtd, 
                            n.branches = n.branches, 
                            n.modules = n.modules)

    preconditions <- hfun.preconditions(tmtd = tmtd, 
                              preconditions = av.preconditions, 
                              modules = modules)
    preconditions <- preconditions$precondition_matrix

    expect_s3_class(tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = preconditions), "cumulative")
      
  })

 test_that("hfun_simulation probs precon cum error", {
    tmt.syntax <- mod_preconditions3
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))
    n.preconditions <- length(grep("==",tmtd)) # grep only '~'
    c.preconditions <- grep("==",tmtd, value = TRUE)
    c.preconditions <- strsplit(c.preconditions,"==")
    v.preconditions <- sapply(c.preconditions,"[[",1)
    a.preconditions <- sapply(c.preconditions,"[[",2)
    av.preconditions <- list("name" = v.preconditions, "value" = a.preconditions)

    modules <- hfun.modules(tmtd = tmtd, 
                            n.branches = n.branches, 
                            n.modules = n.modules)

    preconditions <- hfun.preconditions(tmtd = tmtd, 
                            preconditions = av.preconditions, 
                            modules = modules)

    expect_that(hfun.preconditions(tmtd = gsub("r1=","1=",tmtd), 
                            preconditions = av.preconditions, 
                            modules = modules),throws_error())

    preconditions <- preconditions$precondition_matrix

    expect_s3_class(tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = preconditions), "sequential")
  })

test_that("hfun_simulation probs precon cum error", {
    tmt.syntax <- mod_preconditions4
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))
    n.preconditions <- length(grep("==",tmtd)) # grep only '~'
    c.preconditions <- grep("==",tmtd, value = TRUE)
    c.preconditions <- strsplit(c.preconditions,"==")
    v.preconditions <- sapply(c.preconditions,"[[",1)
    a.preconditions <- sapply(c.preconditions,"[[",2)
    av.preconditions <- list("name" = v.preconditions, "value" = a.preconditions)

    modules <- hfun.modules(tmtd = tmtd, 
                            n.branches = n.branches, 
                            n.modules = n.modules)

    preconditions <- hfun.preconditions(tmtd = tmtd, 
                            preconditions = av.preconditions, 
                            modules = modules)

    preconditions <- preconditions$precondition_matrix

    expect_s3_class(tmt:::hfun.simulation(modules = modules,
                  tmtd = tmtd,
                  preconditions = preconditions), "cumulative")

    expect_that(tmt:::hfun.simulation(modules = modules,
                  tmtd = gsub("r1=","r2=",tmtd),
                  preconditions = preconditions), throws_error())
    
    expect_that(tmt:::hfun.simulation(modules = modules,
                  tmtd = gsub("r1=","1=",tmtd),
                  preconditions = preconditions), throws_error())

  })

  test_that("hfun.design", {
    tmp <- tmt:::hfun.design(modules = modules,
                  tmtd = tmtd, 
                  n.branches = n.branches)
    expect_that(nrow(tmp), equals(n.branches))
    expect_that(ncol(tmp), equals(7))
    expect_named(tmp)
    expect_named(tmp, c("mst","minSolved","maxSolved","items","minSolved_stage","maxSolved_stage","probability"))

    tmt.syntax <- mstdesign_small_v1
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))
    
    modules <- tmt:::hfun.modules(tmtd = tmtd, 
                            n.branches = n.branches, 
                            n.modules = n.modules)
    design_v1 <- tmt:::hfun.design(modules = modules,
                    tmtd = tmtd, 
                    n.branches = n.branches)
    
    tmt.syntax <- mstdesign_small_v2
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))
    modules <- tmt:::hfun.modules(tmtd = tmtd, 
                            n.branches = n.branches, 
                            n.modules = n.modules)

    design_v2 <- tmt:::hfun.design(modules = modules,
                    tmtd = tmtd, 
                    n.branches = n.branches)
    expect_that(design_v1, equals(design_v2))
    expect_that(tmt:::hfun.design(modules = modules,
                    tmtd = gsub("r1","1",tmtd), 
                    n.branches = n.branches),throws_error())


    tmt.syntax <- mod_preconditions2
    tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
    tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
    tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
    tmtd <- unlist( strsplit(tmt.syntax, "\n") )
    tmtd <- tmtd[tmtd!=""]
    n.modules <- length(grep("=~",tmtd, fixed = TRUE))
    n.branches <- length(grep(":=",tmtd, fixed = TRUE))

    modules <- tmt:::hfun.modules(tmtd = tmtd, 
                            n.branches = n.branches, 
                            n.modules = n.modules)
    
    design_v3 <- tmt:::hfun.design(modules = modules,
                    tmtd = tmtd, 
                    n.branches = n.branches)
    expect_that(nrow(design_v3),equals(8))
  })

  test_that("hfun.design_cum", {
    tmp <- tmt:::hfun.design(modules = modules_cum,
                  tmtd = tmtd_cum, 
                  n.branches = n.branches_cum)
    expect_that(nrow(tmp), equals(n.branches_cum))
    expect_that(ncol(tmp), equals(7))
    expect_named(tmp)
    expect_named(tmp, c("mst","minSolved","maxSolved","items","minSolved_stage","maxSolved_stage","probability"))
  })


  test_that("hfun.items", {
    tmp <- tmt:::hfun.items(modules = modules)
    expect_is(tmp,"character")
  })

  test_that("hfun.items_cum", {
    tmp <- tmt:::hfun.items(modules = modules_cum)
    expect_is(tmp,"character")
  })

  test_that("ascii art", {
    expect_is(asciiart,"character")
    expect_that(length(asciiart),equals(6))
  })


# -----------------------------------------------------------------
context("test-helperfunctions check data")
# -----------------------------------------------------------------
dat <- tmt:::sim.rm(100,10,1111)
dat2 <- dat
dat2[,1] <- 0
dat6 <- dat
colnames(dat6) <- paste0("i",seq_len(ncol(dat6)))

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
expect_that(tmt_mstdesign(mstdesign_miss2,"design"), throws_error())
  test_that("un-specified modules", {
    expect_that(tmt_mstdesign(mstdesign_miss), 
      throws_error())
  })

expect_that(length(chunks(seq(10),3)),equals(3))

test_that("test convert function",{
  expect_that(nrow(convert(list(500,500))),equals(2))
  expect_true(is.matrix(convert(c("n" = 500, "mean" = 0, "sd" = 1))))
  expect_that(nrow(convert(list("n" = c(500,500), "mean" = c(0,0), "sd" = c(1,1)))),equals(2))
  expect_that(nrow(convert(list(c(500,0,1),c(500,0,1)))),equals(2))
  expect_that(nrow(convert(matrix(rbind(c("n" = 500, "mean" = 0, "sd" = 1), c("n" = 500, "mean" = 0, "sd" = 1)), ncol = 3))),equals(2))
  expect_that(nrow(convert(500)),equals(1))
  expect_that(nrow(convert(c(500,500))),equals(2))
  expect_that(nrow(convert(c(500,0,1))),equals(1))  

  # errors
  expect_that(convert(list("nn" = c(500,500), "mean" = c(0,0), "sd" = c(1,1))),throws_error())
  expect_that(convert(matrix(rbind(c(500,0,1,2), c(500,0,1,2)), ncol = 4)),throws_error())
  expect_that(convert(c(500,1)),throws_error())

})


# -----------------------------------------------------------------
context("test-helperfunctions additoinal")
# -----------------------------------------------------------------
test_that("test additional functions",{
  thres <- rbind(
    "1" = c(0.800,0.735,0.673,0.616,0.563,0.513,0.468,0.427,0.389,0.356,0.327,0.301,0.280,0.263,0.249,0.240), 
    "2" = c(0.900,0.871,0.841,0.812,0.783,0.753,0.724,0.695,0.665,0.636,0.607,0.577,0.548,0.519,0.489,0.460), 
    "3" = c(1.000,0.998,0.993,0.986,0.977,0.965,0.951,0.934,0.915,0.894,0.870,0.844,0.815,0.784,0.751,0.715)
    )
  tthres <- t(thres)
  thres_error <- thres
  thres_error[3:1,1] <- thres_error[1:3,1]
    
    output1 <- capture_output_lines(thresholds_to_probs(thres))
    output2 <- thresholds_to_probs(thres, opts="")
    output3 <- thresholds_to_probs(tthres, opts="", row = FALSE)
    
    expect_that(length(output1),equals(8))
    expect_that(ncol(output2),equals(3))
    expect_that(nrow(output2),equals(16))
    expect_that(ncol(output2),equals(3))
    expect_that(nrow(output2),equals(16))
    expect_error(thresholds_to_probs(thres_error, opts=""))
    expect_that(output2,equals(output3))
    
    thres_row <- thres[1,,drop=FALSE]
    
    output4 <- thresholds_to_probs(thres_row, opts="", row = TRUE)
    output5 <- capture_output_lines(thresholds_to_probs(thres_row, opts="mstdesign", row = TRUE))
    output6 <- thresholds_to_probs(thres_row, opts="", row = FALSE)

    expect_that(sum(output4-thres_row),equals(0))
    expect_true(is.matrix(output4))
    expect_that(length(output5),equals(21))
    expect_true(is.matrix(output6))
    expect_that(ncol(output6),equals(ncol(thres_row)))

    # tests for add letters function
    expect_that(length(add_letters(10)),equals(10))
    expect_that(length(add_letters(30)),equals(30))



    # function to split
    branches <- tmtd[grepl(":=", tmtd, fixed = TRUE)]
    branches <- strsplit_storing(branches,":=")
    colnames(branches) <- c("path","operator_0", "path_original")
    out1 <- strsplit_storing(variable = branches,
                    split = c("~","+=","+"),
                    cols = "path_original", 
                    new_names = c("module","operator")
                    )

    expect_that(nrow(out1),equals(4))
    expect_that(ncol(out1),equals(7))

    expect_that(strsplit_storing(variable = branches,
                    split = c("~","+=","+"),
                    cols = c("operator_o","path_original"), 
                    new_names = c("module","operator")
                    ),throws_error())

    expect_that(strsplit_storing(branches,":="),throws_error())
})


test_that("test function precon_sim",{
  test1 <- precon_sim(ppar = 500, precon = c(1,10,1))
  expect_named(test1)  
  expect_that(length(test1),equals(2))
  # expect_named(names(test1), c("perspar","preconpar"), ignore.order = TRUE, ignore.case = TRUE)
  expect_true(all.equal(names(test1), c("perspar","preconpar")))
  set.seed(100);theta <- rnorm(500)
  # Seed testen: 2020-05-10
  test1 <- precon_sim(ppar = 500, precon = c(1,10,1), seed=100)
  test2 <- precon_sim(ppar = 500, precon = c(1,10,1), seed=100)
  test3 <- precon_sim(ppar = list(theta), precon = c(1,10,1), seed = 100)
  test4 <- precon_sim(ppar = list(500), precon = c(1,10,1), seed = 100)
  expect_true(all.equal(test1$perspar,test2$perspar))
  expect_true(all.equal(test1$perspar,test3$perspar))
  expect_true(all.equal(test1$perspar,test4$perspar))
  expect_true(all.equal(test1$preconpar,test2$preconpar))

  test1 <- precon_sim(ppar = 500, precon = c(1,10,1),seed=100)
  test2 <- precon_sim(ppar = 500, precon = c(1,10,1),seed=101)
  expect_false(isTRUE(all.equal(test1$perspar,test2$perspar)))
  expect_false(isTRUE(all.equal(test1$preconpar,test2$preconpar)))
  

  # warnings
  expect_warning(precon_sim(ppar = c(500,500), precon = c(1,10,1),seed=100))
  expect_warning(precon_sim(ppar = c(500,500), precon = c(1,10,1)))
  expect_warning(precon_sim(ppar = c(500,500), precon = c(1,10,0.7)))
  expect_warning(precon_sim(ppar = c(500,500), precon = c("min"=1,"max"=10,"r" = 0.8)))
  expect_warning(precon_sim(ppar = c(500,500), precon = list(c(1,10,0.7))))

  # errors 
  expect_error(precon_sim(ppar = c(500,500), precon = matrix(
                                            rbind(
                                              c("min" = 0, "max" = 10, "r" = 0.8),
                                              c("min" = 0, "max" = 10,"r" = 0.8), 
                                              c("min" = 0, "max" = 10,"r" = 0.8)), 
                                              ncol = 3
                                            )))
  expect_error(precon_sim(ppar = c(500,500), precon = list(
                                            rnorm(500),
                                            sample(0:2,500,replace=TRUE)
                                            )))
  expect_error(precon_sim(ppar = c(500), precon = c(1,10,2)))
  expect_error(precon_sim(ppar = c(500), precon = c(0.5,10,1)))
  expect_error(precon_sim(ppar = c(500), precon = rnorm(0,1,500)))
  expect_error(precon_sim(ppar = c(500,500), precon = list(
                                            c("min" = 0.5, "max" = 5, "r" = 0.8),
                                            c("min" = 6, "max" = 10,"r" = 0.4)
                                            )))


  # test for categories
  test0 <- precon_sim(ppar = c(500,500), precon = list(
                                            sample(0:2,500,replace=TRUE),
                                            sample(0:2,500,replace=TRUE)
                                            ))
  
  test1 <- precon_sim(ppar = c(500,500), precon = list(
                                            c("min" = 0, "max" = 5, "r" = 0.8),
                                            c("min" = 6, "max" = 10,"r" = 0.4)
                                            ))
  test2 <- precon_sim(ppar = c(500,500), precon = list(
                                            "min" = c(0,0), 
                                            "max" = c(10,10), 
                                            "r" = c(0.8,0.6)
                                          ))
  test3 <- precon_sim(ppar = c(500,500), precon = matrix(
                                            rbind(
                                              c("min" = 0, "max" = 10, "r" = 0.8),
                                              c("min" = 0, "max" = 10,"r" = 0.8)), 
                                              ncol = 3
                                            ))
  test4 <- precon_sim(ppar = c("N1" = 500, "N2" = 500), precon = list(
                                            "min" = c(0,0), 
                                            "max" = c(10,10), 
                                            "r" = c(0.8,0.6)
                                          ))
  
  expect_that(sum(lengths(test0)),equals(4))
  expect_that(c(min(test1$preconpar[[1]]),max(test1$preconpar[[1]])),equals(c(0,5)))
  expect_that(c(min(test1$preconpar[[2]]),max(test1$preconpar[[2]])),equals(c(6,10)))
  expect_that(c(min(test2$preconpar[[1]]),max(test2$preconpar[[1]])),equals(c(0,10)))
  expect_that(c(min(test2$preconpar[[2]]),max(test2$preconpar[[2]])),equals(c(0,10)))
  expect_that(c(min(test3$preconpar[[1]]),max(test3$preconpar[[1]])),equals(c(0,10)))
  expect_that(c(min(test3$preconpar[[2]]),max(test3$preconpar[[2]])),equals(c(0,10)))
  expect_that(c(min(test4$preconpar[[1]]),max(test4$preconpar[[1]])),equals(c(0,10)))
  expect_that(c(min(test4$preconpar[[2]]),max(test4$preconpar[[2]])),equals(c(0,10)))
  
  # test for correlation
  expect_equal(cor(test1$perspar[[1]],test1$preconpar[[1]]),0.8,tolerance=0.1)
  expect_equal(cor(test1$perspar[[2]],test1$preconpar[[2]]),0.4,tolerance=0.1)
  expect_equal(cor(test2$perspar[[1]],test2$preconpar[[1]]),0.8,tolerance=0.1)
  expect_equal(cor(test2$perspar[[2]],test2$preconpar[[2]]),0.6,tolerance=0.1)
  expect_equal(cor(test3$perspar[[1]],test3$preconpar[[1]]),0.8,tolerance=0.1)
  expect_equal(cor(test3$perspar[[2]],test3$preconpar[[2]]),0.8,tolerance=0.1)
  
  # test for structure
  expect_that(sum(lengths(test1)),equals(4))
  expect_that(sum(lengths(test2)),equals(4))
  expect_that(sum(lengths(test3)),equals(4))
})

