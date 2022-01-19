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


mstdesign_max <- "
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
    b4 := B4(3,6) + B5(3,5) + B6(0,5)
  "

 mstdesign_start <- "
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
    b5 := B5(0,2) + B2(0,2) + B1(0,5)
    b6 := B5(0,2) + B2(3,5) + B3(0,5)
    b7 := B5(3,5) + B5(0,2) + B3(0,5)
    b8 := B5(3,5) + B5(3,5) + B6(0,5)
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

mstdesign_precon2 <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)

  # define constraints
  xcat == data$xcat
  ycat == data$ycat

  # define path
  r1 = c(0,10)
  r2 = c(11,21)
  r3 = c(0,16)
  r4 = c(17,26)
  r5 = c(11,16)
  r6 = c(17,31)

  # define path
  p1:= ycat(1:3) += xcat(0,3) += B4(r1) += B2(r3) += B1
  p2:= ycat(1:3) += xcat(0,3) += B4(r1) += B2(r4) += B3
  p3:= ycat(3:5) += xcat(3,6) += B4(r2) += B5(r5) += B3
  p4:= ycat(3:5) += xcat(3,6) += B4(r2) += B5(r6) += B5
"

mstdesign_probcum <- "
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

mstdesign_max_v2  <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)
  
  # define path
  p1:= B4(0,5) + B2(0,0) + B1
  p2:= B4(0,5) + B2(1,10) + B3
  p3:= B4(6,10) + B5(0,5) + B3
  p4:= B4(6,10) + B5(6,10) + B6
"

mstdesign_max_v3  <- "
  B1 =~ paste0('i', 1:10)
  B2 =~ paste0('i',11:20)
  B3 =~ paste0('i',21:30)
  B4 =~ paste0('i',31:40)
  B5 =~ paste0('i',41:50)
  B6 =~ paste0('i',51:60)
  
  # define path
  p1:= B4(1,9) + B2(0,0) + B1
  p2:= B4(1,9) + B2(1,10) + B3
  p3:= B4(10,10) + B5(0,5) + B3
  p4:= B4(10,10) + B5(6,10) + B6
"

# -----------------------------------------------------------------
context("test-tmt_sim")
# -----------------------------------------------------------------
  test_that("tmt_sim data structure", {
    items <- seq(-2,2, length.out = 30)
    names(items) <- c(paste0("i", 1:30))
    items_3 <- seq(-2,2,length.out=length(tmt_mstdesign(mstdesign_max_v2,"items")$items))
    names(items_3) <- c(paste0("i", seq_along(items_3)))
    set.seed(1111)
    persons <- rnorm(500, mean = 0, sd = 1)
    
      tmp <- tmt_sim(mstdesign = mstdesign,
              items = items,
              persons = 500,
              seed = 1111,
              seed = 1111)
      tmp2 <- tmt_sim(mstdesign = mstdesign,
              items = items,
              persons = persons,
              seed = 1111)
      tmp3 <- tmt_sim(mstdesign = mstdesign_start,
              items = items,
              persons = list(persons,persons),
              seed = 1111)
      tmp4 <- tmt_sim(mstdesign = mstdesign_max_v2,
              items = items_3,
              persons = persons,
              seed = 1111)
      tmp5 <- tmt_sim(mstdesign = mstdesign_max_v3,
              items = items_3,
              persons = 10000,
              seed = 1111)

    expect_is(tmp,"list")
    expect_that(length(tmp), equals(6))
    expect_that(tmp2$persons-tmp$persons, equals(rep(0,500)))
    expect_named(tmp, c("data", "data_mst", "persons", "mstdesign","preconditions", "preconpar"))
    expect_s3_class(tmp,"mstdesign") 

    expect_is(tmp2,"list")
    expect_that(length(tmp2), equals(6))
    expect_named(tmp2, c("data", "data_mst", "persons", "mstdesign","preconditions", "preconpar"))
    expect_s3_class(tmp2,"mstdesign") 

    expect_is(tmp3,"list")
    expect_that(length(tmp3), equals(6))
    expect_named(tmp3, c("data", "data_mst", "persons", "mstdesign","preconditions", "preconpar"))
    expect_s3_class(tmp3,"mstdesign") 

    expect_is(tmp4,"list")
    expect_that(length(tmp4), equals(6))
    expect_named(tmp4, c("data", "data_mst", "persons", "mstdesign","preconditions", "preconpar"))
    expect_s3_class(tmp4,"mstdesign") 
    expect_that(sum(rowSums(tmp4$data_mst[tmp4$data_mst$branching=="B4-B2-B1",paste0('i',11:20)]) ),equals(0))
  })

# -----------------------------------------------------------------
context("test-tmt_sim errors and warnings")
# -----------------------------------------------------------------
test_that("tmt_sim several designs", {
  items <- seq(-2,2,length.out=30)
  names(items) <- c(paste0("i",1:30))

  items_error <- seq(-2,2,length.out=37)
  names(items_error) <- c(paste0("i",1:37))
  persons_error <- list(rnorm(500),rnorm(500))

  items_names <- items
  names(items_names)[1] <- "ii1"
  
  expect_that(tmt_sim(mstdesign=NULL,
        items = seq(-2,2,by=0.5),
        persons = 500), throws_error())

  # expect_that(tmt_sim(mstdesign = mstdesign_start,
  #             items = items,
  #             persons = list(500,500)), throws_error())

  expect_that(tmt_sim(mstdesign=mstdesign,
        items = NULL,
        persons = 500), throws_error())

  expect_that(tmt_sim(mstdesign=mstdesign,
        items = seq(-2,2,by=0.5),
        persons = NULL), throws_error())
  
  expect_that(tmt_sim(mstdesign=mstdesign,
        items = seq(-2,2, length.out = 30),
        persons = 500), gives_warning())

  expect_that(tmt_sim(mstdesign=mstdesign,
        items = items_error,
        persons = 500), throws_error())

expect_that(tmt_sim(mstdesign=mstdesign,
        items = items_names,
        persons = 500), throws_error())

expect_that(tmt_sim(mstdesign=mstdesign,
        items = items,
        persons = persons_error), gives_warning())

  expect_that(tmt_sim(mstdesign=mstdesign,
        items = items,
        persons = NULL,
        mean = 0,
        sd = 1), gives_warning())

  expect_that(tmt_sim(mstdesign=mstdesign,
        items = seq(-2,2,by=0.5),
        persons = 500), throws_error())

  items <- seq(-2,2,length.out=30)
  names(items) <- c(paste0("i",1:29),"ii30")

  expect_that(tmt_sim(mstdesign=mstdesign,
        items = items,
        persons = 500), throws_error())

  items <- seq(-2,2,length.out=30)
  names(items) <- c(paste0("i",1:30))
  expect_that(tmt_sim(mstdesign = mstdesign_max,
      items = items,
      persons = 500), throws_error())
  
  expect_that(tmt_sim(mstdesign = mstdesign_probcum,
      items = items,
      persons = 500), gives_warning())
  
  
})
# -----------------------------------------------------------------
context("test-tmt_sim warnings")
# -----------------------------------------------------------------
 test_that("tmt_sim data structure", {
    items <- seq(-2,2,length.out=30)
    names(items) <- c(paste0("i",1:30))
    set.seed(1111)
    expect_that(tmt_sim(mstdesign = mstdesign_start,
            items = items,
            persons = 500), throws_error())
    # expect_that(tmt_sim(mstdesign = mstdesign_start,
    #     items = items,
    #     persons = c(500,500)), gives_warning())

   expect_that(tmt_sim(mstdesign = mstdesign_start,
        items = items,
        persons = c(500,500,500)), throws_error())
})

# -----------------------------------------------------------------
 test_that("tmt_sim preconditions", {
  # check preconditions
  items_precon <- seq(-2,2, length.out = 60)
  names(items_precon) <- c(paste0("i", 1:60))
  expect_that(tmt_sim(mstdesign = mstdesign_precon,
    items = items_precon,
    persons = 500), throws_error())

  items <- seq(-2,2,length.out=30)
  names(items) <- c(paste0("i",1:30))
   
  expect_that(tmt_sim(mstdesign = mstdesign,
    items = items,
    preconditions = 0.3,
    persons = 500), gives_warning())

  expect_that(tmt_sim(mstdesign = mstdesign_precon2,
      items = items_precon,
      preconditions = 0.3,
      persons = 500), gives_warning())
  
  expect_that(tmt_sim(mstdesign = mstdesign_precon2,
    items = items_precon,
    preconditions = c(0.3,0.3,0.3),
    persons = c(500, 500)), gives_warning())

  expect_that(tmt_sim(mstdesign = mstdesign_precon2,
    items = items_precon,
    preconditions = 0.3,
    persons = c(500, 500)), gives_warning())

 expect_warning(
    expect_error(tmt_sim(mstdesign = mstdesign_precon2,
      items = items_precon,
      preconditions = 0.3,
      persons = c(500, 200)))
    )
})
