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

mstdesign_max <- "
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
    b4 := Start(3,6) + B5(3,5) + B6(0,5)
  "

 mstdesign_start <- "
    B1 =~ c(i1, i2, i3, i4, i5)
    B2 =~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)
    B4 =~ c(i16, i17, i18, i19, i20)
    B5 =~ c(i21, i22, i23, i24, i25)
    B6 =~ c(i26, i27, i28, i29, i30)

    # define starting module
    Start1 == B4
    Start2 == B5

    # define branches
    b1 := Start1(0,2) + B2(0,2) + B1(0,5)
    b2 := Start1(0,2) + B2(3,5) + B3(0,5)
    b3 := Start1(3,5) + B5(0,2) + B3(0,5)
    b4 := Start1(3,5) + B5(3,5) + B6(0,5)
    b5 := Start2(0,2) + B2(0,2) + B1(0,5)
    b6 := Start2(0,2) + B2(3,5) + B3(0,5)
    b7 := Start2(3,5) + B5(0,2) + B3(0,5)
    b8 := Start2(3,5) + B5(3,5) + B6(0,5)
"

# -----------------------------------------------------------------
context("test-tmt_sim")
# -----------------------------------------------------------------
  test_that("tmt_sim data structure", {
    items <- seq(-2,2,length.out=30)
	names(items) <- c(paste0("i",1:30))
    set.seed(1111)
    persons <- rnorm(500,mean=0,sd=1)
    
    set.seed(1111)
	tmp <- tmt_sim(mstdesign = mstdesign,
			items = items,
			persons = 500,
			mean = 0,
			sd = 1, seed = 1111)
    tmp2 <- tmt_sim(mstdesign = mstdesign,
            items = items,
            persons = persons,
            mean = 0,
            sd = 1)
    tmp3 <- tmt_sim(mstdesign = mstdesign_start,
            items = items,
            persons = list(persons,persons),
            mean = 0,
            sd = 1)
    expect_is(tmp,"list")
    expect_that(length(tmp), equals(4))
    expect_that(tmp2$persons-tmp$persons, equals(rep(0,500)))
    expect_named(tmp, c("data", "data_mst", "persons", "mstdesign"))
    expect_s3_class(tmp,"mstdesign") 

    expect_is(tmp2,"list")
    expect_that(length(tmp2), equals(4))
    expect_named(tmp2, c("data", "data_mst", "persons", "mstdesign"))
    expect_s3_class(tmp2,"mstdesign") 
    expect_is(tmp3,"list")
    expect_that(length(tmp3), equals(4))
    expect_named(tmp3, c("data", "data_mst", "persons", "mstdesign"))
    expect_s3_class(tmp3,"mstdesign") 
  })

# -----------------------------------------------------------------
context("test-tmt_sim errors")
# -----------------------------------------------------------------
expect_that(tmt_sim(mstdesign=NULL,
			items = seq(-2,2,by=0.5),
			persons = 500,
			mean = 0,
			sd = 1), throws_error())

expect_that(tmt_sim(mstdesign = mstdesign_start,
            items = items,
            persons = list(500,500),
            mean = 0,
            sd = 1), throws_error())

expect_that(tmt_sim(mstdesign=mstdesign,
			items = NULL,
			persons = 500,
			mean = 0,
			sd = 1), throws_error())

expect_that(tmt_sim(mstdesign=mstdesign,
			items = seq(-2,2,by=0.5),
			persons = NULL,
			mean = 0,
			sd = 1), throws_error())

expect_that(tmt_sim(mstdesign=mstdesign,
			items = seq(-2,2,by=0.5),
			persons = 500,
			mean = 0,
			sd = 1), throws_error())
items <- seq(-2,2,length.out=30)
names(items) <- c(paste0("i",1:29),"ii30")

expect_that(tmt_sim(mstdesign=mstdesign,
			items = items,
			persons = 500,
			mean = 50,
			sd = 1), throws_error())

items <- seq(-2,2,length.out=30)
names(items) <- c(paste0("i",1:30))
expect_that(tmt_sim(mstdesign = mstdesign_max,
		items = items,
		persons = 500,
		mean = 0,
		sd = 1), throws_error())
# -----------------------------------------------------------------
context("test-tmt_sim warnings")
# -----------------------------------------------------------------
 test_that("tmt_sim data structure", {
    items <- seq(-2,2,length.out=30)
    names(items) <- c(paste0("i",1:30))
    set.seed(1111)
    expect_that(tmt_sim(mstdesign = mstdesign_start,
            items = items,
            persons = 500,
            mean = 0,
            sd = 1), gives_warning())
    expect_that(tmt_sim(mstdesign = mstdesign_start,
        items = items,
        persons = c(500,500),
        mean = 0,
        sd = 1), gives_warning())
    expect_that(tmt_sim(mstdesign = mstdesign_start,
        items = items,
        persons = c(500,500),
        mean = c(0,0),
        sd = 1), gives_warning())
    expect_that(tmt_sim(mstdesign = mstdesign_start,
        items = items,
        persons = c(500,500,500),
        mean = c(0,0),
        sd = 1), throws_error())
})
