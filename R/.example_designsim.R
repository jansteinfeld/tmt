\dontrun{
#############################################################################
# translate multistage model 1
#############################################################################
 mstdesign <- "
	 B1 =~ c(i1, i2, i3, i4, i5)
	 B2 =~ c(i6, i7, i8, i9, i10)
	 B3 =~ c(i11, i12, i13, i14, i15)

	 # define starting Block
	 Start == B2

	 # define branches
	 b1 := Start(0,2) + B1
	 b2 := Start(3,5) + B3
 "
 items <- seq(-3,3,length.out = 15)
 names(items) <- paste0("i",1:15)

 persons = 500
set.seed(1111)
 tmt_sim(mstdesign = mstdesign, items = items, persons = persons, mean = 0, sd = 1)

#############################################################################
# translate multistage model 2
#############################################################################
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
    b1 := Start(0,2) + B2(0,2) + B1
    b2 := Start(0,2) + B2(3,5) + B3
    b3 := Start(3,5) + B5(0,2) + B3
    b4 := Start(3,5) + B5(3,5) + B6
  "

 items <- seq(-3,3,length.out = 30)
 names(items) <- paste0("i",1:30)
persons = 500
set.seed(1111)
 tmt_sim(mstdesign = mstdesign, items = items, persons = persons, mean = 0, sd = 1)


}
