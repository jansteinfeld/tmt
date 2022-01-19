#############################################################################
# translate multistage model 1
#############################################################################
 mstdesign <- "
	 M1 =~ c(i1, i2, i3, i4, i5)
	 M2 =~ c(i6, i7, i8, i9, i10)
	 M3 =~ c(i11, i12, i13, i14, i15)

	 # define branches
	 p1 := M2(0,2) + M1
	 p2 := M2(3,5) + M3
 "
items <- seq(-3,3,length.out = 15)
names(items) <- paste0("i", seq(items))

data_1 <- tmt_sim(mstdesign = mstdesign, 
    items = items, 
    persons = 500, 
    seed = 1111)

#############################################################################
# translate multistage model 2
#############################################################################
mstdesign <- "
    M1 =~ c(i1, i2, i3, i4, i5)
    M2 =~ c(i6, i7, i8, i9, i10)
    M3 =~ c(i11, i12, i13, i14, i15)
    M4 =~ c(i16, i17, i18, i19, i20)
    M5 =~ c(i21, i22, i23, i24, i25)
    M6 =~ c(i26, i27, i28, i29, i30)

    # define branches
    p1 := M4(0,2) + M2(0,2) + M1
    p2 := M4(0,2) + M2(3,5) + M3
    p3 := M4(3,5) + M5(0,2) + M3
    p4 := M4(3,5) + M5(3,5) + M6
  "

items <- seq(-3,3,length.out = 30)
names(items) <- paste0("i", seq(items))

data_2 <- tmt_sim(mstdesign = mstdesign, 
    items = items, 
    persons = 500,
    seed = 1111)
