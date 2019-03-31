# example for tmt_mstdesign
\dontrun{
###################################
# Example-2 
###################################
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
# ---------------------------
# for simulation purposes 
tmt_mstdesign(mstdesign, options = "simulation")$simulation

# ---------------------------
# summary of the submitted design
tmt_mstdesign(mstdesign, options = "design")$tmtdesign

# ---------------------------
# matrix of all blocks with the containing items
tmt_mstdesign(mstdesign, options = "blocks")$tmtblocks

# ---------------------------
# vector of all items
tmt_mstdesign(mstdesign, options = "items")$items

# ---------------------------
# list of all four elements
tmt_mstdesign(mstdesign, options = c("design", "simulation", "blocks", "items"))
}

###################################
# Example-2 
###################################
mstdesign <- "
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
designelements <- tmt_mstdesign(mstdesign, 
    options = c("design", "simulation", "blocks", "items"))
