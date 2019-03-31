# example for tmt_rm
#############################################################################
# Example-1 simple Rasch model 
#############################################################################
dat <- tmt:::sim.rm(theta = 100, b = 10, seed = 1111)
dat.rm <- tmt_rm(dat = dat)
summary(dat.rm)

#############################################################################
# Example-1 for multistage-design
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

items <- seq(-1,1,length.out = 15)
names(items) <- paste0("i",1:15)
persons = 1000
mean = 0
sd = 1
dat <- tmt_sim(mstdesign = mstdesign, 
  items = items, persons = persons, mean = mean, sd = sd)
dat.rm <- tmt_rm(dat = dat, mstdesign = mstdesign)
summary(dat.rm)

\dontrun{
  ############################################################################
  # Example-2 simple Rasch model 
  ############################################################################
  dat <- tmt:::sim.rm(theta = 100, b = 10, seed = 1111)
  dat.rm <- tmt_rm(dat = dat)
  summary(dat.rm)

  ############################################################################
  # Example-2 for multistage-design
  ############################################################################
  # also using 'paste' is possible
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
  items <- seq(-1,1,length.out = 30)
  names(items) <- paste0("i",1:30)
  persons = 1000
  mean = 0
  sd = 1
  dat <- tmt_sim(mstdesign = mstdesign, 
    items = items, persons = persons, mean = mean, sd = sd)
  dat.rm <- tmt_rm(dat = dat, mstdesign = mstdesign)
  summary(dat.rm)

}