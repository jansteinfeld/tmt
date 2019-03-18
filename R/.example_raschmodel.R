# example for tmt_rm
#############################################################################
# Example simple Rasch model 
#############################################################################
dat <- tmt:::sim.rm(theta = 100,b = 10,seed = 1111)
dat.rm <- tmt_rm(dat = dat)
summary(dat.rm)

#############################################################################
# Example for multistage-design
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
