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
  M1 =~ c(i1, i2, i3, i4, i5)
  M2 =~ c(i6, i7, i8, i9, i10)
  M3 =~ c(i11, i12, i13, i14, i15)

  # define starting module
  Start == M2

  # define path
  p1 := Start(0,2) + M1
  p2 := Start(3,5) + M3
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
    M1 =~ paste0('i',1:5)
    M2 =~ paste0('i',6:10)
    M3 =~ paste0('i',11:15)
    M4 =~ paste0('i',16:20)
    M5 =~ paste0('i',21:25)
    M6 =~ paste0('i',26:30)

    # define starting module
    Start == M4

    # define path
    p1 := Start(0,2) + M2(0,2) + M1
    p2 := Start(0,2) + M2(3,5) + M3
    p3 := Start(3,5) + M5(0,2) + M3
    p4 := Start(3,5) + M5(3,5) + M6
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

    ############################################################################
  # Example-3 for cumulative multistage-design
  ############################################################################
  # also using 'paste' is possible
  mstdesign <- "
    M1  =~ paste0('i',21:30)
    M2  =~ paste0('i',11:20)
    M3  =~ paste0('i', 1:10)
    M4  =~ paste0('i',31:40)
    M5  =~ paste0('i',41:50)
    M6  =~ paste0('i',51:60)
    
    # define starting module
    Start == M1

    # define path
    p1 := Start(0, 5) += M2( 0,10) += M3
    p2 := Start(0, 5) += M2(11,15) += M4
    p3 := Start(6,10) += M5( 6,15) += M4
    p4 := Start(6,10) += M5(16,20) += M6
    "
  items <- seq(-1,1,length.out = 60)
  names(items) <- paste0("i",1:60)
  persons = 1000
  mean = 0
  sd = 1
  dat <- tmt_sim(mstdesign = mstdesign, 
    items = items, persons = persons, mean = mean, sd = sd)
  dat.rm <- tmt_rm(dat = dat, mstdesign = mstdesign)
  summary(dat.rm)

}