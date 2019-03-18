## ----setup, echo=FALSE, message=FALSE, warning=FALSE--------------------------
  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.width = 10, 
    fig.height = 10
  )
  options(width=80)

  # includes: in_header: "header.html"
  library(tmt)

## ---- rm_nmst, warning=FALSE--------------------------------------------------
   # simulate some data 
 dat <- tmt:::sim.rm(theta = 100,b = 10,seed = 1111)

  # estimate item parameters
 dat.rm <- tmt_rm(dat = dat, optimization = "optim")

 # print summary
 summary(dat.rm)


## ---- rm_mst, warning=FALSE---------------------------------------------------
  # Example for multistage-design
  mstdesign <- "
    B1 =~ c(i1, i2, i3, i4, i5)
    B2 =~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)

    # define starting Block
    Start == B2

    # define branches
    b1 := Start(0,2) + B1(0,5)
    b2 := Start(3,5) + B3(0,5)
  "
    # generate item parameters with corresponding names to the multistage design
  items <- seq(-1,1, length.out = 15)
  names(items) <- paste0("i",1:length(items))
  
    # generate random data under given multistage design
  dat <- tmt_sim(mstdesign = mstdesign, 
                      items = items, 
                      persons = 500, 
                      mean = 0, 
                      sd = 1)
    # estimate the item parameters under the given multistage-design
  dat.rm <- tmt_rm(dat = dat, 
                  mstdesign = mstdesign, 
                  optimization = "optim")
  
    # print summary of item parameters
  summary(dat.rm)

## ---- rm_lrtest, warning=FALSE------------------------------------------------
    # simulate some data
  dat_nmst <- tmt:::sim.rm(theta = 100,b = 10,seed = 1111)

    # estimate item parameters
  dat_nmst_rm <- tmt_rm(dat = dat_nmst, optimization = "optim")
  
    # calculate likelihood ratio-test
  dat_lrt_nmst <- tmt_lrtest(dat_nmst_rm, optimization = "optim")
  
    # print summary
  summary(dat_lrt_nmst)

## ---- rm_lrtest_mst, warning=FALSE--------------------------------------------
    # example of multistage-design
  mstdesign <- "
    B1 =~ c(i1, i2, i3, i4, i5)
    B2 =~ c(i6, i7, i8, i9, i10)
    B3 =~ c(i11, i12, i13, i14, i15)

    # define starting Block
    Start == B2

    # define branches
    b1 := Start(0,2) + B1(0,5)
    b2 := Start(3,5) + B3(0,5)
  "
    # generate item parameters with corresponding names to the multistage design
  items <- seq(-1,1, length.out = 15)
  names(items) <- paste0("i",1:length(items))
  
      # generate random data under given multistage design
  set.seed(1111)
  dat_mst <- tmt_sim(mstdesign = mstdesign, 
                      items = items, 
                      persons = 500, 
                      mean = 0, 
                      sd = 1)

    # estimate the item parameters under the given multistage-design
  dat_mst_rm <- tmt_rm(dat = dat_mst, 
                  mstdesign = mstdesign, 
                  optimization = "optim")

    # calculate likelihood ratio-test
  dat_lrt_mst <- tmt_lrtest(dat_mst_rm, optimization = "optim")
  
    # print summary
  summary(dat_lrt_mst)

## ---- gmt, warning=FALSE------------------------------------------------------
    # example of multistage-design
  items <- seq(-1,1,length.out = 30)
  names(items) <- paste0("i",1:30)
  persons = 100
  mean = 0
  sd = 1
  dat <- tmt:::sim.rm(theta = persons, b = items, 1111)
  
  dat.rm <- tmt_rm(dat, optimization = "optim")
  dat.lrt <- tmt_lrtest(dat.rm, split = "median", optimization = "optim")


  info <- rep(c("group_a","group_b"),each = 15)
  names(info) <- paste0("i",1:30)

  drop <- c("i1","i18","i20","i10")

  tmt_gmc(object = dat.lrt, 
    ellipse = TRUE, 
    info = info,
    drop = drop,
    title = "graphical model check",
    alpha = 0.05,
    legendtitle = "split criteria")


