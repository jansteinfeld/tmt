# example for tmt_lrtest
\dontrun{
#############################################################################
# Example Rasch model and Likelihood Ratio Test
#############################################################################
dat <- tmt:::sim.rm(theta = 100,b = 10,seed = 1111)
dat.rm <- tmt_rm(dat = dat)
dat.lrt <- tmt_lrtest(dat.rm)
summary(dat.lrt)
}
