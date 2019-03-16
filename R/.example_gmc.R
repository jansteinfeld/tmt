\dontrun{
#############################################################################
# Example of Graphical Model Check
#############################################################################
items <- seq(-3,3,length.out = 30)
names(items) <- paste0("i",1:30)
persons = 100
mean = 0
sd = 1
dat <- tmt:::sim.rm(theta = persons, beta = items,1111)
dat.rm <- tmt_rm(dat, optimization = "optim")
dat.lrt <- tmt_lrtest(dat.rm, split = "median", optimization = "optim")

info <- rep(c("group_a","group_b"),each = 15)
names(info) <- paste0("i",1:30)

drop <- c("i1","i18","i20","i100")

library(ggplot2)
tmt_gmc(object = dat.lrt, 
	ellipse = TRUE, 
	info = info,
	drop = drop,
	title = "graphical model check",
	alpha = 0.05,
	legendtitle = "split criteria")
}
