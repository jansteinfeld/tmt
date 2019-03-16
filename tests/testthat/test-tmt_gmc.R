items <- seq(-3,3,length.out = 30)
names(items) <- paste0("i",1:30)
persons <- 500
mean <- 0
sd <- 1
dat <- tmt:::sim.rm(persons, items,1111)
invisible(dat.rm <- tmt_rm(dat, optimization = "optim"))
suppressWarnings(dat.lrt <- tmt_lrtest(dat.rm, 
	split = "median", optimization = "optim"))
suppressWarnings(dat.lrt2 <- tmt_lrtest(dat.rm, 
	split = "median", optimization = "optim", se = FALSE))
suppressWarnings(dat.lrt3 <- tmt_lrtest(dat.rm, 
	split = "median", optimization = "optim", se = TRUE, sum0 = FALSE))
info <- rep(c("group_a","group_b"),each = 15)
names(info) <- paste0("i",1:30)

drop <- c("i1","i2","i5")

 p <- tmt_gmc(object = dat.lrt, 
			ellipse = TRUE, 
			info = info,
			drop = drop,
			title = "graphical model check",
			alpha = 0.05,
			legendtitle = "split criteria")

plot_class <- dat.lrt
class(plot_class) <- "mst"
# -----------------------------------------------------------------
context("test-tmt_gmc")
# -----------------------------------------------------------------
  test_that("tmt_gmc data structure", {
    expect_type(p,"list")
    expect_that(length(p), equals(9))
  })
  test_that("tmt_gmc class", {
       expect_is(
    	tmt_gmc(object = dat.lrt3, 
			ellipse = TRUE, 
			info = info,
			drop = NULL,
			title = "graphical model check",
			alpha = 0.05,
			legendtitle = "split criteria"), 
    	"ggplot"
    )
  })
# -----------------------------------------------------------------
context("test-tmt_gmc check warnings")
# -----------------------------------------------------------------
  test_that("tmt_gmc warnings", {
     expect_that(
    	tmt_gmc(object = dat.lrt2, 
			ellipse = TRUE, 
			info = info,
			drop = drop,
			title = "graphical model check",
			alpha = 0.05,
			legendtitle = "split criteria"), 
    	gives_warning()
    )
  })
# -----------------------------------------------------------------
context("test-tmt_gmc check errors")
# -----------------------------------------------------------------
  test_that("tmt_gmc error", {
    expect_that(
    	tmt_gmc(object = plot_class, 
			ellipse = TRUE, 
			info = info,
			drop = c("i100"),
			title = "graphical model check",
			alpha = 0.05,
			legendtitle = "split criteria"), 
    	throws_error()
    )
    expect_that(
    	tmt_gmc(object = dat.lrt, 
			ellipse = TRUE, 
			info = c("i100" = "2aus5"),
			drop = c("i100"),
			title = "graphical model check",
			alpha = 0.05,
			legendtitle = "split criteria"), 
    	throws_error()
    )
  })





