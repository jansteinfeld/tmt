% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmt_gmc.R
\name{tmt_gmc}
\alias{tmt_gmc}
\title{Function for the Graphical Model Check}
\usage{
tmt_gmc(
  object,
  title = "graphical model check",
  xaxis = NULL,
  yaxis = NULL,
  lim = NULL,
  ellipse = FALSE,
  drop = NULL,
  alpha = 0.05,
  legendtitle = "split criteria",
  info = NULL
)
}
\arguments{
\item{object}{object of the function [tmt::tmt_lrttest()]}

\item{title}{of the plot}

\item{xaxis}{description of the x-axis}

\item{yaxis}{description of the y-axis}

\item{lim}{of the plot}

\item{ellipse}{should confidence-ellipse be plotted}

\item{drop}{which items should be excluded from the plot}

\item{alpha}{which alpha should be used for the ellipse}

\item{legendtitle}{Title of the Legend}

\item{info}{vector with further information for the Plot with names of submitted items}
}
\description{
This function performs a so-called graphical model check on the basis of the previously performed Likelihood Ratio Test [tmt::tmt_lrttest()]. The estimated item parameters of the two groups are plotted against each other. There is the possibility in this function to highlight items, to be excluded items from the plot, and to produce confidence-ellipses if desired.
}
\examples{
#############################################################################
# Example of Graphical Model Check
#############################################################################
items <- seq(-3,3,length.out = 16)
names(items) <- paste0("i",1:16)
persons = 500
dat <- tmt:::sim.rm(theta = persons, b = items, seed = 1234)
dat.rm <- tmt_rm(dat)
dat.lrt <- tmt_lrtest(dat.rm, split = "median")

info <- rep(c("group_a","group_b"),each = 8)
names(info) <- paste0("i",1:16)

drop <- c("i1","i18")

#library(ggplot2)
plot <- tmt_gmc(object = dat.lrt, 
	ellipse = TRUE, 
	info = info,
	drop = drop,
	title = "graphical model check",
	alpha = 0.05,
	legendtitle = "split criteria")

}
\author{
Jan Steinfeld
}
