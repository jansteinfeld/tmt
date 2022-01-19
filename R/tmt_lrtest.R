#' Computation of Andersen's Likelihood-Ratio Test
#'
#' This function applies the Likelihood Ratio Test of Andersen. Note that all persons with raw score equal to "median" are assigned to the lower group in cases of a median split. Is is also allowed to split after "mean" or submit any dichotomous vector as split criteria.
#' 
#' @param object it is necessary to submit an object of the function \code{mst} or \code{nmst}
#' @param split  default is the split criteria "median" of the raw score, optional are "mean" or any dichotomous vector
#' @param cores  submit integer of cores you would like to apply
#' @param se     logical: if true, the standard error is estimated
#' @param \ldots further arguments for the \code{tmt_rm} function
#' 
#' @author Jan Steinfeld
#'
#' @importFrom parallel detectCores clusterExport makeCluster parSapply stopCluster
#' @importFrom stats median pchisq
#'  
#' @return List with following entries
#' 
#' 	\item{data_orig}{Submitted data frame with item responses}
#' 	\item{betapars_subgroup}{List of item parameters (difficulty) for each subgroup}
#' 	\item{se.beta_subgroup}{List of standard errors of the estimated item parameters}
#' 	\item{model}{Used model ((mst) for Rasch model with multistage design)}
#' 	\item{LRvalue}{LR-value}
#' 	\item{df}{Degrees of freedoms for the test statistic}
#' 	\item{pvalue}{P-value of the likelihood ratio test}
#' 	\item{loglik_subgroup}{Log-likelihoods for the subgroups}
#' 	\item{split_subgroup}{List of split vector for each subgroup}
#' 	\item{call}{Submitted arguments for the function (matched call)}
#' 	\item{fitobj}{List of objects from subgroup estimation}
#' 
#' 
#' @useDynLib tmt
#' @importFrom Rcpp sourceCpp
#' 
#' @references
#'\itemize{
#'  \item Andersen, E. B. (1973). A goodness of fit test for the Rasch model. \emph{Psychometrika}, 38(1), 123-140.
#'  \item Fischer, G. H., & Molenaar, I. W. (Eds.). (2012). \emph{Rasch models: Foundations, recent developments, and applications}. 
#' 	    Springer Science & Business Media.
#' }
#' 
#' @seealso \code{\link{tmt_rm}}
#' @example ./R/.example_lrtest.R
#' 
#' @export
tmt_lrtest <- function(object, split = "median", cores = NULL, se = TRUE, ...){
	
	#if(class(object) %in% c("mst","nmst")){
  		#base::UseMethod("lrtest")
  	#} else {
  	#	stop("Please submit only objects of the class 'mst' or 'nmst' from the function tmt_rm\n")
  	#}
  	if (inherits(object, "mst")) {
		out <- lrtest.mst(object = object, split = split, cores = cores, se = se,...)
		class(out) <- "lrtest_mst"
		return(out)

	}else if (inherits(object, "nmst")) {
		out <- lrtest.nmst(object = object, split = split, cores = cores, se = se,...)
		class(out) <- "lrtest_nmst"
		return(out)
	} else{
		stop("Only objects of classes 'nmst' or 'mst' are allowed. Please use first the function 'tmt_rm'\n")
	}
}

