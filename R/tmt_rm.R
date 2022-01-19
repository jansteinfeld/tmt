#' Estimation (CML) ot the Rasch model with or without multistage designs.
#'
#' The \code{tmt_rm} function estimates the Rasch model. If the data are collected based on a multistage design (see Zwitser and Maris, 2015) the specific multistage design \code{mstdesign} has to be submitted. 
#'
#' @param dat          a matrix of dichotomous (0/1) data or a list of the function \code{tmt_designsim}
#' @param mstdesign    Model for the multistage design, if CML estimation without multistage designs is required, than leave the default value
#' @param weights      is optional for the weights of cases
#' @param start        Vector of start values. If no vector is provided, the start values will be automatic generated
#' @param sum0         logical: If the item parameters should be normed to 'sum = 0' as recommended by Glas (2016, p. 208). Otherwise sum0=FALSE
#' @param se           logical: should the standard error should be estimated?
#' @param optimization character: Per default 'nlminb' is used but 'optim' is also supported.
#' @param ...          optional further arguments for optim and nlminb use control = list() with arguments.
#' 
#' @author Jan Steinfeld
#' 
#' @importFrom stats optim nlminb
#' @importFrom Rcpp sourceCpp
#' @importFrom utils tail
#' 
#' @details
#' According to Glas (1988) <doi:10.3102/10769986013001045> CML estimation of item parameters is biased if the data is collected in multistage designs and this design is not considered. Zwitser and Maris (2015) <doi:10.1007/s11336-013-9369-6> propose to use an additional design matrix to fragment the elementary symmetric function. Their approach is implemented in this package. MST designs with a probabilistic instead of a deterministic routing rule (see, e.g. Chen, Yamamoto, & von Davier, 2014 <doi:10.1201/b16858>) are not estimated with this method, therefore the proposed solouting is again modified by Steinfeld and Robitzsch (2021) <doi:10.31234/osf.io/ew27f> which is also integrated into this package.
#' 
#' @return List with following entries
#' 
#' 	\item{betapar}{Estimated item difficulty parameters (if sum0=FALSE, than the first item is set to 0)}
#' 	\item{se.beta}{Standard errors of the estimated item parameters}
#' 	\item{loglik}{Conditional log-likelihood of the model}
#' 	\item{df}{Number of estimated parameters}
#' 	\item{N}{Number of Persons}
#' 	\item{I}{Number of items}
#' 	\item{data_orig}{Submitted data frame with item responses}
#' 	\item{data}{Used data frame with item responses}
#' 	\item{desmat}{Design matrix}
#' 	\item{convergence}{Convergence criterion}
#' 	\item{iterations}{Number of iterations}
#' 	\item{hessian}{Hessian-Matrix}
#' 	\item{model}{Used model ((mst) for Rasch model with multistage design)}
#' 	\item{call}{Submitted arguments for the function (matched call)}
#' 	\item{designelements}{If the multistage version is requested, the preprocessed design is returned, otherwise NULL}
#' 	\item{mstdesign}{If the multistage version is requested, the submitted design is returned, otherwise NULL}
#' 
#' 
#' @useDynLib tmt
#' 
#' @references
#'\itemize{
#'  \item Baker, F. B., & Harwell, M. R. (1996). Computing elementary symmetric functions and their derivatives:
#'     A didactic. \emph{Applied Psychological Measurement}, 20(2), 169-192.
#'  \item Baker, F. B., & Kim, S. H. (2004). \emph{Item response theory: Parameter estimation techniques}. CRC Press.
#' \item Chen, H., Yamamoto, K., & von Davier, M. (2014). Controlling multistage testing exposure rates in international large-scale assessments. 
#'    In A. Yan, A. A. von Davier, & C. Lewis (Eds.), \emph{Computerized Multistage Testing: Theory and Applications} (pp. 391–409). 
#'    New York: CRC Press. https://doi.org/10.1201/b16858
#'  \item Fischer, G. H., & Molenaar, I. W. (Eds.). (2012). \emph{Rasch models: Foundations, recent developments, and applications}. 
#' 			Springer Science & Business Media.
#' 	\item Formann, A. K. (1986). A note on the computation of the second-order derivatives of the elementary symmetric 
#' 	   functions in the Rasch model. \emph{Psychometrika}, 51(2), 335-339.
#' 	\item Glas, C.A.W. (1988). The Rasch model and multistage testing. \emph{Journal of Educational Statistics}, 13(1), 45-52.
#' \item Glas, C.A.W. (2016). Maximum-Likelihood Estimation. In van der Linden, W.J. (Ed.), \emph{Handbook of Item Response Theory: 
#' 	  Volume two: Statistical tools.} (pp. 197 - 236). New York: CRC Press.
#'  \item Rasch, G. (1960). \emph{Probabalistic models for some intelligence and attainment tests.} Danmarks
#'     paedagogiske institut.
#' \item Steinfeld, J., & Robitzsch, A. (2021). Conditional maximum likelihood estimation in probability-branched multistage designs. 
#'    \emph{PsyArXiv}. 20 March 2021. https://doi.org/10.31234/osf.io/ew27f
#'  \item Verhelst, N.D., Glas, C.A.W., & van der Sluis, A. (1984). Estimation Problems in the Rasch-Model:
#'     The Basic Symmetric Functions. \emph{Computational Statistics Quarterly}, 1(3), 245-262.
#'  \item Zwitser, R. J., & Maris, G. (2015). Conditional statistical inference with multistage testing designs.
#'     \emph{Psychometrika}, 80(1), 65-84.
#' }
#' 
#' @seealso \link{tmt_lrtest}
#' 
#' @example ./R/.example_raschmodel.R
#' 
#' @export
tmt_rm <- function(dat, mstdesign = NULL, weights = NULL, start = NULL, 
                  sum0 = TRUE, se = TRUE, optimization = "nlminb", ...){

	call <- match.call()
	
	if(!is.null(mstdesign)|inherits(dat, "mstdesign")){
		out <- raschmodel.mst(dat = dat, mstdesign = mstdesign, weights = weights, start = start, 
							sum0 = sum0, se = se, optimization = optimization, call = call, ...)
		class(out) <- "mst"
		return(out)

	}
	if(is.null(mstdesign)){
		out <- raschmodel.nmst(dat = dat, weights = weights, start = start,  
							sum0 = sum0, se = se, optimization = optimization, call = call, ...)
        class(out) <- "nmst"
		return(out)
	}
}
