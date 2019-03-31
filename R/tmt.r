#' @details
#' In multistage tests different groups of items (blocks) are presented to persons depending on their response behavior to previous item groups. Multistage testing is thus a simple form of adaptive testing. If data is collected on the basis of such a multistage design and the items are estimated using the Conditional Maximum Likelihood (CML) method, Glas (1989) <doi:10.3102/10769986013001045> has shown, that the item parameters are biased. Zwitser and Maris (2013) <doi:10.1007/s11336-013-9369-6> showed in their work, that taking the applied multistage design in consideration and including it in the estimation of the item parameters, the estimation of item parameters is not biased using the CML method. Their proposed solution is implemented in our package.
#' 
#' An application example can be found in the vignette by using the following command in the R console \code{vignette("introduction_to_tmt")}
#'
#' @section logo:
#' \if{latex}{\figure{tmt.pdf}{options: width=0.6in}}
#' 
#' 
#' @references
#'\itemize{
#' \item Andersen, E. B. (1973). A goodness of fit test for the Rasch model. \emph{Psychometrika}, 38(1), 123-140.
#' \item Baker, F. B., & Harwell, M. R. (1996). Computing elementary symmetric functions and their derivatives:
#'    A didactic. \emph{Applied Psychological Measurement}, 20(2), 169-192. Chicago
#' \item Baker, F. B., & Kim, S. H. (2004). \emph{Item response theory: Parameter estimation techniques}. CRC Press.
#' \item Fischer, G. H., & Molenaar, I. W. (Eds.). (2012). \emph{Rasch models: Foundations, recent developments, and applications}. 
#'    Springer Science & Business Media.
#' \item Formann, A. K. (1986). A note on the computation of the second-order derivatives of the elementary symmetric 
#'    functions in the Rasch model. \emph{Psychometrika}, 51(2), 335-339.
#' \item Glas, C.A.W. (1988). The Rasch model and multistage testing. \emph{Journal of Educational Statistics}, 13(1), 45-52.
#' \item Glas, C.A.W. (2016). Maximum-Likelihood Estimation. In van der Linden, W.J. (Ed.), \emph{Handbook of Item Response Theory: 
#'    Volume two: Statistical tools.} (pp. 197 - 236). New York: CRC Press.
#' \item Rasch, G. (1960). \emph{Probabalistic models for some intelligence and attainment tests.} Danmarks
#'    paedagogiske institut.
#' \item Verhelst, N.D., Glas, C.A.W. und van der Sluis, A. (1984). Estimation Problems in the Rasch-Model:
#'    The Basic Symmetric Functions. \emph{Computational Statistics Quatarly}, 1(3), 245-262.
#' \item Zwitser, R. J., & Maris, G. (2015). Conditional statistical inference with multistage testing designs.
#'    \emph{Psychometrika}, 80(1), 65-84.
#' }
#' 
#' @examples
#'  tmt:::tmt_ascii()
#' 
#' 
#' 
"_PACKAGE"
