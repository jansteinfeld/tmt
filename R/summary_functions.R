# ---------------------------------
# Class mst
# ---------------------------------
#' @export
summary.mst <- function(object,...){
  cat("\n")
   cat("Call:\n", "  " ,paste(deparse(object$call), sep="\n", collapse="\n"),
          "\n\n", sep="")
  cat("\n")
    cat("Results of", object$model, "estimation: \n")
  cat("\n")
  cat("Difficulty parameters: \n")
    betapar <- object$betapar
    if(!is.null(object$se.beta)){
      se <- object$se.beta
      result <- rbind(betapar, se)
      rownames(result) <- c("Estimate", "Std. Error")
    }else{
      result <- matrix(betapar,nrow = 1)
      rownames(result) <- "Estimate"
      colnames(result) <- names(betapar)
    }
  print(result)
  cat("\n")
  cat("CLL:", object$loglik, "\n")
  cat("Number of iterations:", object$iterations, "\n")
  cat("Number of parameters:", object$df+1, "\n")
  cat("\n")
  invisible(object)
}

# #############################################

# ---------------------------------
# Class nmst
# ---------------------------------
#' @export
summary.nmst <- function(object,...){
  cat("\n")
   cat("Call:\n", "  " ,paste(deparse(object$call), sep="\n", collapse="\n"),
          "\n\n", sep="")
  cat("\n")
    cat("Results of", object$model, "estimation: \n")
  cat("\n")
  cat("Difficulty parameters: \n")
    betapar <- object$betapar
    if(!is.null(object$se.beta)){
      se <- object$se.beta
      result <- rbind(betapar, se)
      rownames(result) <- c("Estimate", "Std. Error")
    }else{
      result <- matrix(betapar,nrow = 1)
      rownames(result) <- "Estimate"
      colnames(result) <- names(betapar)
    }
  print(result)
  cat("\n")
  cat("CLL:", object$loglik, "\n")
  cat("Number of iterations:", object$iterations, "\n")
  cat("Number of parameters:", object$df+1, "\n")
  cat("\n")
  invisible(object)
}

# #############################################

# ---------------------------------
# Class lrt_mst
# ---------------------------------
#' @export
summary.lrtest_mst <- function(object,...)
  # summary method for objects of class "LR" (from LRtest")
{
  cat("\nLikelihood ratio test (Andersen) for multistage designs:\n")
  cat("\nValue (Chi^2): ", round(object$LRvalue,3))
  cat("\ndf (Chi^2): ",object$df)
  cat("\np-value: ",round(object$pvalue,3))
}
# ---------------------------------
# Class lrt_nmst
# ---------------------------------
#' @export
summary.lrtest_nmst <- function(object,...)
  # summary method for objects of class "LR" (from LRtest")
{
  cat("\nLikelihood ratio test (Andersen):\n")
  cat("\nValue (Chi^2): ", round(object$LRvalue,3))
  cat("\ndf (Chi^2): ",object$df)
  cat("\np-value: ",round(object$pvalue,3))
}
