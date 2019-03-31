# print functions
# output for CLASS 'lrtest'
#' @export
print.lrtest_nmst <- function(x,...) {
  cat("\nLikelihood ratio test results of", x$model, "estimation: \n\n")
  cat("Call:\n", paste(deparse(x$call), sep="\n", collapse="\n"),
            "\n\n", sep="")
  cat("\nLikelihood ratio test (Andersen):\n")
  cat("\nValue (Chi^2): ", round(x$LRvalue,3))
  cat("\ndf (Chi^2): ",x$df)
  cat("\np-value: ",round(x$pvalue,3))
  invisible(x)
}

#' @export
print.lrtest_mst <- function(x,...) {
  cat("\nLikelihood ratio test results of", x$model, "estimation: \n\n")
  cat("Call:\n", paste(deparse(x$call), sep="\n", collapse="\n"),
            "\n\n", sep="")
  cat("\nLikelihood ratio test (Andersen):\n")
  cat("\nValue (Chi^2): ", round(x$LRvalue,3))
  cat("\ndf (Chi^2): ",x$df)
  cat("\np-value: ",round(x$pvalue,3))
  invisible(x)
}

# output for CLASS mst (Rasch model with multistage-design)
#' @export
print.mst <- function(x,...){
  cat("\nResults of", x$model, "estimation: \n\n")
  cat("Call:\n", paste(deparse(x$call), sep="\n", collapse="\n"),
            "\n\n", sep="")
  cat("Log Likelihood:", x$loglik, "\n")
  cat("Number of items:", x$I, "\n")
  cat("Number of persons:", x$N, "\n")
  cat("Number of iterations:", x$iterations, "\n")
  cat("Number of parameters:", x$df+1, "\n")
  cat("used mstdesign:\n")
  print(x$designelements$design)
  cat("\n")
   cat("item parameters (difficulty): \n")
    betapar <- x$betapar
    if(!is.null(x$se.beta)){
      se <- x$se.beta
      result <- rbind(betapar, se)
      rownames(result) <- c("Estimate", "Std. Error")
    }else{
      result <- matrix(betapar,nrow = 1)
      rownames(result) <- "Estimate"
      colnames(result) <- names(betapar)
    }
    colnames(result) <- colnames(x$data)
  print(result)
  invisible(x)
}

# output for CLASS nmst (Rasch model without multistage-design)
#' @export
print.nmst <- function(x,...){
  cat("\n")
    cat("Results of", x$model, "estimation: \n\n")
  cat("Call:\n", paste(deparse(x$call), sep="\n", collapse="\n"),
            "\n\n", sep="")
  cat("Log Likelihood:", x$loglik, "\n")
  cat("Number of items:", x$I, "\n")
  cat("Number of persons:", x$N, "\n")
  cat("Number of iterations:", x$iterations, "\n")
  cat("Number of parameters:", x$df+1, "\n")
  cat("\n")
   cat("item parameters (difficulty): \n")
    betapar <- x$betapar
    if(!is.null(x$se.beta)){
      se <- x$se.beta
      result <- rbind(betapar, se)
      rownames(result) <- c("Estimate", "Std. Error")
    }else{
      result <- matrix(betapar,nrow = 1)
      rownames(result) <- "Estimate"
      colnames(result) <- names(betapar)
    }
  colnames(result) <- colnames(x$data)
  print(result)
  invisible(x)
}

