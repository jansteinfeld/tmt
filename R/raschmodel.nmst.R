raschmodel.nmst <- function(dat, weights = NULL, start = NULL, sum0 = TRUE, 
                           se = TRUE, optimization = "nlminb", call = NULL, ...){
  call_intern <- match.call()
  if(is.null(call)){
    call <- call_intern
  }
  # ----------------------------
  # diagnostic
  # a0 <- Sys.time(); cat("Start:",format(a0,"%H:%M:%S")); flush.console()
  # ----------------------------

  if ( !is.matrix(dat) & !is.data.frame(dat) ) stop("please submit either a 'data.frame' or a 'matrix'")

  # amount of Items and Persons
  i <- ncol(dat)
  n <- nrow(dat)

  # if colnames of Items missing, name Items :
  if (is.null( colnames(dat) ) ) colnames(dat) <- paste0("i",formatC(1:i,flag = 0, width = nchar(i)) )

  # get weights: only for further implementations
  if (is.null(weights) ) weights <- rep.int(1, n)
    
  if (!is.null(start) & length(start) != ncol(dat)){
    stop("Please submit a start vector with values for each item")
  }
  # ----------------------------
  # check input:
  # ----------------------------

  # convert data to matrix
  if (!is.matrix(dat) ) dat <- as.matrix(dat)

  # colSums
  cs <- colSums(dat * weights, na.rm = TRUE)

  # check data
  dat_orig <- dat
  dat <- data_check(dat = dat)$dat

  dat <- dat[weights > 0, , drop = FALSE]
  weights <- weights[weights > 0]

  
  # check if missings are present
  dat_na <- is.na(dat)

  # ----------------------------------------------------------
  if(sum0){
    # ----------------------------
    if(is.null(start)) {
      if(any(dat_na)){
        ws <- colSums(!is.na(dat) * weights)
        start <- log(ws - cs) - log(cs)
      } else {
        start <- log(sum(weights) - cs) - log(cs)
      }
    }
    start <- start[-i]
    desmat <- rbind(diag(1,nrow = length(start) ), -1)
    # ----------------------------
  }else{
    # ----------------------------
    if(is.null(start)) {
      if(any(dat_na)){
        ws <- colSums(!is.na(dat) * weights)
        start <- log(ws - cs) - log(cs) #previously:# -qlogis(cs/ws)
      } else {
        start <- log(sum(weights) - cs) - log(cs)
      }
    }
    start <- start[-1]
    desmat <- rbind(0,diag(1,nrow=length(start)))
    # ----------------------------
  }
  # ----------------------------------------------------------
  if(!any(dat_na)){
    # cat( "data prep" ) ; a1 <- Sys.time(); print( a1-a0 ) ; a0 <- a1
     rs <- rowSums(dat)
     rf <- as.vector(tapply(weights, factor(rs, levels = 0:i), sum))
     rf[is.na(rf)] <- 0

    cloglik <- function(par){
      b <- desmat %*% par
      g <- esf_nmst_sum_matrix( b,order=0 )[[1]]
      cll <- -sum( cs * b ) - sum( rf * log( g ) ) # Baker & Kim (2004 ;p.133 [equation 5.38]) (1, for score 0)
      return( -cll )
    }

    agradient <- function(par){
      b <- desmat %*% par
      g01 <- esf_nmst_sum_matrix( b, order = 1 )
      g0 <- g01[[1]]
      g1 <- g01[[2]]
      gr1 <- cs
      gr2 <- -((rf %*% (g1 / g0)) )
      gr <- gr1 + gr2
      gr <- gr %*% desmat 
      gr <- gr[1,]
      return(gr)
    }

   ahessian <- function(par) {
      b <- desmat %*% par
      g012 <- esf_nmst_sum_matrix( b, order = 2 )
      g0 <- g012[[1]]
      g1 <- g012[[2]]
      g2 <- g012[[3]]
      hessian <- matrix( 0, nrow = i, ncol = i )
      g1dg0 <- g1/g0
        for (ii in 1:i){ 
          hessian[ii,] <- ( rf %*% (g2[,ii,]/g0 - (g1[,ii]/g0) * g1dg0) )
        }
      out <- t(desmat) %*% hessian %*% desmat
      return(out)
   }
   #------------ if no Missings are in the dataset end here --------
  }

  # cat("NA-Patterns") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

  # Begin, if Missing Values are in the data
  if (any(dat_na)) {

    ## process NA patterns and calculate static things once
    res <- data_processing_nmst(dat, weights)
    na_patterns <- res$na_patterns
    y_i <- res$y_i 
    na_i <- res$na_i
    wi_i <- res$wi_i
    wi2_i <- res$wi2_i
    cs_i <- res$cs_i
    rs_i <- res$rs_i
    rf_i <- res$rf_i
    k_i <- res$k_i

    ## conditional log-likelihood function for NA pattern i
    cll_i <- function( cs_i, par_i, rf_i ) {
      -sum( cs_i * par_i ) - sum( rf_i * log(esf_nmst_sum_matrix( epsi = par_i, order=0)[[1]]) )
    }

    ## objective function: conditional log-likelihood
    cloglik <- function(par) {
        par_n <- c(desmat %*% par)
        par_i <- lapply(wi_i, function(x){
                                if(length(x) < 1) par_n else par_n[-x]
                        })
      ## initialize return values and extract esf parameters
      cll <- 0
      ## conditional log-likelihood
      cll <- sum(mapply(cll_i, cs_i, par_i, rf_i, SIMPLIFY = TRUE, USE.NAMES = FALSE))
     ## collect and return
      return(-cll)
    }

    ## analytical gradient
    agradient <- function(par) {
      par_n <- c(desmat %*% par)
        par_i <- lapply(wi_i, function(x){
          if(length(x) < 1) par_n else par_n[-x]
        })
      ## initialize return value and esf parameters
      rval <- matrix(0, nrow = length(seq_along(levels(na_patterns))), ncol = i)
      g01 <- mapply(FUN = esf_nmst_sum_matrix, epsi = par_i, order = 1, SIMPLIFY = FALSE)
       ## loop over observed NA patterns

      for(nai in seq_along(levels(na_patterns))) {
        if(length(wi_i[[nai]])>0){
          gr1 <- cs_i[[nai]]
          gr2 <- -( (rf_i[[nai]] %*% (g01[[nai]][[2]] / g01[[nai]][[1]])) )
          rval[nai, -wi_i[[nai]]] <- gr1 + gr2
        }else{
          gr1 <- cs_i[[nai]]
          gr2 <- -( (rf_i[[nai]] %*% (g01[[nai]][[2]] / g01[[nai]][[1]])) )
          rval[nai, ] <- gr1 + gr2
        }
      }
      return(colSums(rval[, drop = FALSE]) %*% desmat)
    }

    ahessian <- function(par) {
      par_n <- c(desmat %*% par)
        par_i <- lapply(wi_i, function(x){
          if(length(x) < 1) par_n else par_n[-x]
        })
       out <- matrix(0, nrow = length(par)+1, ncol = length(par)+1)
      g012 <- mapply(FUN = esf_nmst_sum_matrix, epsi = par_i, order = 2, SIMPLIFY = FALSE)
      
      ## loop over observed NA patterns      
      for(i in seq_along(levels(na_patterns))) {
        g0_i <- g012[[i]][[1]]
        g1_i <- g012[[i]][[2]]
        g2_i <- g012[[i]][[3]]
        i_i <- ncol(y_i[[i]])

        g1dg0 <- g1_i/g0_i
        hessian <- matrix(0, nrow = length(par)+1, ncol = length(par)+1)
        for (ii in seq_len(i_i)){
          if(length(wi_i[[i]])>0){
            hessian[-c(wi_i[[i]]),-c(wi_i[[i]])][ii,] <- (rf_i[[i]] %*% (g2_i[,ii,]/g0_i - (g1_i[,ii]/g0_i) * g1dg0))
          }else{
            hessian[ii,] <- ((rf_i[[i]] %*% (g2_i[,ii,]/g0_i - (g1_i[,ii]/g0_i) * g1dg0)))
          }
        }
        out <- out + hessian
      }
      out <- t(desmat) %*% out %*% desmat 
      return(out)
    }
  } #ende loop dat_na

  # results
  if(tolower(optimization) == "optim"){
    # -------------- OPTIM -------------------
    fit <- stats::optim(par = start, fn = cloglik, gr = agradient,
                        method = "BFGS", hessian = se, ...)
    # -------------- OPTIM -------------------
    loglik <- -fit$value                        #log-likelihood value
    iter <- as.numeric(fit$counts[1])           #number of iterations
    convergence <- fit$convergence
    betapar <- as.vector(desmat %*% fit$par)    #beta estimates
    names(betapar) <- paste0("est.b_",colnames(dat))

      if(se) {
        se.beta <- sqrt(diag(desmat %*% solve(fit$hessian) %*% t( desmat )))
        names(se.beta) <- paste0("se.b_",colnames(dat))
      } else {
        se.beta <- NULL
      }
  } else if(tolower(optimization) == "nlminb"){
    # -------------- nlminb -------------------
    fit <- stats::nlminb(start = start, objective = cloglik, gradient = agradient, ...)
    # -------------- nlminb -------------------
    loglik <- -fit$objective                        #log-likelihood value
    iter <- as.numeric(fit$iterations)           #number of iterations
    convergence <- fit$convergence
    betapar <- as.vector(desmat %*% fit$par)    #beta estimates
    names(betapar) <- paste0("est.b_",colnames(dat))
     
      if(se) {
        se.beta <- sqrt(diag(desmat %*% solve(ahessian(fit$par)) %*% t( desmat )))
        names(se.beta) <- paste0("se.b_",colnames(dat))
      } else {
        se.beta <- NULL
      }
  } else {
    stop("Only 'optim' and 'nlminb' are supported as optimization. \nPlease change your input '",optimization,"' to either 'optim' or 'nlminb'.")
  }
  # cat("optim") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

# return data
  out <- list(
    betapar = betapar,
    se.beta = se.beta,
    loglik = loglik,
    df = i - 1,
    data_orig = dat_orig,
    N = n,
    I = i,
    data = dat,
    desmat = desmat,
    convergence = convergence,
    iterations = iter,
    hessian = fit$hessian,
    model = "Rasch model (nmst)",
    call = call,
    mstdesign = NULL
  )
  class(out) <- "nmst"
return(out)
}
