raschmodel.mst <- function(dat, mstdesign = NULL, weights = NULL, start = NULL,
                          sum0 = TRUE, se = TRUE, optimization = "nlminb", call = NULL, ...)
{
  call_intern <- match.call()
  if(is.null(call)){
    call <- call_intern
  }
  # ----------------------------
  # diagnostic
  # a0 <- Sys.time(); cat("Start:",format(a0,"%H:%M:%S")); flush.console()
  # ----------------------------
  if(inherits(dat, "list") & "mstdesign" %in% class(dat)){
    mstdesign <- dat$mstdesign
    dat <- dat$data
  }
  if (!is.matrix(dat) & !is.data.frame(dat)) {
    stop("please submit either a 'data.frame' or a 'matrix'")
  }

  if(is.null(mstdesign)) {
    stop("it is necessary to specify the multistage design in the 'mstdesign' section \n")
  }

  if (!is.null(start) & length(start) != ncol(dat)){
    stop("Please submit a start vector with values for each item")
  }

  mstdesign_orig <- mstdesign
################################################################################################
# Verzweigungsschema zu den Daten hinzufuegen um damit dann spaeter evtl. einen Plot zu erzeugen?
 # if (!any(colnames(dat) %in% "branching")) {
 #   stop("branching design is required in the submitted data, with the column name 'branching'")
 # }

 # design <- dat[,"branching"]
 # dat <- dat[,!colnames(dat) %in% "branching"]
################################################################################################

  # convert data to matrix
  if (!is.matrix(dat) ) dat <- as.matrix(dat)
  # ----------------------------
  # check submitted data
  dat_orig <- dat
  dat_check <- data_check(dat = dat)
  dat <- dat_check$dat
  status <- dat_check$status
  
  # preparing the submitted branching design
  designelements <- tmt_mstdesign(mstdesign = mstdesign, options = c("design","items","start"))
  items_design <- designelements$items
  mstdesign <- apply(designelements$design,2,as.character)
  startblocks <-  designelements$start
  info_startval <- NULL
  if(nrow(startblocks)==1){
    info_startval <- strsplit(startblocks[,"items"],",")[[1]]
  }

  # amount of Items and Persons
  n <- nrow(dat)
  i <- ncol(dat)

  # check if all items in the data are also specified in the mstdesign and vice versa!
  if (!identical(sort(colnames(dat)),sort(items_design))) {
    if (!all(colnames(dat) %in% items_design)) {
      cat("The following items are specified in the dataset, but not in the submitted mstdesign: ")
      cat(colnames(dat)[!colnames(dat) %in% items_design],"\n")
    }
    if (!all(items_design %in% colnames(dat))) {
      cat("The following items are specified in the mstdesign, but not in the dataset: ")
      cat(items_design[!items_design %in% colnames(dat)],"\n")
    }
  #  if (length(status)!=0){
  #    cat("The following items had to be excluded, due to (nearly) full '1' or '0' responses: ",status,"\n")
  #  }  
    stop("It is necessary, that all Items in the dataset are also specified in the submitted mstdesign and vice versa! \nPleas update the design and the data.\n")
  }

    # get weights: only for further implementations
  if (is.null(weights) ) weights <- rep.int(1, n)
    
  dat <- dat[weights > 0, , drop = FALSE]
  weights <- weights[weights > 0]


  # ----------------------------
  # check input:
  # ----------------------------
  stopifnot(length(weights) == n)

  # ---------------- Start loop over multistage blocks here -------------------
  res <- data_processing_mst(dat, mstdesign, weights)
    y_i <- res$y_i
    na_p <- res$na_p
    items_i <- res$items_i
    items_l <- res$items_l
    minSolved_i <- res$minSolved_i
    maxSolved_i <- res$maxSolved_i
    cs_i <- res$cs_i
    rs_i <- res$rs_i
    rf_i <- res$rf_i

  if(sum0){
    # generate start vector
    if (is.null(start)) {
      cs <- colSums(dat * weights, na.rm = TRUE)
      ws <- colSums(!is.na(dat) * weights)
      start <- log(ws - cs) - log(cs)
        # choose for start block informative starting values
        if(!is.null(info_startval)){
          start[!names(start)%in%info_startval] <- 0
        }
    }
    start <- start[-i]
    desmat <- rbind(diag(1,nrow = length(start) ), -1)
    rownames(desmat) <- colnames(dat)
  }else{
    # generate start vector
    if (is.null(start)) {
      cs <- colSums(dat * weights, na.rm = TRUE)
      ws <- colSums(!is.na(dat) * weights)
      start <- log(ws - cs) - log(cs)
        # choose for start block informative starting values
        if(!is.null(info_startval)){
          start[!names(start)%in%info_startval] <- 0
        }
    }
    start <- start[-1]
    desmat <- rbind(0,diag(1,nrow=length(start)))
    rownames(desmat) <- colnames(dat)
  }

    ## conditional log-likelihood function for NA pattern i
    cll_i <- function(cs_i, par_i, rf_i, oj_i, minSolved_i, maxSolved_i) {
      esf <- esf_mst_sum_vector(parlist = par_i, ojlist = oj_i, 0,
                                              minSolved = minSolved_i, maxSolved = maxSolved_i)[[1]]
      esf[(sum(minSolved_i) + 1):(sum(maxSolved_i) + 1)] <- log(esf[(sum(minSolved_i)+1):(sum(maxSolved_i)+1)])
      return( -sum( cs_i * unlist(par_i) ) - sum(rf_i  * esf) )
    }

    cloglik <- function(par) {
        par_n <- c(desmat %*% par)
          par_i <- lapply(items_l,FUN = function(x){
            lapply(x,FUN = function(y){
              as.numeric(par_n[y])
            })
          })
        oj_i <- lapply(par_i, function(x) lapply(x, function(y) rep(1,length(y))))
        ## initialize return values and extract esf parameters
        cll <- 0
        ## conditional log-likelihood
        cll <- sum(mapply(cll_i, cs_i, par_i, rf_i, oj_i , minSolved_i, maxSolved_i, SIMPLIFY = TRUE, USE.NAMES = FALSE))
       ## collect and return
       # browser(expr= cll==Inf )
        return(-cll)
    }

    ## analytical gradient
    agradient <- function(par) {
      par_n <- c(desmat %*% par)
        par_i <- lapply(items_l,FUN = function(x){
          lapply(x,FUN = function(y){
            as.numeric(par_n[y])
          })
        })
      oj_i <- lapply(par_i, function(x) lapply(x, function(y) rep(1,length(y))))
      ## initialize return value and esf parameters
      out <- matrix(0, nrow = nrow(mstdesign), ncol = i)
      g01 <- mapply(FUN = esf_mst_sum_vector, parlist = par_i,
                    ojlist = oj_i, order = 1, minSolved = minSolved_i, maxSolved = maxSolved_i, SIMPLIFY = FALSE)
      for(nai in 1:nrow(mstdesign)) {
        min_nai <- sum(minSolved_i[[nai]])+1
        max_nai <- sum(maxSolved_i[[nai]])+1

        gr1   <- cs_i[[nai]]
        g0    <- g01[[nai]][[1]][sum(min_nai):sum(max_nai)]
        g1    <- g01[[nai]][[2]][sum(min_nai):sum(max_nai),]
        rf_ii <- rf_i[[nai]][sum(min_nai):sum(max_nai)]

        gr2 <- -(rf_ii %*% (g1 / g0))
        out[nai, items_i[[nai]]] <- gr1 + gr2
        }

      return(colSums(out[, drop = FALSE]) %*% desmat)
    }
    # esf[(sum(minSolved_i) + 1):(sum(maxSolved_i) + 1)] <- log(esf[(sum(minSolved_i)+1):(sum(maxSolved_i)+1)])

  ## analytival hessian matrix
  ahessian <- function(par) {
      par_n <- c(desmat %*% par)
       par_i <- lapply(items_l, FUN = function(x){
          lapply(x,FUN = function(y){
            as.numeric(par_n[y])
          })
        })
      oj_i <- lapply(par_i, function(x) lapply(x, function(y) rep(1,length(y))))
      out <- matrix(0, nrow = length(par)+1, ncol = length(par)+1)
      g012 <- mapply(FUN = esf_mst_sum_vector, parlist = par_i,
                     ojlist = oj_i, order = 2, minSolved = minSolved_i, maxSolved = maxSolved_i, SIMPLIFY = FALSE)
      ## loop over observed NA patterns      
      for(nai in 1:nrow(mstdesign)) {
        min_nai <- sum(minSolved_i[[nai]])+1
        max_nai <- sum(maxSolved_i[[nai]])+1

        g0_i <- g012[[nai]][[1]][sum(min_nai):sum(max_nai)]
        g1_i <- g012[[nai]][[2]][sum(min_nai):sum(max_nai),]
        g2_i <- g012[[nai]][[3]][sum(min_nai):sum(max_nai),,]
        rf_ii <- rf_i[[nai]][sum(min_nai):sum(max_nai)]
        
        i_i <- ncol(y_i[[nai]])
        g1dg0 <- g1_i/g0_i
        hessian <- matrix(0, nrow = length(par)+1, ncol = length(par)+1)
        for (ii in seq_len(i_i)){
         # if(length(unlist(items_l[[nai]]))>0){
            hessian[items_i[[nai]][ii],items_i[[nai]]] <- (rf_ii %*% (g2_i[,ii,]/g0_i - (g1_i[,ii]/g0_i) * g1dg0))
         # }else{
         #  hessian[items_i[[nai]][ii],items_i[[nai]]] <- (rf_ii %*% (g2_i[,ii,]/g0_i - (g1_i[,ii]/g0_i) * g1dg0))
         # }
        }
        out <- out + hessian
      }
      out <- t(desmat) %*% out %*% desmat
      
      return(out)
    }

# return data
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

  out <- list(
    betapar = betapar,
    se.beta = se.beta,
    loglik = loglik,
    df = i - 1,
    N = n,
    I = i,
    data_orig = dat_orig,
    data = dat,
    desmat = desmat,
    convergence = convergence,
    iterations = iter,
    hessian = fit$hessian,
    model = "Rasch model (mst)",
    call = call,
    designelements = designelements,
    mstdesign = mstdesign_orig
  )
  class(out) <- "mst"
  return(out)
}
