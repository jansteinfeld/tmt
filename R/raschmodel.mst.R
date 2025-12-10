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
  if(!is.null(mstdesign)) {
    mstdesign_orig <- mstdesign  
  } 
  # else {
  #   mstdesign_orig <- mstdesign
  # }
  
  ################################################################################################
  # todo: MST Design should be added for additional plot
   # if (!any(colnames(dat) %in% "branching")) {
   #   stop("branching design is required in the submitted data, with the column name 'branching'")
   # }

   # design <- dat[,"branching"]
   # dat <- dat[,!colnames(dat) %in% "branching"]
  ################################################################################################
  # preparing the submitted mst design
  designelements <- tmt_mstdesign(mstdesign = mstdesign, options = c("design","items"))
  items_design <- designelements$items
  cumulative <- ifelse(any(class(designelements$design)=="cumulative"),TRUE,FALSE)
  mstdesign <- apply(designelements$design,2,as.character)
  # startmodules <-  designelements$start
  # info_startval <- NULL
  
  # if(nrow(startmodules)==1){
  #   info_startval <- strsplit(startmodules[,"items_to"],",")[[1]]
  # }


  names_dat <- colnames(dat)

  precondition <- NULL
  if (!is.null(designelements$preconditions$precondition_matrix)) {
    precon_nam <- designelements$preconditions$precondition_matrix[,"name"]
    # adjust the information of dat, remove preconditions
    names_dat <- names_dat[!names_dat%in%unlist(precon_nam)]
    precondition <- designelements$preconditions$preconditions
  }

  # check if all items in the data are also specified in the mstdesign and vice versa!
  if (!identical(sort(names_dat),sort(items_design))) {
    if (!all(names_dat %in% items_design)) {
      cat("The following items are specified in the dataset, but not in the submitted mstdesign: ")
      cat(names_dat[!names_dat %in% items_design],"\n")
    }
    if (!all(items_design %in% names_dat)) {
      cat("The following items are specified in the mstdesign, but not in the dataset: ")
      cat(items_design[!items_design %in% names_dat],"\n")
    }
  #  if (length(status)!=0){
  #    cat("The following items had to be excluded, due to (nearly) full '1' or '0' responses: ",status,"\n")
  #  }  
    stop("It is necessary, that all Items in the dataset are also specified in the submitted mstdesign and vice versa! \nPlease update the design and the data.\n")
  }


  # convert data to matrix
  if (!is.matrix(dat) ) dat <- as.matrix(dat)
  # ----------------------------


  # amount of Items and Persons
  n <- nrow(dat)
  i <- ncol(dat[,names_dat])



  # check submitted data
  dat_orig <- dat
  dat_check <- data_check(dat = dat, items = items_design)
  dat <- dat_check$dat
  # status <- dat_check$status
  
    # get weights: only for further implementations
  if (is.null(weights) ) weights <- rep.int(1, n)
    
  dat <- dat[weights > 0, , drop = FALSE]
  weights <- weights[weights > 0]


  # ----------------------------
  # check input:
  # ----------------------------
  stopifnot(length(weights) == n)

  # ---------------- Start loop over multistage modules here -------------------
  res <- data_processing_mst(dat, mstdesign, weights, precondition, cumulative)
    y_i <- res$y_i
    # na_p <- res$na_p
    cs_i <- res$cs_i
    # rs_i <- res$rs_i
    rf_i <- res$rf_i
    items_i <- res$items_i
    items_l <- res$items_l
    probabilities_i <- res$probabilities_i
    minSolved_i <- res$minSolved_i
    maxSolved_i <- res$maxSolved_i
    minSolved_stage_i <- res$minSolved_stage_i
    maxSolved_stage_i <- res$maxSolved_stage_i
    cumulative_i <- res$cumulative_i

  if(sum0){
    # generate start vector
    if (is.null(start)) {
      cs <- colSums(dat[,names_dat] * weights, na.rm = TRUE)
      ws <- colSums(!is.na(dat[,names_dat]) * weights)
      start <- log(ws - cs) - log(cs)
        # choose for start module informative starting values
        # if(!is.null(info_startval)){
        #   start[!names(start)%in%info_startval] <- 0
        # }
    }
    start <- start[-i]
    desmat <- rbind(diag(1,nrow = length(start) ), -1)
    rownames(desmat) <- colnames(dat[,names_dat])
  } else {
    # generate start vector
    if (is.null(start)) {
      cs <- colSums(dat[,names_dat] * weights, na.rm = TRUE)
      ws <- colSums(!is.na(dat[,names_dat]) * weights)
      start <- log(ws - cs) - log(cs)
        # choose for start module informative starting values
        # if(!is.null(info_startval)){
        #   start[!names(start)%in%info_startval] <- 0
        # }
    }
    start <- start[-1]
    desmat <- rbind(0,diag(1,nrow=length(start)))
    rownames(desmat) <- colnames(dat[,names_dat])
  }

    ## conditional log-likelihood function for NA pattern i
    cll_i <- function(cs_i, par_i, probabilities_i, rf_i, oj_i, minSolved_i, minSolved_stage_i, maxSolved_i, maxSolved_stage_i, cumulative_i) {
      esf <- esf_mst_sum_vector(parlist = par_i, ojlist = oj_i, probs = probabilities_i, order = 0,
                                minSolved = minSolved_i, maxSolved = maxSolved_i,
                                minSolved_design = minSolved_stage_i, maxSolved_design = maxSolved_stage_i, cumulative = cumulative_i)[[1]]
      #esf[(sum(minSolved_i) + 1):(sum(maxSolved_i) + 1)] <- log(esf[(sum(minSolved_i)+1):(sum(maxSolved_i)+1)])
      esf[(tail(minSolved_stage_i,n=1) + 1):(tail(maxSolved_stage_i,n=1) + 1)] <- log(esf[(tail(minSolved_stage_i,n=1) + 1):(tail(maxSolved_stage_i,n=1) + 1)])
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
        cll <- sum(mapply(cll_i, cs_i, par_i, probabilities_i, rf_i, oj_i, minSolved_i, minSolved_stage_i, maxSolved_i, maxSolved_stage_i, cumulative_i, SIMPLIFY = TRUE, USE.NAMES = FALSE))
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
      # probabilities_ii <- lapply(probabilities_i,unlist)
      g01 <- mapply(FUN = esf_mst_sum_vector, parlist = par_i,
                    ojlist = oj_i, order = 1, minSolved = minSolved_i, maxSolved = maxSolved_i,
                    minSolved_design = minSolved_stage_i, maxSolved_design = maxSolved_stage_i, probs = probabilities_i, cumulative = cumulative_i, SIMPLIFY = FALSE)
      for(nai in seq_len(nrow(mstdesign))) {
        min_nai <- minSolved_stage_i[[nai]] + 1
        max_nai <- maxSolved_stage_i[[nai]] + 1
        min_nai <- tail(min_nai,n=1)
        max_nai <- tail(max_nai,n=1)

        gr1   <- cs_i[[nai]]
        g0    <- g01[[nai]][[1]][min_nai:max_nai]
        g1    <- g01[[nai]][[2]][min_nai:max_nai,]
        rf_ii <- rf_i[[nai]][min_nai:max_nai]

        gr2 <- -(rf_ii %*% (g1 / g0))
        out[nai, items_i[[nai]]] <- gr1 + gr2
        }
      out <- colSums(out[, drop = FALSE]) %*% desmat
      return(out)
    }
    # esf[(sum(minSolved_i) + 1):(sum(maxSolved_i) + 1)] <- log(esf[(sum(minSolved_i)+1):(sum(maxSolved_i)+1)])

  # # analytival hessian matrix
  ahessian <- function(par) {
      par_n <- c(desmat %*% par)
       par_i <- lapply(items_l, FUN = function(x){
          lapply(x,FUN = function(y){
            as.numeric(par_n[y])
          })
        })
      oj_i <- lapply(par_i, function(x) lapply(x, function(y) rep(1,length(y))))
      out <- matrix(0, nrow = length(par)+1, ncol = length(par)+1)
      # probabilities_ii <- lapply(probabilities_i,unlist)
      g012 <- mapply(FUN = esf_mst_sum_vector, parlist = par_i,
                     ojlist = oj_i, order = 2, minSolved = minSolved_i, maxSolved = maxSolved_i,
                     minSolved_design = minSolved_stage_i, maxSolved_design = maxSolved_stage_i, probs = probabilities_i, cumulative = cumulative_i, SIMPLIFY = FALSE)
      ## loop over observed NA patterns      
      for(nai in seq(nrow(mstdesign))) {
        min_nai <- minSolved_stage_i[[nai]] + 1
        max_nai <- maxSolved_stage_i[[nai]] + 1
        min_nai <- tail(min_nai,n=1)
        max_nai <- tail(max_nai,n=1)
        
        g0_i <- g012[[nai]][[1]][min_nai:max_nai]
        g1_i <- g012[[nai]][[2]][min_nai:max_nai,]
        g2_i <- g012[[nai]][[3]][min_nai:max_nai,,]
        rf_ii <- rf_i[[nai]][min_nai:max_nai]
        
        i_i <- ncol(y_i[[nai]])
        g1dg0 <- g1_i/g0_i
        hessian <- matrix(0, nrow = length(par_n), ncol = length(par_n))
        for (ii in seq_len(i_i)){
            hessian[items_i[[nai]][ii],items_i[[nai]]] <- (rf_ii %*% (g2_i[,ii,]/g0_i - (g1_i[,ii]/g0_i) * g1dg0))
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
    names(betapar) <- paste0("est.b_",names_dat)
   
      if(se) {
        se.beta <- sqrt(diag(desmat %*% solve(fit$hessian) %*% t( desmat )))
        names(se.beta) <- paste0("se.b_",names_dat)
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
    names(betapar) <- paste0("est.b_",names_dat)
   
      if(se) {
        se.beta <- sqrt(diag(desmat %*% solve(ahessian(fit$par)) %*% t( desmat )))
        names(se.beta) <- paste0("se.b_",names_dat)
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
