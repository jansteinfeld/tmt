#' Function for the Simulation of Multistage-Designs
#' 
#' This function simulates data after a multistage design. The subjects are drawn from a normal 
#' distribution with specified mean and standard deviation (default N (0,1)). As an additional argument, 
#' a seed can also be passed.
#' 
#' @param mstdesign definition of desired multistage design
#' @param persons amount of persons per starting module
#' @param items vector of difficulty parameters for each items
#' @param mean optional mean for person parameter; default = 0
#' @param sd optional sd for person parameter; default = 1
#' @param ... further optional arguments like \code{set.seed}
#'
#' @author Jan Steinfeld
#' 
#' @importFrom stats rnorm
#' 
#' @return List with following entries
#' 
#'  \item{data}{Matrix with item responses}
#'  \item{data_mst}{Data frame with item responses and additional a vector of used modules per person}
#'  \item{persons}{Generated and used person parameters}
#'  \item{mstdesign}{Submitted multistage design}
#' 
#' 
#' @example ./R/.example_designsim.R
#' 
#' @export
tmt_sim <- function(mstdesign = NULL, 
  items = NULL, persons = NULL, mean = 0, sd = 1,...){

  # sanitary checks
  # are there a start module, stages and maxSolved module
  if (is.null(mstdesign)) stop("mstdesign needs to be specified! \n")
  if (is.null(items)) stop("vector of difficulty parameter is required")
  if (is.null(persons)) stop("integer of persons for each starting module is required")

  input <- tmt_mstdesign(mstdesign = mstdesign, options = c("simulation", "items","start"))
  design <- input$simulation
  items_d <- input$items
  start_b <- input$start

  if (!all(names(items) %in% items_d & items_d %in% names(items))) {
    stop("the specified items in the mstdesign and in the vector are different")
  }

    # 2019-03-14
  # check input values
  if(length(persons) < nrow(start_b)){
    warning("The submitted amount of persons is used for each starting module")
    persons <- rep(persons,nrow(start_b))
  }  
    
  # check input for persons: either integer or specified thetas
  if (!is.list(persons) & length(persons)==nrow(start_b)) {
    if(length(mean) < nrow(start_b)){
      warning("The submitted value for 'mean' is used for all persons")
      mean <- rep(mean,nrow(start_b))
    }  
    if(length(sd) < nrow(start_b)){
      warning("The submitted value for 'sd' is used for all persons")
      sd <- rep(sd,nrow(start_b))
    } 
    n <- sum(persons)
    n_n <- persons
    ppar <- sapply(persons, FUN = function(x) stats::rnorm(x, mean, sd), simplify = FALSE, USE.NAMES = FALSE)
    ppar <- as.list(ppar)
    # mapply(":",c(1,persons[-length(persons)]+1),cumsum(persons), simplify = FALSE)
    p_pos_start <- c(1,persons[-length(persons)]+1)
    p_pos_end <- cumsum(persons)
  } else if (!is.list(persons) & length(persons) > nrow(start_b) & !all(unlist(persons)%%1==0)) {
    n_n <- n <- length(persons)
    ppar <- list(persons)
    p_pos_start <- c(1)
    p_pos_end <- n
  } else if (is.list(persons) & !all(unlist(persons)%%1==0)) {
    n <- length(unlist(persons))
    n_n <- sapply(persons,length)
    p_pos_start <- c(1,cumsum(n_n)[-length(n_n)]+1)
    p_pos_end <- cumsum(n_n)
    ppar <- persons
  } else {
    stop("The passed value for 'persons' does not match the design")
  }
 
    mat <- data.frame(matrix(NA, nrow = n, ncol = length(items) + 1))
    colnames(mat) <- c("branching",items_d )
    mat[,"branching"] <- rep(unique(design[["start"]][,"from"]), times = n_n)
    helper_stages <- vector(mode="character", length=nrow(mat))
    
    for (s in seq_len(length(design))) { # start with 2 because in first entry are the starting values
      # only once for the start module
      if (s == 1) {
        for (ss in seq_along(unique(mat[,"branching"]))) {
          sb <- unique(mat[,"branching"])[ss]

          items_i <- unique(design[["start"]][design[["start"]][,"from"] == sb,"items_to"])
          items_i <- unlist(strsplit(items_i,","))
          mat[p_pos_start[ss]:p_pos_end[ss] , items_i] <- sim.rm(theta = ppar[[ss]],
                                                        b = items[names(items) %in% items_i],...)
          helper_stages[p_pos_start[ss]:p_pos_end[ss]] <- sb
        }
        ppar <- unlist(ppar)
      } else {
        # get information from last stage and store this information
        stage_last <- helper_stages
        helper_stages <- vector(mode="character", length=nrow(mat))
        for (sl in unique(stage_last)) {
          # Items from stage before
          persn_sl <- stage_last == sl

          items_i <- unique(design[[s]][design[[s]][,"from"] == sl,"items_from"])
          items_i <- unlist(strsplit(items_i,","))
          # sums of stage before
          rs_sl <- rowSums(mat[persn_sl, items_i])

          maxSolved <- as.numeric(design[[s]][design[[s]][,"from"] == sl,"maxSolved"])
          if (any(max(maxSolved) > length(items_i))) {
            stop("Within the design matrix an divergent amount of maxSolved items are specified\n")
          }
          # now get information from and to based on solved items
          # groups are the position of the next module within the design file
          groups_n <- cut(rs_sl, sort(c(0,maxSolved)), include.lowest = TRUE, labels = 1:length(maxSolved) )
          for (g in unique(groups_n)) {
            item_g <- design[[s]][design[[s]][,"from"] == sl &
                                    design[[s]][,"maxSolved"] == sort(maxSolved)[as.numeric(g)],"items_to"]
            item_g <- as.character(unlist(strsplit(item_g, ",")))
            to <- design[[s]][design[[s]][,"from"] == sl & design[[s]][,"maxSolved"] == sort(maxSolved)[as.numeric(g)], "to"]
            mat[persn_sl, "branching"][groups_n %in% g] <- paste0(mat[persn_sl, "branching"][groups_n %in% g],"-",to)
            helper_stages[persn_sl][groups_n %in% g] <- to
            # simulate again, based on existing person parameters
            mat[persn_sl, item_g][groups_n %in% g,] <- sim.rm(theta = ppar[persn_sl][groups_n %in% g], b = items[item_g],...)
          }
        }
      } # end else
    }  # end for (s in 1:length(design)) {
    mat_mst <- mat
    mat <- apply(mat[,-1],2,as.numeric)
    
    out <- list(data = mat, 
              data_mst = mat_mst, 
              persons = ppar, 
              mstdesign = mstdesign)

    class(out) <- append(class(out),"mstdesign")
    return(out)
}
