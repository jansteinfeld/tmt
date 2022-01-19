#' Function for the Simulation of Multistage-Designs
#'
#' This function simulates data according to the specified and submitted multistage design. The persons are drawn from a standard normal distribution if the amount of persons are specified. As an additional argument, a seed can also be set. If requested, it is also possible to submit a vector ore list of person parameters to specify different person distributions.
#'
#' @param mstdesign definition of desired multistage design
#' @param items vector of difficulty parameters for each items
#' @param persons amount of persons per starting module
#' @param preconditions definition of preconditions can optionally be specified. In the case of probabilistic routing preconditions such as a pre-test, which are taken into account in the MST design. For the specification the correlation with the true person parameter have to be specified. The submitted correlation is adjusted in the function according to Demirtas and Yavuz (2015; <doi:10.1080/10543406.2014.920868>) It is also possible to submit your own vector with integers for the preconditions.
#' @param ... further optional arguments like setting a \code{seed}
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
#' @references
#'\itemize{
#'  \item Demirtas, H., & Yavuz, Y. (2015). Concurrent Generation of Ordinal and Normal Data. \emph{Journal of Biopharmaceutical Statistics}, 25(4), 635-650. https://doi.org/10.1080/10543406.2014.920868
#' }
#' 
#' 
#' @example ./R/.example_designsim.R
#'
#' @export
#' 
tmt_sim <- function(mstdesign = NULL,
                    items = NULL, persons = NULL, preconditions = NULL, ...) {

  # check for manual seed
  additional_arguments <- list(...)
  seed <- NULL
  if (!is.null(additional_arguments$seed)) seed <- additional_arguments$seed
  if (!is.null(additional_arguments$mean)) warning("argument 'mean' is deprecated")
  if (!is.null(additional_arguments$sd)) warning("argument 'sd' is deprecated")

  # sanitary checks
  # are there a start module, stages and maxSolved module
  if (is.null(mstdesign)) stop("mstdesign needs to be specified! \n")
  if (is.null(items)) stop("vector of difficulty parameter is required")
  # if (is.null(persons)) stop("integer of persons for each starting module is required")

  input <- tmt_mstdesign(mstdesign = mstdesign, options = c("simulation", "items"))
  design <- input$simulation
  items_d <- input$items
  start_b <- input$simulation$start



  # Update 2020-04-28 preconditions
  if (!is.null(input$preconditions)) {
    # precon_d <- input$preconditions$rules
    # precon_b <- input$preconditions$paths
    precon_start <- input$preconditions$preconditions
  }

  if (!is.null(preconditions) & is.null(input$preconditions)) {
    warning("No preconditions were specified in the mstdesign, but passed as a function argument. The 'preconditions' argument is set to 'NULL'.")
    # preconditions has to be set to NULL, because the information has already been created
    preconditions <- NULL
  }

  if (is.null(preconditions) & !is.null(input$preconditions)) {
    stop("Please specify a correlation for the preconditions")
  }

  # generate preconditions vector for generating variables
  if (!is.null(preconditions) & (sum(unlist(persons)) != length(sum(unlist(preconditions))))) {
    precon_corr <- preconditions
  if (length(preconditions) < nrow(input$preconditions$precondition_matrix)) {
    warning("The specified correlation in 'preconditions' was used for all starting groups.")
    # message("The specified correlation in 'preconditions' was used for all starting groups")
    precon_corr <- rep(precon_corr, length.out = nrow(input$preconditions$precondition_matrix))
  } else if (length(preconditions) >  nrow(input$preconditions$precondition_matrix)) {
    warning(paste0("The first ",nrow(input$preconditions$precondition_matrix)," specified correlation in 'preconditions' were used."))
    # message("The specified correlation in 'preconditions' was used for all starting groups")
    precon_corr <- rep(precon_corr, length.out = nrow(input$preconditions$precondition_matrix))
  }
  
  # precon_start muss noch aufbereitet werden...
  precon_values <- grep("value", colnames(precon_start))

  precon_start[, precon_values] <- apply(precon_start[, precon_values, drop = FALSE], 2, function(x) as.numeric(gsub("\\(|\\)", "", x)))
  precon_n <- grep("precondition", colnames(precon_start))

    preconditions <- list()
    for (ii in precon_n) {
      preconditions_mat <- matrix(NA, ncol = 3, nrow = 1)
      colnames(preconditions_mat) <- c("min", "max", "r")
      i <- input$preconditions$precondition_matrix[,"name"] %in% unique(precon_start[, ii])
      preconditions_mat[, 1] <- as.numeric(input$preconditions$precondition_matrix[i,"min"])
      preconditions_mat[, 2] <- as.numeric(input$preconditions$precondition_matrix[i,"max"])
      preconditions_mat[, 3] <- precon_corr[i]
      preconditions[[unique(precon_start[, ii])]] <- preconditions_mat
    }
  }

  if (nrow(start_b) != length(persons)) {
    if(is.list(persons)) {
      warning(paste0(length(persons)," groups of Persons were specified, but only one starting group, the groups were merged"))
      persons <- unlist(persons)
    } else if (all(persons%%1==0) & length(unique(persons)) == 1) {
      persons <- unique(persons)
    } else if (!all(persons%%1<1 & persons%%1!=0)) {
      stop("Please specify a unique number of desired persons for the data simulation. A vector with different amounts of persons is currently only supported for several start groups.")
    }
  }
  
  # generate person parameter and if desired also sum score for preconditions
  personinfo <- precon_sim(ppar = persons, precon = preconditions, seed = seed)
  persons <- personinfo$perspar
  precon <- personinfo$preconpar


  if (is.null(names(items))) {
    if (length(items) == length(items_d)) {
      warning("The provided item parameters are submitted without names, this was subsequently made up (in ascending order according to the information in the design)")
      names(items) <- items_d
    }
  }
  if (!all(items_d %in% names(items))) stop("vector of 'named' item parameter is required")

  if(length(items) != length(items_d)) {
    stop("The number of specified items in 'items' does not match the MST design.")
  } 

  # # 2019-03-14
  # # check input values
  # if(length(persons) < nrow(start_b)){
  #   warning("The submitted amount of persons is used for each starting module")
  #   persons <- rep(persons,nrow(start_b))
  # }

  if (length(persons) < nrow(start_b)) {
    stop("Please specify as many groups of person parameters as start blocks are defined")
  }

  n <- sum(lengths(persons))
  # n_n <- sapply(persons,length)
  n_n <- lengths(persons)
  p_pos_start <- c(1, cumsum(n_n)[-length(n_n)] + 1)
  p_pos_end <- cumsum(n_n)
  ppar <- persons
  preconpar <- precon


  if (!is.null(preconditions)) {
    mat <- matrix(NA, nrow = n, ncol = length(items) + 1 + length(precon))
    inp <- do.call(cbind, precon)
    mat[, seq(precon) + 1] <- as.matrix(inp)
    colnames(mat) <- c("branching", names(preconditions), items_d)
    mat <- data.frame(mat)
  } else {
    mat <- data.frame(matrix(NA, nrow = n, ncol = length(items) + 1))
    colnames(mat) <- c("branching", items_d)
  }
  mat[, "branching"] <- rep(unique(design[["start"]][, "from"]), times = n_n)
  helper_stages <- vector(mode = "character", length = nrow(mat))

  for (s in seq_len(length(design))) {
    # only once for the start module
    if (s == 1) {
      for (ss in seq_along(unique(mat[, "branching"]))) {
        sb <- unique(mat[, "branching"])[ss]

        items_i <- unique(design[["start"]][design[["start"]][, "from"] == sb, "items_to"])
        items_i <- unlist(strsplit(items_i, ","))
        mat[p_pos_start[ss]:p_pos_end[ss], items_i] <- sim.rm(
          theta = ppar[[ss]],
          b = items[names(items) %in% items_i],
          seed = seed
        )
        helper_stages[p_pos_start[ss]:p_pos_end[ss]] <- sb
      }
      ppar <- unlist(ppar, use.names = FALSE)
      # 2020-05-13 the preconditions are always added but only taken into account if the design is cumulative. 
      # If so they appear in the 'from' column in the submitted and processed design information
      # if(is.list(preconpar)) preconpar <- unlist(preconpar, use.names = FALSE)
    } else { # start with 2 because in first entry are the starting values
      # get information from last stage and store this information
      stage_last <- helper_stages
      helper_stages <- vector(mode = "character", length = nrow(mat))
      for (sl in sort(unique(stage_last))) {
        # Items from stage before
        persn_sl <- stage_last == sl

        items_i <- unique(design[[s]][design[[s]][, "from"] == sl, "items_from"])
        items_i <- unlist(strsplit(items_i, ","))
        # sums of stage before


        # calculate the sum score. If there are preconditions, then this has to be adjusted here
        probabilities <- design[[s]][design[[s]][, "from"] == sl, "probability"]
        probabilities <- as.numeric(unlist(sapply(probabilities, strsplit, ",")))
        maxSolved <- as.numeric(design[[s]][design[[s]][, "from"] == sl, "maxSolved"])
        minSolved <- as.numeric(design[[s]][design[[s]][, "from"] == sl, "minSolved"])

        if (is.null(preconpar) & (any(max(maxSolved) > length(items_i)))) {
          stop("Within the design matrix an divergent amount of maxSolved items are specified\n") # nolint
        }

        rs_sl <- rowSums(mat[persn_sl, items_i])
        # if(!is.null(preconpar) & !all(probabilities == 1)) rs_sl <- rs_sl + preconpar[persn_sl] # nolint

        # now get information from and to based on solved items
        # groups are the position of the next module within the design file
        if (all(probabilities == 1)) {
          # 2021-12-19 added, for cases with two or lower amounts of items in one module
          if ( any(maxSolved - minSolved == 0) ) {
              if(maxSolved[maxSolved - minSolved == 0] == 0){
                groups_n <- rep(NA,length(rs_sl))
                minequalmax <- maxSolved[maxSolved-minSolved==0]
                groups_n[rs_sl %in% minequalmax] <- seq_along(minequalmax)
                newcat <- c(0, maxSolved)[!c(0, maxSolved) %in% minequalmax]
                groups_n <- factor(groups_n, levels = seq_along(c(newcat,length(minequalmax))))
                groups_n[!rs_sl %in% minequalmax] <- cut(rs_sl[!rs_sl %in% minequalmax], sort(c(0,newcat)), include.lowest = TRUE, labels = seq_along(newcat) + length(minequalmax)) # nolint  
              } else {
                groups_n <- cut(rs_sl, sort(c(0, maxSolved)), include.lowest = TRUE, labels = seq_along(maxSolved)) # nolint  
              }
          } else {
            groups_n <- cut(rs_sl, sort(c(0, maxSolved)), include.lowest = TRUE, labels = seq_along(maxSolved)) # nolint  
          }
          # groups_n <- cut(rs_sl, sort(c(0, maxSolved)), include.lowest = TRUE, labels = seq_along(maxSolved)) # nolint
          for (g in sort(unique(groups_n))) {
            item_g <- design[[s]][design[[s]][, "from"] == sl &
              design[[s]][, "maxSolved"] == sort(maxSolved)[as.numeric(g)], "items_to"] # nolint
            item_g <- as.character(unlist(strsplit(item_g, ",")))
            to <- design[[s]][design[[s]][, "from"] == sl & design[[s]][, "maxSolved"] == sort(maxSolved)[as.numeric(g)], "to"] # nolint
            mat[persn_sl, "branching"][groups_n %in% g] <- paste0(mat[persn_sl, "branching"][groups_n %in% g], "-", to) # nolint
            helper_stages[persn_sl][groups_n %in% g] <- to
            # simulate again, based on existing person parameters
            mat[persn_sl, item_g][groups_n %in% g, ] <- sim.rm(
              theta = ppar[persn_sl][groups_n %in% g], # nolint
              b = items[item_g], # nolint
              seed = seed
            )
          }
        } else { # probability based routing
          # 2019-11-15 added feature for probabilistic routing
          probabilities <- design[[s]][design[[s]][, "from"] == sl, "probability"] # nolint

          probabilities <- do.call(rbind, sapply(probabilities, strsplit, ","))
          probabilities <- apply(probabilities, 2, as.numeric)
          if (!all(colSums(probabilities) == 1)) warning("For each raw score, the probabilities do not add up to 1\n") # nolint

          groups_n <- rep(NA, length(rs_sl))
          seq_gg <- ifelse(min(minSolved) == 0, 1, 0)

          for (gg in seq(min(minSolved), max(maxSolved))) {
            groups_n[rs_sl == gg] <- sample(design[[s]][design[[s]][, "from"] == sl, "to"], size = sum(rs_sl == gg), prob = probabilities[, gg + seq_gg], replace = TRUE) # nolint
          }
          for (ggg in sort(unique(groups_n))) {
            item_g <- design[[s]][design[[s]][, "from"] == sl &
              design[[s]][, "to"] == ggg, "items_to"]
            item_g <- as.character(unlist(strsplit(item_g, ",")))
            # to <- design[[s]][design[[s]][,"from"] == sl & design[[s]][,"to"] == ggg, "to"] # nolint
            mat[persn_sl, "branching"][groups_n %in% ggg] <- paste0(mat[persn_sl, "branching"][groups_n %in% ggg], "-", ggg) # nolint
            helper_stages[persn_sl][groups_n %in% ggg] <- ggg
            # simulate again, based on existing person parameters
            mat[persn_sl, item_g][groups_n %in% ggg, ] <- sim.rm(
              theta = ppar[persn_sl][groups_n %in% ggg], # nolint
              b = items[item_g], # nolint
              seed = seed
            ) # nolint
          }
        }
      }
    } # end else
  } # end for (s in 1:length(design)) {
  mat_mst <- mat
  mat <- apply(mat[, -1], 2, as.numeric)

  out <- list(
    data = mat,
    data_mst = mat_mst,
    persons = ppar,
    mstdesign = mstdesign,
    preconditions = preconditions,
    preconpar = preconpar
  )

  class(out) <- append(class(out), "mstdesign")
  return(out)
}
