# Simulation of Rasch model data

sim.rm <- function(theta, b, seed = NULL){

  seed_beta <- seed_theta <- NULL
  

  if (((length(theta) + length(b) )== 2) && !is.null(seed) && length(seed) != 2) {

    warning("It is necessary to set a seed for both theta and beta if no vector is passed. The specified seed was used for the theta parameters, the following is used for beta: ", seed + 1)
    #> Warning: This is what a warning looks like
    seed_theta <- seed
    seed_beta <- seed + 1
  }

  if (((length(theta) + length(b)) == 2) && length(seed) == 2) {
    message("The specified seeds were used for the theta and beta parameters: ", seed[1], " and ", seed[2])
    seed_theta <- seed[1]
    seed_beta <- seed[2]
  }

  if ((length(theta) == 1 && length(b) != 1) && length(seed) == 1) { 
    message("The specified seed will be used for the theta parameters")
    seed_theta <- seed
  }
  
  if ((length(theta) != 1 && length(b) == 1)  && length(seed) == 1) {
    message("The specified seed will be used for the beta parameters")
    seed_beta <- seed
  }

  # cat("seed sim: ",seed)
  
	if(length(theta) == 1) {
    if(!is.null(seed_theta)) {
      set.seed(seed_theta)
    }
    theta <- stats::rnorm(theta, mean = 0, sd = 1)	
	}

	if(length(b) == 1) {
    if(!is.null(seed_beta)) {
      set.seed(seed_beta)
    }
    b <- stats::rnorm(b, mean = 0, sd = 1)	
	}
	# if(!is.null(seed)){
		# set.seed(seed)
		# resp <- outer(theta, b, "-")
		# p_exp <- exp(resp)
		# prop_solve <- p_exp/(1 + p_exp)
		# dat.resp <-(prop_solve > matrix(stats::runif(length(b)*length(theta)),length(theta),length(b)))*1
	# } else {
		resp <- outer(theta, b, "-")
		p_exp <- exp(resp)
		prop_solve <- p_exp/(1 + p_exp)
		dat.resp <-(prop_solve > matrix(stats::runif(length(b)*length(theta)),length(theta),length(b)))*1
	# }
	if(!is.null(names(b))){
		colnames(dat.resp) <- names(b)
	}
return(dat.resp)
}
