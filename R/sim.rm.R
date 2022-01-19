# Simulation of Rasch model data

sim.rm <- function(theta, b, seed = NULL){

	if(length(theta) == 1){
		if(!is.null(seed)){
			set.seed(seed)
			theta <- stats::rnorm(theta, mean = 0, sd = 1)	
		}else{
			theta <- stats::rnorm(theta, mean = 0, sd = 1)	
		}
		theta <- theta	
	}

	if(length(b) == 1){
		if(!is.null(seed)){
			set.seed(seed)
			b <- stats::rnorm(b, mean = 0, sd = 1)	
		}else{
			b <- stats::rnorm(b, mean = 0, sd = 1)	
		}
		b <- b	
	}

	if(!is.null(seed)){
		set.seed(seed)
		resp <- outer(theta, b, "-")
		p_exp <- exp(resp)
		prop_solve <- p_exp/(1 + p_exp)
		dat.resp <-(prop_solve > matrix(stats::runif(length(b)*length(theta)),length(theta),length(b)))*1
	} else {
		resp <- outer(theta, b, "-")
		p_exp <- exp(resp)
		prop_solve <- p_exp/(1 + p_exp)
		dat.resp <-(prop_solve > matrix(stats::runif(length(b)*length(theta)),length(theta),length(b)))*1
	}
	if(!is.null(names(b))){
		colnames(dat.resp) <- names(b)
	}
return(dat.resp)
}
