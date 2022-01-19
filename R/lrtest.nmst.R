lrtest.nmst <- 
function(object, split = "median", cores = NULL, se = TRUE, ...) {
call <- match.call()

data_orig <- object$data
# use.method lrtest

if(!is.null(cores)){
  if(cores > parallel::detectCores()){
    stop(paste0("You specified more cores than your computer have. Pleas change to equal or lower of: ",
    parallel::detectCores()," cores \n"))
  } 
  cl <- parallel::makeCluster(cores)
}

# split name
# split categories
if(any(is.na(object$data))){
  if(length(split) == 1 & any(split %in% c("mean","median"))) {
    na_patterns <- factor(apply(is.na(object$data), 1, function(x) paste(which(x), collapse = "\r")))
    #na_pattern_levels <- levels(na_patterns)
    sum_i <- rowSums(object$data,na.rm = TRUE)
    order_i <- order(na_patterns)
    sum_i <- sum_i[order_i]
    na_patterns <- na_patterns[order_i]
    object$data <- object$data[order_i,]

    if (split == "mean") {
      mean_ig <- sapply(split(sum_i, na_patterns), function(x){
       x_mean <- mean(x)
       ifelse(x <= x_mean, 1, 2)
      })
      split_i <- factor(unlist(mean_ig), 
                      levels = c(1,2), 
                      labels = c("low score <= [mean]","high score  > [mean]"))
    }
    if (split == "median") {
      median_ig <- sapply(split(sum_i, na_patterns), function(x){
        x_median <- stats::median(x)
        ifelse(x <= x_median, 1, 2)
       })
       split_i <- factor(unlist(median_ig), 
                       levels = c(1,2), 
                       labels = c("low score <= [median]","high score > [median]"))
    }
  } else {
    if(!length(split) == nrow(object$data)){
      stop("The submitted split vector does not match the amount of persons in your data\n")
    } 
    if(length(unique(split)) != 2){
      stop("Pleas submit a dichotomous split vector!\n")
    }

    split_i <- as.factor(as.character(split))
  }
} else {
  if (length(split) == 1 & any(split %in% c("mean","median"))) {
    sum_i <- rowSums(object$data)
    if (split == "mean") {
      mean_ig <- ifelse(sum_i <= mean(sum_i), 1, 2)
      split_i <- factor(mean_ig, 
                      levels = c(1,2), 
                      labels = c("low score <= [mean]","high score  > [mean]"))
    }
    if (split == "median") {
      median_ig <- ifelse(sum_i <= stats::median(sum_i), 1, 2)
      split_i <- factor(median_ig, 
                      levels = c(1,2), 
                      labels = c("low score <= [median]","high score > [median]"))
    }
  } else {
    if(!length(split) == nrow(object$data)){
      stop("The submitted split vector does not match the amount of persons in your data\n")
    } 
    if(length(unique(split)) != 2){
      stop("Please submit a dichotomous split vector!\n")
    }
    split_i <- as.factor(as.character(split))
  }
}

# Split the data after (generated) grouping vector
datalist <- split.data.frame(object$data, split_i) # 3 x faster than by 
names(datalist) <- levels(split_i)

#----------item to be deleted---------------
deleted_items <- lapply(datalist, function(x) {
                      data_check(dat = x)$status  #items to be removed within subgroup
                    })

deleted_item <- unique(unlist(deleted_items))
if (length(deleted_item) >= (ncol(object$data)-1)) {
  stop("\nNo items with appropriate response patterns left to perform LR-test!\n")
}

if(length(deleted_item) > 0){
  warning(paste0(
    cat("\nThe following items were excluded due to inappropriate response patterns within subgroups: \n"),
    paste(deleted_item, collapse=" "),
    cat("\nFull and subgroup models are estimated without these items!\n")
  ), immediate.=TRUE)
}


if (length(deleted_item) > 0) {
  object$data <- object$data[,!colnames(object$data)%in%deleted_item]
  datalist <- split.data.frame(object$data, split_i) # 3 x faster than by 
  datalist[[3]] <- object$data # add for basis model
  names(datalist) <- c(levels(split_i),"basis")
} 

if(!is.null(cores)){
  if (object$model == "Rasch model (nmst)") {
    parallel::clusterExport(cl, list("raschmodel.nmst"), envir = environment(lrtest.nmst))
    likpar <- parallel::parSapply(cl,datalist,function(x) {
                               objectg <- raschmodel.nmst(x, se = se, ...)
                               likg <- objectg$loglik
                               nparg <- length(objectg$betapar)
                               betapar <- objectg$betapar
                               se <- objectg$se.beta
                               list(likg,nparg,betapar,se,objectg) 
                               })
    on.exit(parallel::stopCluster(cl))
  }
}else{
  if (object$model == "Rasch model (nmst)") {
    likpar <- sapply(datalist,function(x) {
                     objectg <- raschmodel.nmst(x, se = se, ...)
                     likg <- objectg$loglik
                     nparg <- length(objectg$betapar)
                     betapar <- objectg$betapar
                     se <- objectg$se.beta
                     list(likg,nparg,betapar,se,objectg) 
                    })
  }
}

if (length(deleted_item) > 0) {
loglik_basis <- unlist(likpar[1,"basis"])
betapar_basis <- likpar[,"basis"][[3]]
} else {
  loglik_basis <- object$loglik
  betapar_basis <- object$betapar
}

fitobj <- likpar[5, ]
likpar <- likpar[-5,]


loglikg <- sum(unlist(likpar[1,unique(split_i)]))
LRvalue <- 2*(abs(loglikg-loglik_basis))
df <- sum(unlist(likpar[2,unique(split_i)]))-(length(betapar_basis)) - 1
pvalue <- 1 - stats::pchisq(LRvalue, df)
betapars_subgroup <- likpar[3,unique(split_i)]
separs_subgroup <- likpar[4,unique(split_i)]

result <- list(data_orig = data_orig, 
               betapars_subgroup = betapars_subgroup,
               se.beta_subgroup = separs_subgroup,
               model = object$model,
               LRvalue = LRvalue,
               df = df, 
               pvalue = pvalue, 
               loglik_subgroup = unlist(likpar[1,unique(split_i)], use.names = FALSE),
               split_subgroup = split_i, 
               call = call, 
               fitobj = fitobj)
return(result)
}
