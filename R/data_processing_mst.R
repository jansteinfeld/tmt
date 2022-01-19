data_processing_mst <- function(dat, mstdesign, weights, precondition, cumulative){
 cumulative_i <- probabilities_i <- y_i <- na_p <- items_i <- items_l <- minSolved_i <- minSolved_stage_i <- maxSolved_i <- maxSolved_stage_i <- cs_i <- rs_i <- rf_i <- vector("list", nrow(mstdesign))
 # do.call(paste0,branches_new)
 # 2020-11-15 added precondition do the data processing function. If precondition != NULL generate list of precondition
 preconpar <- preconnum <- NULL
 preconsize <- 0
 if (!is.null(precondition)) {
   preconpar <- t(precondition[,grep("precondition",colnames(precondition)),drop=FALSE])
   preconnum <- t(precondition[,grep("value",colnames(precondition)), drop = FALSE])
   preconsize <- nrow(preconpar)
 }

  for (mst in seq_len(nrow(mstdesign))) {
    items_m <- as.list(strsplit(mstdesign[mst, "items"],";")[[1]])
    cumulative_i[[mst]] <- rep(cumulative,length(items_m))
    # 2020-05-16 added preconsize do get info of item position in data.frame without precondition
    items_i[[mst]] <- match(unlist(strsplit(unlist(items_m),",")),colnames(dat)) - preconsize
    items_l[[mst]] <- lapply(strsplit(unlist(items_m),","),FUN = function(x){
                        match(x,colnames(dat)) - preconsize
                      })
    minSolved_i[[mst]] <- as.numeric(strsplit(mstdesign[mst, "minSolved"],";")[[1]])
    minSolved_stage_i[[mst]] <- as.numeric(strsplit(mstdesign[mst, "minSolved_stage"],";")[[1]])
    maxSolved_i[[mst]] <- as.numeric(strsplit(mstdesign[mst, "maxSolved"],";")[[1]])
    maxSolved_stage_i[[mst]] <- as.numeric(strsplit(mstdesign[mst, "maxSolved_stage"],";")[[1]])
    probabilities_i[[mst]] <- unlist(lapply(strsplit(mstdesign[mst, "probability"],";")[[1]],FUN = function(x) as.numeric(strsplit(x,",")[[1]])))

    # 2020-05-15 adapted for precondition
    # get persons for Items
    na_p[[mst]] <- rowSums(!is.na(dat[,items_i[[mst]] + preconsize]))==length(items_i[[mst]])

    if (!is.null(precondition)) {
      preconsub <- paste0("dat[,'", preconpar[,mst],"']", "==", preconnum[,mst], collapse="&")
      na_p[[mst]] <- na_p[[mst]] & eval(parse(text = preconsub))
    } 

    y_i[[mst]] <- dat[na_p[[mst]], items_i[[mst]] + preconsize, drop = FALSE]
    weights_i <- weights[na_p[[mst]]]
    cs_i[[mst]] <- colSums(y_i[[mst]] * weights_i)
    rs_i[[mst]] <- rowSums(y_i[[mst]])
    rf_i[[mst]] <- as.vector(tapply(weights_i, factor(rs_i[[mst]], 
            levels = 0:ncol(y_i[[mst]])), sum))
    rf_i[[mst]][is.na(rf_i[[mst]])] <- 0
  }
  
  res <- list(
        y_i = y_i,
        na_p = na_p,
        items_i = items_i,
        items_l = items_l,
        minSolved_i = minSolved_i,
        minSolved_stage_i = minSolved_stage_i,
        maxSolved_i = maxSolved_i,
        maxSolved_stage_i = maxSolved_stage_i,
        cs_i = cs_i,
        rs_i = rs_i,
        rf_i = rf_i,
        probabilities_i = probabilities_i,
        cumulative_i = cumulative_i
    )
return(res)
}
