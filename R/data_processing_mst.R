data_processing_mst <- function(dat, mstdesign, weights){
 y_i <- na_p <- items_i <- items_l <- minSolved_i <- minSolved_stage_i <- maxSolved_i <- maxSolved_stage_i <- cs_i <- rs_i <- rf_i <- vector("list", nrow(mstdesign))
  for (mst in seq_len(nrow(mstdesign))) {
    items_m <- as.list(strsplit(mstdesign[mst, "items"],";")[[1]])
    items_i[[mst]] <- match(unlist(strsplit(unlist(items_m),",")),colnames(dat))
    items_l[[mst]] <- lapply(strsplit(unlist(items_m),","),FUN = function(x){
                        match(x,colnames(dat))
                      })
    minSolved_i[[mst]] <- as.numeric(strsplit(mstdesign[mst, "minSolved"],";")[[1]])
    minSolved_stage_i[[mst]] <- as.numeric(strsplit(mstdesign[mst, "minSolved_stage"],";")[[1]])
    maxSolved_i[[mst]] <- as.numeric(strsplit(mstdesign[mst, "maxSolved"],";")[[1]])
    maxSolved_stage_i[[mst]] <- as.numeric(strsplit(mstdesign[mst, "maxSolved_stage"],";")[[1]])
    na_p[[mst]] <- rowSums(!is.na(dat[,items_i[[mst]]]))==length(items_i[[mst]])
    y_i[[mst]] <- dat[na_p[[mst]], items_i[[mst]], drop = FALSE]
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
        rf_i = rf_i
    )
return(res)
}
