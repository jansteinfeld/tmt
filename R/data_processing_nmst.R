data_processing_nmst <- function(dat, weights){
## process NA patterns and calculate static things once
  i <- ncol(dat)
  na_patterns <- factor(apply(is.na(dat), 1, function(x) paste(which(x), collapse = "\r")))
  y_i <- na_i <- wi_i <- wi2_i <- cs_i <- rs_i <- rf_i <- k_i <- vector("list", nlevels(na_patterns))
  # na_pattern_levels <- levels(na_patterns)
  na_pattern_levels <- unique(na_patterns)
  for (nai in seq_along(na_pattern_levels)) {
    ## parse NA pattern
    na_level_i <- as.character(na_pattern_levels[nai])
    wi_i[[nai]] <- as.integer(strsplit(na_level_i, "\r")[[1]])
    wi2_i[[nai]] <- ifelse(length(wi_i[[nai]]) < 1, 1:i , (1:i)[-wi_i[[nai]]])
    k_i[[nai]] <- length(wi2_i[[nai]])

    ## select subset
    na_i[[nai]] <- which(na_patterns == na_level_i)
    if(length(wi_i[[nai]]) < 1){
      y_i[[nai]] <- dat[na_i[[nai]], , drop = FALSE]
      }else{
        y_i[[nai]] <- dat[na_i[[nai]], -wi_i[[nai]], drop = FALSE]
      }
    weights_i <- weights[na_i[[nai]]] 
    cs_i[[nai]] <- colSums(y_i[[nai]] * weights_i)
    rs_i[[nai]] <- rowSums(y_i[[nai]])
    rf_i[[nai]] <- as.vector(tapply(weights_i, factor(rs_i[[nai]], 
                      levels = 0:ncol(y_i[[nai]])), sum))
    rf_i[[nai]][is.na(rf_i[[nai]])] <- 0
  }

  res <- list(
    na_patterns = na_patterns,
    y_i = y_i, 
    na_i = na_i,
    wi_i = wi_i,
    wi2_i = wi2_i,
    cs_i = cs_i,
    rs_i = rs_i,
    rf_i = rf_i,
    k_i = k_i
    )

  return(res)
}
