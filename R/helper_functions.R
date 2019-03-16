# helper functions

#########################################################################
hfun.blocks <- function(tmtd, n.branches, n.blocks, n.start){
  # definition of block and items
  blocks <- matrix(0, ncol = 2, nrow = n.blocks + n.start)
  colnames(blocks) <- c("from","items")
  # -----------------------------------
  # helper matrix for the definition of block and its dedicated items
  for (i in grep("=~", tmtd, fixed = TRUE)) {
    x <- tmtd[i]
    command.sep <- strsplit(x,"=~")[[1]]
    eval_value <- grep("paste",command.sep)
    if(length(eval_value!=0)){
     command.sep[2] <- recode_paste(command.sep[2])
    }
    blocks[i,] <- command.sep
  }
  for (i in grep("==", tmtd, fixed = TRUE)) {
    x <- tmtd[i]
    command.sep <- strsplit(x,"==")[[1]]
    blocks[i,"from"] <- command.sep[1]
    blocks[i,"items"] <- blocks[
                                grep(command.sep[2],blocks[,"from"]),
                                "items"]
  }
  return(blocks)
}
#########################################################################

#########################################################################
hfun.simulation <- function(blocks, tmtd, n.stages){

  simulation <- list()
  simulation[["start"]] <- matrix(0,ncol = 3, nrow = sum(grepl("==",tmtd, fixed = TRUE)))
  colnames(simulation$start) <- c("from","to","items")

  # create output list
  for (i in 1:n.stages) {
    simulation[[as.character(i)]] <- NA
  }
  input <- matrix(0,nrow = 1, ncol = 5)
  colnames(input) <- c("from","to","minSolved","maxSolved","items")

  for (i in seq_along(tmtd)) {
    x <- tmtd[i]
    # remove blanks and quotes
    tmt.command <- gsub("\\s\\\"", "", x)

    # get start block
    if (grepl("==", tmt.command, fixed = TRUE)) {
      start.b <- strsplit(tmt.command,"==")[[1]]
      not.b <- agrep("start",start.b)
      row.n <- grep("0",simulation$start[,"from"])[1]

      simulation$start[row.n,"from"] <- simulation$start[row.n,"to"] <- start.b[not.b]
      rows.blocks <- sapply(start.b[-not.b],grep,blocks[,"from"])
      simulation$start[row.n,"items"] <- gsub("c|\\(|\\)","",blocks[rows.blocks,"items"])


    } else if (grepl(":=", tmt.command, fixed = TRUE)) {
      # get definition of branches
      b0 <- strsplit(tmt.command,":=", fixed = TRUE)[[1]]
      b0.branches <- strsplit(b0[2],"+", fixed = TRUE)[[1]]

      for (ii in 1:(length(b0.branches) - 1)) {
        b1 <- strsplit(b0.branches[ii],"\\(|\\)")[[1]]
        b1.names <- b1[1]
        b1.minmax <- strsplit(b1[2],",",fixed = TRUE)[[1]]
        b1.items <- blocks[grep(b1.names,blocks[,"from"]),"items"]
        b1.items <- gsub("c|\\(|\\)","",b1.items)

        b2.names <- strsplit(b0.branches[ii + 1],"\\(|\\)")[[1]][1]
        b2.items <- blocks[grep(b2.names,blocks[,"from"]),"items"]
        b2.items <- gsub("c|\\(|\\)","",b2.items)

        input[,c("minSolved","maxSolved")] <- b1.minmax
        input[,"from"] <- b1.names
        input[,"to"] <- b2.names
        # input[,"items"] <- paste0(b1.items,";",b2.items)
        if(length(b2.items) == 0){
          stop("The specified design has a bug in the block label. \nThe affected block is: ",
            strsplit(b0.branches[ii + 1],"\\(|\\)")[[1]][1])
        }
        input[,"items"] <- b2.items
        if (any(is.na(simulation[[ii + 1]]))) {
          simulation[[ii + 1]] <- input
        } else {
          simulation[[ii + 1]] <- rbind(simulation[[ii + 1]],input)
        }
        input[1,] <- rep(0,ncol(input)) # clear row
      }
    }
  }
  # store only unique combinations
  simulation <- lapply(simulation,unique)
  return(simulation)
}
#########################################################################


#########################################################################
hfun.design <- function(blocks, tmtd, n.branches){
  design <- matrix(0, ncol = 4, nrow = n.branches)
  colnames(design) <- c("mst","minSolved","maxSolved","items")

  branches <- tmtd[grepl(":=", tmtd, fixed = TRUE)]
  branches <- lapply(branches,function(x){
    strsplit(x, ":=", fixed = TRUE)[[1]][2]
  })
  branches <- unlist(branches)

  for (i in seq_along(branches)) {
    b0.branches <- strsplit(branches[i],"+", fixed = TRUE)[[1]]

    stages <- paste0("^",gsub("(.*?)((?:\\().+?(\\)))","\\1",b0.branches,perl = TRUE),"$")
    minmax <- gsub("(.*?)((?:\\()(.+?)(\\)))","\\3",b0.branches,perl = TRUE)
    
    minmax <- strsplit(minmax,",")
    # if last entry is not numeric, than last specified block in branch has 
    # missing minSolved and maxSolved
    # set to min = 0 and max = max of possible raw score
    check_numeric <- suppressWarnings(as.numeric(minmax[[length(minmax)]]))
    minmax <- do.call(rbind,minmax)

    rows.blocks <- sapply(stages,grep,blocks[,"from"])
    if(any(sapply(rows.blocks,length)==0)) stop("There are undefined Blocks within your specified branches. Please correct the design.")

    items <- paste0(blocks[rows.blocks,"items"],collapse = ";")
    items <- unlist(regmatches(items, gregexpr( "(?<=\\().+?(?=\\))", items, perl = TRUE)))

    if (all(is.na(check_numeric))) {
      minmax[nrow(minmax),] <- c("0",as.character(length(strsplit(items[nrow(minmax)],",")[[1]])))
    }

    design[i,"mst"] <- paste0(stages,collapse = "-")
    design[i,"minSolved"] <- paste0(minmax[,1],collapse = ";")
    design[i,"maxSolved"] <- paste0(minmax[,2],collapse = ";")
    design[i,"items"] <- paste0(items,collapse = ";")

  }
  return(data.frame(design))
}

hfun.items <- function(blocks){
  items <- blocks[,"items"]
  items <- gsub("(.*?)(\\()(.+?)(\\))","\\3",items,perl = TRUE)
  items <- paste0(items,collapse = ",")
  items <- unlist(strsplit(items,","))
  items <- unique(items[order(as.numeric(gsub("\\D","",items)))])
return(items)
}
#########################################################################

#########################################################################
hfun.start <- function(blocks, tmtd){

  start <- matrix(0,ncol = 2, nrow = sum(grepl("==",tmtd, fixed = TRUE)))
  colnames(start) <- c("from","items")

  input <- matrix(0,nrow = 1, ncol = 5)
  colnames(input) <- c("from","to","minSolved","maxSolved","items")
  startblocks <- which(grepl("==",tmtd, fixed = TRUE))

  for (i in seq_along(startblocks)) {
    x <- tmtd[startblocks[i]]
    # remove blanks and quotes
    tmt.command <- gsub("\\s\\\"", "", x)

    # get start block
    
      start.b <- strsplit(tmt.command,"==")[[1]]
      not.b <- agrep("start",start.b)
      row.n <- grep("0",start[,"from"])[1]

      start[i,"from"] <- start.b[not.b]
      rows.blocks <- sapply(start.b[-not.b],grep,blocks[,"from"])
      start[i,"items"] <- gsub("c|\\(|\\)","",blocks[rows.blocks,"items"])
  }
  return(start)
}
#########################################################################


#########################################################################
# data check
data_check <- function(dat){
  if (is.null(nrow(dat))) stop("There are not enough Persons in your data! \n")
    n <- nrow(dat)
  if(is.null(colnames(dat))){
    datnames <- paste0("column: ",1:ncol(dat))
  } else{
    datnames <- colnames(dat)
  }
  
  # very efficient way to check the input: Idea from the 'psychotools' package
  status_i <- as.character(
               cut(colMeans(dat, na.rm = TRUE), 
                  c(-Inf, 1/(2 * n), 1 - 1/(2 * n), Inf), labels = c("0", "0/1", "1")
                )
              )

  status_i[is.na(status_i)] <- "NA"
  status_i <- factor(status_i, levels = c("0/1", "0", "1", "NA"))
  ident <- status_i == "0/1"
  names(status_i) <- datnames

  # information for the user
  if (any(status_i == "0") ){
    warning(paste0("The following items where excluded due to (nearly) full '0' responses: ",
    names( status_i )[ status_i == "0" ],"\n"))
    } 
  if (any(status_i == "1") ){
   warning(paste0("The following items where excluded due to (nearly) full '1' responses: ",
   names( status_i )[ status_i == "1" ],"\n"))
  } 
  if (any(status_i == "NA") ){
    warning(paste0("The following items where excluded due to (nearly) full 'NA': ",
    names( status_i )[ status_i == "NA" ],"\n"))
  } 
  ## just estimate those items in category "0/1"
  dat_orig <- dat
  dat <- dat[,ident, drop = FALSE]
    if(sum(ident)==ncol(dat_orig)) {
      status <- NULL
    } else {
      status <- names(status_i[status_i %in% c("1","0","NA")])
    }
    out <- list(dat = dat, status = status)
  return(out)
}
#########################################################################


#########################################################################
# ellipse for tmt_gmc
draw_ellipse <- function(x = 0, y = 0, a = 1, b = 1, angle = pi/3,
                            n = 300) {
            cc <- exp(seq(0, n) * (0 + (0+2i)) * pi/n)
            R <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)),
                        ncol = 2, byrow = TRUE)
            res <- cbind(x = a * Re(cc), y = b * Im(cc)) %*% R
            data.frame(x = res[, 1] + x, y = res[, 2] + y)
}
#########################################################################

#########################################################################
# function to evaluate strings with paste in mstdesign
recode_paste <- function(string){
  string <- as.name(string)
  string_eval <- eval(parse(text =string))
  string_recode <- gsub("\\s","",string_eval)
  string_out <- paste0("c(",paste0(string_recode,collapse = ","),")")
 return(string_out)
}

#########################################################################
# ascii art
tmt_ascii <- function(){
  cat("\n")
  cat(" _             _   \n")
  cat("| |_ _ __ ___ | |_ \n")
  cat("| __| '_ ` _ \\| __|\n")
  cat("| |_| | | | | | |_ \n")
  cat(" \\__|_| |_| |_|\\__|\n")
  cat("\n")
}                   
