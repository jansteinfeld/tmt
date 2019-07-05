# helper functions

#########################################################################
hfun.modules <- function(tmtd, n.branches, n.modules, n.start){
  # definition of module and items
  modules <- matrix(0, ncol = 2, nrow = n.modules + n.start)
  colnames(modules) <- c("from","items")
  # -----------------------------------
  # helper matrix for the definition of module and its dedicated items
  for (i in grep("=~", tmtd, fixed = TRUE)) {
    x <- tmtd[i]
    command.sep <- strsplit(x,"=~")[[1]]
    eval_value <- grep("paste",command.sep)
    if(length(eval_value!=0)){
     command.sep[2] <- recode_paste(command.sep[2])
    }
    modules[i,] <- command.sep
  }
  for (i in grep("==", tmtd, fixed = TRUE)) {
    x <- tmtd[i]
    command.sep <- strsplit(x,"==")[[1]]
    modules[i,"from"] <- command.sep[1]
    modules[i,"items"] <- modules[grep(
                                  paste0("^",command.sep[2],"$"),modules[,"from"]),
                                "items"]
  }
  modules
}
#########################################################################

#########################################################################
hfun.simulation <- function(modules, tmtd, n.stages){

  simulation <- list()
  simulation[["start"]] <- matrix(0,ncol = 4, nrow = sum(grepl("==",tmtd, fixed = TRUE)))
  colnames(simulation$start) <- c("from","to","items_from","items_to")

  # create output list
  for (i in 1:n.stages) {
    simulation[[as.character(i)]] <- NA
  }
  input <- matrix(0,nrow = 1, ncol = 6)
  colnames(input) <- c("from","to","minSolved","maxSolved","items_from","items_to")

  for (i in grep(c("==|:="),tmtd)) {
    x <- tmtd[i]
    # remove blanks and quotes
    tmt.command <- gsub("\\s\\\"", "", x)

    # get start module
    if (grepl("==", tmt.command, fixed = TRUE)) {
      start.b <- strsplit(tmt.command,"==")[[1]]
      not.b <- agrep("start",start.b)
      row.n <- grep("0",simulation$start[,"from"])[1]

      simulation$start[row.n,"from"] <- simulation$start[row.n,"to"] <- start.b[not.b]
      rows.modules <- sapply(paste0("^",start.b[-not.b],"$"),grep,modules[,"from"])
      simulation$start[row.n,"items_from"] <- gsub("c|\\(|\\)","",modules[rows.modules,"items"]) 
      simulation$start[row.n,"items_to"] <- gsub("c|\\(|\\)","",modules[rows.modules,"items"])

    } else if (grepl(":=", tmt.command, fixed = TRUE)) {
      # get definition of branches
      b0 <- strsplit(tmt.command,":=", fixed = TRUE)[[1]]

      # 01.05.2019 added cumulative design feature
      if(grepl("+=",b0[2])) {
        b0.branches <- strsplit(b0[2],"+=", fixed = TRUE)[[1]]  
        sim_class <- "cumulative"
      } else {
        b0.branches <- strsplit(b0[2],"+", fixed = TRUE)[[1]]  
        sim_class <- "sequential"
      }
      

      for (ii in 2:(length(b0.branches))) {
        b1 <- strsplit(b0.branches[ii-1],"\\(|\\)")[[1]]
        b1.names <- b1[1]
        b1.minmax <- strsplit(b1[2],",",fixed = TRUE)[[1]]
        b1.items <- modules[grep(b1.names,modules[,"from"]),"items"]
        b1.items <- gsub("c|\\(|\\)","",b1.items)

        b2.names <- strsplit(b0.branches[ii],"\\(|\\)")[[1]][1]
        b2.items <- modules[grep(b2.names,modules[,"from"]),"items"]
        b2.items <- gsub("c|\\(|\\)","",b2.items)

        input[,c("minSolved","maxSolved")] <- b1.minmax
        input[,"from"] <- b1.names
        input[,"to"] <- b2.names
        # input[,"items"] <- paste0(b1.items,";",b2.items)
        if (sim_class == "sequential" | ii == 2) {
          input[,"items_from"] <- b1.items
        } else if (sim_class == "cumulative" & ii > 2){
          input[,"items_from"] <- paste0(simulation[[ii-1]][,"items_from"][simulation[[ii-1]][,"to"] %in% b1.names][1],",",b1.items)
        }
        if(length(b2.items) == 0){
          stop("The specified design has a bug in the module label. \nThe affected module is: ",
            strsplit(b0.branches[ii],"\\(|\\)")[[1]][1])
        }
        input[,"items_to"] <- b2.items
        if (any(is.na(simulation[[ii]]))) {
          simulation[[ii]] <- input
        } else {
          simulation[[ii]] <- rbind(simulation[[ii]],input)
        }
        input[1,] <- rep(0,ncol(input)) # clear row
      }
    }
  }
  # store only unique combinations
  simulation <- lapply(simulation,unique)
  class(simulation) <- sim_class
  simulation
}
#########################################################################


#########################################################################
hfun.design <- function(modules, tmtd, n.branches){
  design <- matrix(0, ncol = 6, nrow = n.branches)
  colnames(design) <- c("mst","minSolved","maxSolved","items","minSolved_stage","maxSolved_stage")

  branches <- tmtd[grepl(":=", tmtd, fixed = TRUE)]
  branches <- lapply(branches,function(x){
    strsplit(x, ":=", fixed = TRUE)[[1]][2]
  })
  branches <- unlist(branches)

  for (i in seq_along(branches)) {

     # 01.05.2019 added cumulative design feature
    if(grepl("+=",branches[i])) {
      b0.branches <- strsplit(branches[i],"+=", fixed = TRUE)[[1]]
      design_class <- "cumulative"
    } else {
      b0.branches <- strsplit(branches[i],"+", fixed = TRUE)[[1]]
      design_class <- "sequential"
    }
   

    stages <- paste0("^",gsub("(.*?)((?:\\().+?(\\)))","\\1",b0.branches,perl = TRUE),"$")
    minmax <- gsub("(.*?)((?:\\()(.+?)(\\)))","\\3",b0.branches,perl = TRUE)
    
    minmax <- strsplit(minmax,",")
    # if last entry is not numeric, than last specified module in branch has 
    # missing minSolved and maxSolved
    # set to min = 0 and max = max of possible raw score
    check_numeric <- suppressWarnings(as.numeric(minmax[[length(minmax)]]))
    minmax <- do.call(rbind,minmax)

    rows.modules <- sapply(stages,grep,modules[,"from"])
    if(any(sapply(rows.modules,length)==0)) stop("There are undefined modules within your specified branches. Please correct the design.")

    items <- paste0(modules[rows.modules,"items"],collapse = ";")
    items <- unlist(regmatches(items, gregexpr( "(?<=\\().+?(?=\\))", items, perl = TRUE)))

    # check if minSolved and maxSolved in last module is specified, otherwise it is specified here:
    if (all(is.na(check_numeric))) {
      if (design_class == "sequential") {
        minmax[nrow(minmax),] <- c("0",as.character(length(strsplit(items[nrow(minmax)],",")[[1]])))
      }
      if (design_class == "cumulative") {
       minmax[nrow(minmax),] <- c(minmax[nrow(minmax)-1,1], as.character(as.numeric(minmax[nrow(minmax)-1,2]) + length(strsplit(items[nrow(minmax)],",")[[1]]))) 
      }
    }
    minmax_stages <- minmax_cum <- minmax
    if (design_class == "cumulative") {
      minmax_tmp <- minmax <- apply(minmax,2,as.numeric)
      items_n <- lengths(regmatches(items, gregexpr(",", items))) + 1 # for first Item

      for(ii in 2:nrow(minmax_tmp)){
        # correct minSolved
        check_minSolved <- minmax[ii,1] - minmax[ii-1,2]
        check_maxSolved <- minmax[ii,2] - minmax[ii-1,1]
        if (check_minSolved <= 0) {
          minmax_tmp[ii,1] <- 0
        } else if (check_minSolved < items_n[ii]) {
          minmax_tmp[ii,1] <- check_minSolved
        }
        # correct maxSolved
        if (check_maxSolved <= items_n[ii]) {
          minmax_tmp[ii,2] <- check_maxSolved
        } else if (check_maxSolved > items_n[ii]) {
          minmax_tmp[ii,2] <- items_n[ii]
        }
      }
      minmax <- apply(minmax_tmp,2,as.character)
    } else {
      # cumsum for non cumulativ designs
      minmax_stages[,1] <- cumsum(as.numeric(minmax[,1]))
      minmax_stages[,2] <- cumsum(as.numeric(minmax[,2]))
    }

    design[i,"mst"] <- paste0(stages,collapse = "-")
    design[i,"items"] <- paste0(items,collapse = ";")
    design[i,"minSolved"] <- paste0(minmax[,1],collapse = ";")
    design[i,"maxSolved"] <- paste0(minmax[,2],collapse = ";")
    design[i,"minSolved_stage"] <- paste0(minmax_stages[,1],collapse = ";")
    design[i,"maxSolved_stage"] <- paste0(minmax_stages[,2],collapse = ";")
  }

  design <- data.frame(design)
  class(design) <- append(class(design),design_class)
  design
}
#########################################################################


#########################################################################
hfun.items <- function(modules){
  items <- modules[,"items"]
  items <- gsub("(.*?)(\\()(.+?)(\\))","\\3",items, perl = TRUE)
  items <- paste0(items,collapse = ",")
  items <- unlist(strsplit(items,","))
  items <- unique(items[order(as.numeric(gsub("\\D","",items)))])
items
}
#########################################################################

#########################################################################
hfun.start <- function(modules, tmtd){

  start <- matrix(0,ncol = 2, nrow = sum(grepl("==",tmtd, fixed = TRUE)))
  colnames(start) <- c("from","items")

  input <- matrix(0,nrow = 1, ncol = 5)
  colnames(input) <- c("from","to","minSolved","maxSolved","items")
  startmodules <- which(grepl("==",tmtd, fixed = TRUE))

  for (i in seq_along(startmodules)) {
    x <- tmtd[startmodules[i]]
    # remove blanks and quotes
    tmt.command <- gsub("\\s\\\"", "", x)

    # get start module
    
      start.b <- strsplit(tmt.command,"==")[[1]]
      not.b <- agrep("start",start.b)
      row.n <- grep("0",start[,"from"])[1]

      start[i,"from"] <- start.b[not.b]
      rows.modules <- sapply(paste0("^",start.b[-not.b],"$"),grep,modules[,"from"])
      start[i,"items"] <- gsub("c|\\(|\\)","",modules[rows.modules,"items"])
  }
  start
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
  out
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
 string_out
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
