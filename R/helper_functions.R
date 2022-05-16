# helper functions

#########################################################################
hfun.modules <- function(tmtd, n.branches, n.modules){
  # definition of module and items
  modules <- matrix(0, ncol = 2, nrow = n.modules)
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
  modules
}
#########################################################################

#########################################################################
hfun.simulation <- function(modules, tmtd, preconditions){

  # check, if routing is defined in additional variable or within path definition
  routing_class <- routing <- NULL 
  if (any(grep("[^:=+~]=[^~=]",tmtd))) {
    routing <- grep("[^:+=~]=[^~=]",tmtd,value=TRUE)
    routing <- gsub("\\s\\\"", "", routing)
    routing <- gsub("c\\(|\\(|\\)","",routing) 
    routing <- do.call(rbind,sapply(routing,strsplit,"="))
    colnames(routing) <- c("name","criteria")
    rownames(routing) <- NULL
    class(routing) <- "prob_rules"
    if (length(grep("^[[:alnum:]]$",routing[,"name"],value=TRUE))!=0) {
      stop("The names of the routing criteria must be alphanumeric (e.g. 'r1')\n")
    }
    if (all(as.numeric(unlist(strsplit(routing[,"criteria"],",")))<1)) {
      routing_class <- "probabilistic"
    } else {
      routing_class <- "deterministic"
    }
  } else {
    routing <- grep(":=",tmtd, value = TRUE)
    routing <- gsub("\\s\\\"", "", routing)

    routing_name <- gsub("\\s*\\([^\\)]+\\).*", "",routing,perl = TRUE)
    routing_name <- do.call(rbind,sapply(routing_name,strsplit,":="))


    routing <- gsub("[^(]*\\(([^)]+)\\)[^()]*", "\\1,",routing,perl=TRUE)
    routing <- gsub(",$","",routing)
    routing_out <- matrix(NA,ncol=2,nrow = length(routing))
    colnames(routing_out) <- c("name","criteria")
    routing_out[,1] <- routing_name[,1]
    routing_out[,2] <- routing
    routing <- as.numeric(do.call(c,sapply(routing,strsplit,",",fixed=TRUE)))
    if (!any(routing>1)) {
      routing_class <- "probabilistic"
      routing <- routing_out # change here for probabilistic definition within path. (Name = module; criteria = values for routing)
      class(routing) <- "prob_path"
    } else {
      routing_class <- "deterministic"
      routing <- NULL
    }
  }
  # 2020-04-24 moved gsub
  modules[,"items"] <- gsub("c\\(|\\(|\\)","",modules[,"items"])

  simulation <- list()
  # simulation[["start"]] <- matrix(0,ncol = 4, nrow = sum(grepl("==",tmtd, fixed = TRUE)))
  # 2020-03-27 == was removed as not necessary, that the starting modules are defined separately
  branches <- tmtd[grepl(":=", tmtd, fixed = TRUE)]
  branches <- strsplit_storing(branches,":=")
  colnames(branches) <- c("path","operator_0", "path_original")
  # 2020-04-17 added new feature to enable splitting with several splitcriteria
  branches <- strsplit_storing(variable = branches,
                              split = c("~","+=","+","++"),
                              cols = "path_original", 
                              new_names = c("module","operator")
                              )


  # added 2020-05-11 to consider pre-conditions
  if (!is.null(preconditions)) {
    precondition_cols <- colnames(branches)[unique(c(apply(branches,1,function(x) grep(paste0(preconditions[,"name"],collapse="|"),x))))]
  
    # dissolve parentheses for preconditions
    i <- 1
    for (m in precondition_cols){
      branches <- strsplit_storing(branches,"\\(|\\)",m, fixed = FALSE, new_names = paste0(c("precondition_","value_"),i), store_operator = FALSE)
      i <- i + 1
    }
    # dissolve parentheses for modules
    i <- 1
    for (m in grep("module",colnames(branches),value=TRUE)){
      branches <- strsplit_storing(branches,"\\(|\\)",m, fixed = FALSE, new_names = paste0(c("module_","rule_"),i), store_operator = FALSE)
      i <- i + 1
    }      
  } else {
    # dissolve parentheses for modules
    i <- 1
    for (m in grep("module",colnames(branches),value=TRUE)){
      branches <- strsplit_storing(branches,"\\(|\\)",m, fixed = FALSE, new_names = paste0(c("module_","rule_"),i), store_operator = FALSE)
      i <- i + 1
    }
  }

  # set values for the first module(s)
  input <- matrix(0,nrow = length(unique(branches[,"module_1"])), ncol = 4)
  colnames(input) <- c("from","to","items_from","items_to")
  input[,c("from")] <- input[,c("to")] <- unique(branches[,"module_1"])
  input[,c("items_from")] <- input[,c("items_to")] <- modules[match(unique(branches[,"module_1"]),modules[,"from"]),"items"]
  simulation[["start"]] <- input

  # simulation[["start"]] <- "Startmodule"
  # colnames(simulation$start) <- c("from","to","items_from","items_to")

  # create output list
  for (i in 2:length(grep("module",colnames(branches)))) {
    simulation[[as.character(i)]] <- NA
  }

  input <- matrix(0,nrow = 1, ncol = 7)
  colnames(input) <- c("from","to","minSolved","maxSolved","items_from","items_to", "probability")

  for (i in grep(c(":="), tmtd)) {
    x <- tmtd[i]
    # remove blanks and quotes
    tmt.command <- gsub("\\s\\\"", "", x)

    # get start module
    # if (grepl("==", tmt.command, fixed = TRUE)) {
    #   start.b <- strsplit(tmt.command,"==")[[1]]
    #   not.b <- agrep("start",start.b)
    #   row.n <- grep("0",simulation$start[,"from"])[1]

    #   simulation$start[row.n,"from"] <- simulation$start[row.n,"to"] <- start.b[not.b]
    #   rows.modules <- sapply(paste0("^",start.b[-not.b],"$"),grep,modules[,"from"])
    #   simulation$start[row.n,"items_from"] <- gsub("c|\\(|\\)","",modules[rows.modules,"items"]) 
    #   simulation$start[row.n,"items_to"] <- gsub("c|\\(|\\)","",modules[rows.modules,"items"])

    # } else if (grepl(":=", tmt.command, fixed = TRUE)) {
      # get definition of branches
      b0 <- strsplit(tmt.command,":=", fixed = TRUE)[[1]]

      # 01.05.2019 added cumulative design feature
      
      # 2020-06-19 mixed designs of cumulative and non cumulative is missing

      if (grepl("\\+\\=|\\+\\+", b0[2])) {
        b0.branches <- strsplit(b0[2],"\\+\\=|\\+\\+")[[1]]
        sim_class <- "cumulative"
        if (any(grepl("\\+[^=]", b0.branches))) {
          b0.branches <- unlist(strsplit(b0.branches,"+", fixed = TRUE))
          sim_class <- "sequential"  
        }
      } else  if (grepl("\\+[^=]",b0[2])) {
        b0.branches <- strsplit(b0[2],"+", fixed = TRUE)[[1]]  
        sim_class <- "sequential"
      } 

      if (!is.null(preconditions)) {
        b0.branches  <- b0.branches[-grep(paste0(preconditions[,"name"],collapse="|"), b0.branches)]
      }

      for (ii in 2:(length(b0.branches))) {
        b1 <- strsplit(b0.branches[ii-1],"\\(|\\)")[[1]]
        b1.names <- b1[1]
        # b1.names <- paste0("^",b1.names,"$",collapse="")
        if (!is.null(routing) & !is.null(routing_class)) {
          if (routing_class == "deterministic") {
            b1[2] <- routing[routing[,"name"]%in%b1[2],"criteria"]
            input[,"probability"] <- paste0(rep(1,max(as.numeric(strsplit(b1[2],",")[[1]]))+1),collapse = ",") # +1 for '0' category
          } else if (routing_class == "probabilistic") {
             if (is.null(preconditions)) {
              minstart <- 0
              # tohere <- 0
            } else {
              minstart <- as.numeric(as.character(min(preconditions[,"min"])))
              # tohere <- 0
            }
            fromhere <- minstart

            if (any(duplicated(routing[,"name"]))) {
              stop("The names for the routing rules are not unique. Please change the names and start the function again.")
            }
            if (inherits(routing,"prob_path")) {
              input[,"probability"] <- routing[grep(c(":="), tmtd)%in%i,"criteria"] # here the probabilities are definied in path, therefore several different probabilities are available for the same module
              tohere <- sum(gregexpr(",",routing[grep(c(":="), tmtd)%in%i,"criteria"],fixed=TRUE)[[1]]>0) + minstart
            } else if (inherits(routing, "prob_rules")) {
              input[,"probability"] <- routing[routing[,"name"]%in%b1[2],"criteria"]
              tohere <- sum(gregexpr(",",routing[routing[,"name"]%in%b1[2],"criteria"],fixed=TRUE)[[1]]>0) + minstart
            }
             
            b1[2] <- paste0(fromhere,",", tohere)
          } 
        } 

        b1.minmax <- strsplit(b1[2],",",fixed = TRUE)[[1]]
        b1.items <- modules[grep(paste0("^",b1.names,"$",collapse=""),modules[,"from"]),"items"]
        # b1.items <- gsub("c|\\(|\\)","",b1.items) # 2020-04-24 moved gsub to top

        b2.names <- strsplit(b0.branches[ii],"\\(|\\)")[[1]][1]
        b2.items <- modules[grep(paste0("^",b2.names,"$",collapse=""),modules[,"from"]),"items"]
        # b2.items <- gsub("c|\\(|\\)","",b2.items) # 2020-04-24 moved gsub to top

        input[,c("minSolved","maxSolved")] <- b1.minmax
        input[,"from"] <- b1.names
        input[,"to"] <- b2.names
        # input[,"items"] <- paste0(b1.items,";",b2.items)

        if (sim_class == "sequential" | ii == 2) {
          input[,"items_from"] <- b1.items
        } else if (sim_class == "cumulative" & ii > 2){
          # if (!is.null(preconditions)) {
            # input[,"items_from"] <- paste0(paste0(preconditions$name,","), simulation[[ii-1]][,"items_from"][simulation[[ii-1]][,"to"] %in% b1.names][1],",",b1.items)
          # } else {
            input[,"items_from"] <- paste0(simulation[[ii-1]][,"items_from"][simulation[[ii-1]][,"to"] %in% b1.names][1],",",b1.items)
          # }
        }

        if (routing_class=="deterministic") {
            input[,"probability"] <- paste0(rep("1",length(strsplit(input[,"items_from"],",")[[1]])+1),collapse = ",") # +1 for '0' category
        }

        if(length(b2.items) == 0){
          stop("The specified MST design needs some attention. \nThe affected module is: ",
            strsplit(b0.branches[ii],"\\(|\\)")[[1]][1])
        }
        input[,"items_to"] <- b2.items
        
        # only for the first entry
        if (any(is.na(simulation[[ii]]))) {
          simulation[[ii]] <- input
        } else {
          simulation[[ii]] <- rbind(simulation[[ii]],input)
        }

        input[1,] <- rep(0,ncol(input)) # clear row
      }
    # }
  }

  

  # store only unique combinations
  simulation <- lapply(simulation,unique)
  
  # 2020-05-13 add preconditions to items_from
  if (!is.null(preconditions) & (sim_class=="cumulative")) {
    for (i in seq(simulation)) {
       simulation[[i]][,"items_from"] <- paste0(paste0(preconditions[,"name"],collapse=","),",",simulation[[i]][,"items_from"])
    }  
  } 

  # define the start modules
  # start.b <- simulation[['start']][,"from"]
  # start.m <- simulation[['start']][,"items_from"]
  # inputstart <- matrix(0, nrow = length(start.b), ncol = 4)
  # colnames(inputstart) <- c("from","to","items_from","items_to")
  # inputstart[,"to"] <- inputstart[,"from"] <- start.b
  # inputstart[,"items_from"] <- inputstart[,"items_to"] <- start.m
  # simulation[["start"]] <- unique(inputstart)

  class(simulation) <- sim_class
  simulation
}
#########################################################################


#########################################################################
hfun.design <- function(modules, tmtd, n.branches){
  
 # check, if routing is defined separate
  routing_class <- routing <- NULL
  if (any(grep("[^:=+~]=[^~=]",tmtd))) {
    routing <- grep("[^:+=~]=[^~=]",tmtd,value=TRUE)
    routing <- gsub("\\s\\\"", "", routing)
    routing <- gsub("c\\(|\\(|\\)","",routing) 
    routing <- do.call(rbind,sapply(routing,strsplit,"="))
    colnames(routing) <- c("name","criteria")
    if (length(grep("^[[:alnum:]]$",routing[,"name"],value=TRUE))!=0) {
      stop("The names of the routing criteria must be alphanumeric (e.g. 'r1')\n")
    }
    if (all(as.numeric(strsplit(routing[1,"criteria"],",")[[1]]) < 1) & all(as.numeric(strsplit(routing[1,"criteria"],",")[[1]]) > 0)) {
      routing_class <- "probabilistic"
    } else {
      routing_class <- "deterministic"
    }
  } else { # note: change here, if probabilities are allowed within path definition
    routing_class <- "deterministic"
  }

  design <- matrix(0, ncol = 7, nrow = n.branches)
  colnames(design) <- c("mst","minSolved","maxSolved","items","minSolved_stage","maxSolved_stage","probability")


  # if (any(grepl("+=",branches)) {
  #   cumlength <- sapply(branches,strsplit,"+=",fixed = TRUE, simplify=TRUE)  
  #   cumlength <- unlist(lapply(cumlength,length)) - 2 # rule applies not for the first and last module!
    
  #   items <- unlist(regmatches(modules[,"items"], gregexpr( "(?<=\\().+?(?=\\))", modules[,"items"], perl = TRUE)))
  #   possible_scores <- sapply(items,function(x) length(strsplit(x,",")[[1]]) +1 , simplify=TRUE,USE.NAMES=FALSE)
  #   modules_score <- cbind(modules[,"from"],possible_scores)
  # # es muss nun herausgefunden werden, wie viele Items in dem vorherigen Modul sind, dies ist der rep Faktor für die Module


  #   design <- matrix(0, ncol = 7, nrow = n.branches)
  #   colnames(design) <- c("mst","minSolved","maxSolved","items","minSolved_stage","maxSolved_stage","probability")
    
  # }
    

  branches <- tmtd[grepl(":=", tmtd, fixed = TRUE)]
  branches <- lapply(branches,function(x){
    strsplit(x, ":=", fixed = TRUE)[[1]][2]
  })
  branches <- unlist(branches)
  
  for (i in seq_along(branches)) {

    # 2019-05-01 added cumulative design feature
    if ( grepl("\\+\\=|\\+\\+", branches[i]) ) {
      b0.branches <- strsplit(branches[i],c("\\+\\=|\\+\\+"))[[1]]
      design_class <- "cumulative"
    } else {
      b0.branches <- strsplit(branches[i],"+", fixed = TRUE)[[1]]
      design_class <- "sequential"
    }
   
    probabilities <- rep(1,length(b0.branches))
    if (!is.null(routing)) {   
      if (routing_class == "deterministic") {
        for(ii in seq_along(routing[,"name"])){
          b0.branches <- gsub(routing[ii,"name"],routing[ii,"criteria"],b0.branches)
        }
      } else if (routing_class=="probabilistic") {
        if (design_class == "cumulative"){
          routes <- gsub("(.*?)((?:\\()(.+?)(\\)))","\\3",b0.branches,perl = TRUE)
          for (ii in seq_along(routes)) {
            if (routes[ii]%in%routing[,"name"]) {
              probabilities[ii] <- routing[routing[,"name"]%in%routes[ii],"criteria"]
            }
          }
        } else if (design_class == "sequential" & (length(grep("==",tmtd,value=TRUE))>0)) {
          # condition with sequential routing and preconditions
          n.preconditions <- sapply(grep("==",tmtd,value=TRUE),function(x) strsplit(x,"==")[[1]])[1,]
          b0.branches_adapt <- b0.branches[-grep(n.preconditions,b0.branches)]
          routes <- gsub("(.*?)((?:\\()(.+?)(\\)))","\\3",b0.branches_adapt,perl = TRUE)
          for (ii in seq_along(routes)) {
            if (routes[ii]%in%routing[,"name"]) {
              probabilities[ii] <- routing[routing[,"name"]%in%routes[ii],"criteria"]
            }
          }
        } else if (design_class == "sequential" & (length(grep("[^:+]=[^~]",tmtd,value=TRUE))>0)) {
          # n.preconditions <- sapply(grep("[^:+]=[^~]",tmtd,value=TRUE),function(x) strsplit(x,"=")[[1]])[1,]
          routes <- gsub("(.*?)((?:\\()(.+?)(\\)))","\\3",b0.branches,perl = TRUE)
          for (ii in seq_along(routes)) {
            if (routes[ii]%in%routing[,"name"]) {
              probabilities[ii] <- routing[routing[,"name"]%in%routes[ii],"criteria"]
            }
          }
        }
        
      } 
    }

    
    v.preconditions <- grep("==",tmtd,value=TRUE) # grep only '~'
    if(length(v.preconditions)>0){
      n.preconditions <- sapply(v.preconditions,function(x) strsplit(x,"==")[[1]])[1,]
      b0.branches <- b0.branches[-grep(n.preconditions,b0.branches)]
    }

    stages <- paste0("^",gsub("(.*?)((?:\\().+?(\\)))","\\1",b0.branches,perl = TRUE),"$")
    minmax <- gsub("(.*?)((?:\\()(.+?)(\\)))","\\3",b0.branches,perl = TRUE)
    #  gregexpr(",",routing[routing[,"name"]%in%minmax[ii],"criteria"],fixed=TRUE)[[1]]>0
    if (routing_class=="probabilistic") {
      for (ii in seq_along(minmax)) {
        if (minmax[ii]%in%routing[,"name"]) {
          minmax[ii] <- paste0("0,",sum(strsplit(routing[routing[,"name"]%in%minmax[ii],"criteria"],",")[[1]]>0) - 1)    
        }
      }
    }
     

    minmax <- strsplit(minmax,",")
    # if last entry is not numeric, than last specified module in branch has 
    # missing minSolved and maxSolved
    # set to min = 0 and max = max of possible raw score
    check_numeric <- suppressWarnings(as.numeric(minmax[[length(minmax)]]))
    minmax <- do.call(rbind,minmax)

    rows.modules <- sapply(stages,grep,modules[,"from"])
    
    if(any(sapply(rows.modules,length)==0)) stop("There are undefined modules within your specified branches. Please correct the design.")

    items_vec <- items <- modules[rows.modules,"items"]
    
    # 2020-06-20: fixed issue in prob.designs with unproper item-vector preparation
    items_match <- gregexpr( "(?<=\\().+?(?=\\))", items, perl = TRUE)
    items <- unlist(regmatches(items, items_match))
    items <- paste0(items,collapse = ";")


    # check if minSolved and maxSolved in last module is specified, otherwise it is specified here:
    if (all(is.na(check_numeric))) {
      if (design_class == "sequential") {

        minmax[nrow(minmax),] <- c("0",as.character(length(strsplit(items_vec[nrow(minmax)],",")[[1]])))
        if (routing_class=="probabilistic") {
          probabilities[nrow(minmax)] <- paste0(rep("1",length = length(strsplit(items_vec[nrow(minmax)],",")[[1]])+1),collapse=",")
        }
      }
      if (design_class == "cumulative") {
       minmax[nrow(minmax),] <- c(minmax[nrow(minmax)-1,1], as.character(as.numeric(minmax[nrow(minmax)-1,2]) + length(strsplit(items_vec[nrow(minmax)],",")[[1]]))) 
      # 2020-05-21 fixed issue: for cumulative-probabilistic designs the weights for the last module has to be extended 
      if (routing_class=="probabilistic") probabilities[nrow(minmax)] <- paste0(rep("1",length = length(unlist(strsplit(items,","))) + length(stages)),collapse=",")
      }
    }
    # minmax_stages <- minmax_cum <- minmax
    minmax_stages <- minmax
    if (design_class == "cumulative") {

      minmax_tmp <- minmax <- apply(minmax,2,as.numeric)
      
      # 2020-06-20 fixed item preparation for cumulative designs
      items_n <- strsplit(items, ";")[[1]]
      # items_n <- lengths(regmatches(items_n, gregexpr(",", items_n))) + 1 # for first Item
      items_n <- lengths(sapply(items_n,strsplit,","))
      
      for(iii in 2:nrow(minmax_tmp)){
        # correct minSolved
        check_minSolved <- minmax[iii,1] - minmax[iii-1,2]
        check_maxSolved <- minmax[iii,2] - minmax[iii-1,1]
        if (check_minSolved <= 0) {
          minmax_tmp[iii,1] <- 0
        } else if (check_minSolved < items_n[iii]) {
          minmax_tmp[iii,1] <- check_minSolved
        }
        # correct maxSolved
        if (check_maxSolved <= items_n[iii]) {
          minmax_tmp[iii,2] <- check_maxSolved
        } else if (check_maxSolved > items_n[iii]) {
          minmax_tmp[iii,2] <- items_n[iii]
        }
      }
      minmax <- apply(minmax_tmp,2,as.character)
    } else {
      # cumsum for non cumulativ designs
      minmax_stages[,1] <- cumsum(as.numeric(minmax[,1]))
      minmax_stages[,2] <- cumsum(as.numeric(minmax[,2]))
    }

    if (routing_class=="deterministic") {
      # +1 for '0' category
      # probabilities_tmp <- lapply(as.numeric(minmax[,2])+1,function(x) rep(1,x))
      items_split <- strsplit(items,";")[[1]]
      
      if(design_class == "cumulative"){
        
        items_list <- strsplit(items_split,",")
        prob_sequences <- lapply(seq(items_list), function(x) sequence(nvec = x)) 
        

        probs1 <- lapply(strsplit(items_split,","),function(x) rep(1, length(x)))
        probs2 <- lapply(prob_sequences,function(x) unlist(probs1[x]))
        probabilities_tmp <- lapply(probs2,function(x) c(1,x)) #add 1 for zero score

      } else {
        probabilities_tmp <- lapply(strsplit(items_split,","),function(x) rep(1, length(x) + 1))
      } 
      probabilities <- unlist(lapply(probabilities_tmp,paste0,collapse=",")) 
      # for(p in seq_along(probabilities_tmp)) {
      #     probabilities[p] <- paste0(probabilities_tmp[[p]],collapse=",")
      # }
    } 

    design[i,"mst"] <- paste0(stages,collapse = "-")
    design[i,"items"] <- paste0(items,collapse = ";")
    design[i,"minSolved"] <- paste0(minmax[,1],collapse = ";")
    design[i,"maxSolved"] <- paste0(minmax[,2],collapse = ";")
    design[i,"minSolved_stage"] <- paste0(minmax_stages[,1],collapse = ";")
    design[i,"maxSolved_stage"] <- paste0(minmax_stages[,2],collapse = ";")
    design[i,"probability"] <- paste0(probabilities,collapse = ";")
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
# hfun.start <- function(modules, tmtd){

#   start <- matrix(0,ncol = 2, nrow matrix(0, ncol = 2, nrow = n.modules + n.start)= sum(grepl("==",tmtd, fixed = TRUE)))
#   colnames(start) <- c("from","items")

#   input <- matrix(0,nrow = 1, ncol = 5)
#   colnames(input) <- c("from","to","minSolved","maxSolved","items")
#   startmodules <- which(grepl("==",tmtd, fixed = TRUE))

#   for (i in seq_along(startmodules)) {
#     x <- tmtd[startmodules[i]]
#     # remove blanks and quotes
#     tmt.command <- gsub("\\s\\\"", "", x)

#     # get start module
    
#       start.b <- strsplit(tmt.command,"==")[[1]]
#       not.b <- agrep("start",start.b)
#       row.n <- grep("0",start[,"from"])[1]

#       start[i,"from"] <- start.b[not.b]
#       rows.modules <- sapply(paste0("^",start.b[-not.b],"$"),grep,modules[,"from"])
#       start[i,"items"] <- gsub("c\\(|\\(|\\)","",modules[rows.modules,"items"])
#   }
#   start
# }
#########################################################################

#########################################################################
hfun.preconditions <- function(tmtd, preconditions, modules){

  # is there a separate definition of the rules
  routing_def <- grep("[^:+=~]=[^~=]",tmtd, value=TRUE)
  routing_check <- TRUE

  if (length(routing_def) > 0) {
    # check, if the length is lower than the module size. If not, than deterministic routing is applied
    routing_def <- gsub("\\s\\\"", "", routing_def)
    routing_def <- gsub("c\\(|\\(|\\)","",routing_def) 
    routing_def <- sapply(routing_def,strsplit,"=")
    names(routing_def) <- NULL
    routing_def <- do.call(rbind,routing_def)
    colnames(routing_def) <- c("name","rule")

    if (length(grep("^[[:alnum:]]$",routing_def[,"name"],value=TRUE))!=0) {
      stop("The names of the routing rule must be alphanumeric (e.g. 'r1')\n")
    }

    # get length of modules
    modules_n <- lengths(gregexpr(",",modules[,"items"])) + 1
    routing_rule_n <- lengths(gregexpr(",",routing_def[,"rule"]))
    min_size <- min(length(modules_n),length(routing_rule_n))
    routing_check <- ifelse(all(routing_rule_n[seq(min_size)] < modules_n[seq(min_size)]),FALSE,TRUE) 
  
  } else {
    modules_n <- lengths(gregexpr(",",modules[,"items"])) + 1
  }

  modules_n <- data.frame(modules,modules_n)
  colnames(modules_n) <- c("module","items","length")

  if((length(routing_def) > 0) & routing_check){
    routing_class <- "probabilistic"
  } else {
    routing_class <- "deterministic"
  }

  # define outputlist
  tmtd_out <- list("original" = tmtd, 
                    "tmtd" = NULL,
                    "modules" = NULL,
                    "rules" = NULL,
                    "paths" = NULL,
                    "preconditions" = NULL,
                    "precondition_matrix" = NULL)


 # determine branches and disassemble
  branches <- tmtd[grepl(":=", tmtd, fixed = TRUE)]
  branches <- strsplit_storing(branches,":=")
  colnames(branches) <- c("path","operator_0", "path_original")

  # 2020-04- 17 added new feature to enable splitting with several splitcriteria
  branches <- strsplit_storing(variable = branches,
                               split = c("~","+=","+","++"),
                               cols = "path_original", 
                               new_names = c("module","operator")
                               )

  precondition_cols <- colnames(branches)[unique(c(apply(branches,1,function(x) grep(paste0(preconditions$name,collapse="|"),x))))]
 
  # dissolve parentheses for preconditions
  i <- 1
  for (m in precondition_cols){
    branches <- strsplit_storing(branches,"\\(|\\)",m, fixed = FALSE, new_names = paste0(c("precondition_","value_"),i), store_operator = FALSE)
    i <- i + 1
  }
  # dissolve parentheses for modules
  i <- 1
  for (m in grep("module",colnames(branches),value=TRUE)){
    branches <- strsplit_storing(branches,"\\(|\\)",m, fixed = FALSE, new_names = paste0(c("module_","rule_"),i), store_operator = FALSE)
    i <- i + 1
  }




  # if (length(routing_def) > 0) {
  #   if(routing_class=="probabilistic"){
  #        if ( all(is.na(match(branches[,v],routing_def[,"name"]))) ){
  #          rules_precon <- do.call(rbind,sapply(branches[,v],strsplit,":"))
  #          rules_precon <- matrix(as.numeric(rules_precon),    # Convert to numeric matrix
  #                 ncol = ncol(rules_precon))
  #          maxscores <- max(rules_precon)
  #          rules_precon <- data.frame(rules_precon,"probs" = NA)

  #           for(i in seq(nrow(rules_precon))) {
  #               tmp <- rep(0,maxscores)
  #               tmp[rules_precon[i,1]:rules_precon[i,2]] <- 1
  #               rules_precon$probs[i] <- paste0(tmp,collapse=",")
  #           }
          

  #        }
  #     }
  #   for(v in grep("value",colnames(branches))){
  #      if(routing_class=="probabilistic"){ 

  #      }else {
  #       branches[,v] <- routing_def[,"rule"][match(branches[,v],routing_def[,"name"])]  
  #     }
  #   }    
  # }

  if (length(routing_def) > 0) {
    for(v in grep("value",colnames(branches))){
      # check, of the rules are globaly defined or directly in the paths
        rule_defined_glob <- !is.na(match(branches[,v],routing_def[,"name"]))
        branches[rule_defined_glob,v] <- routing_def[,"rule"][match(branches[rule_defined_glob,v],routing_def[,"name"])]  
    }    
  }
  # the design extend to preconditions that are dissolved here
  branches_expand <- expand.matrix(variable = branches,
                  names = grep("value",colnames(branches),value = TRUE))

  # finde the unique rows
  # sum only those preconditions, which are kumulative
  rules_n <- grep("rule",colnames(branches_expand))

  precondition_val <- gsub("value_","",grep("value",colnames(branches_expand), value = TRUE))
  precondition_operator <- unique(branches_expand[,paste0("operator_",precondition_val), drop = FALSE])
  precondition_values <- paste0("value_",precondition_val)[sapply(precondition_operator,function(x) any(c("+=","++")%in%x)) ]
  precondition_sums <- rowSums(branches_expand[,precondition_values, drop = FALSE])

  # matrix with name and min max of precondition
  precondition_matrix <- matrix(NA,nrow = length(precondition_values), ncol = 3)
  colnames(precondition_matrix) <- c("name","min","max")
  precondition_matrix[,"name"] <- unlist(unique(branches_expand[,grep("precondition",colnames(branches_expand))]))
  precondition_matrix[,"min"] <- unlist(apply(branches_expand[,precondition_values, drop = FALSE],2,min))
  precondition_matrix[,"max"] <- unlist(apply(branches_expand[,precondition_values, drop = FALSE],2,max))
  # determine the max amount of sums within each path at each stage
  path_sums <- matrix(NA,nrow = nrow(branches_expand), ncol = length(rules_n))
  colnames(path_sums) <- paste0("stage_",seq_len(ncol(path_sums)))
  for (i in seq(ncol(path_sums))) {
    # wenn kumulativ, dann kumulieren, sonst jeweils stufe berichten
    path_sums[,i] <- modules_n[,"length"][match(branches_expand[,paste0("module_",i)],modules_n[,"module"])]
    operator_i <- branches_expand[,paste0("operator_",i-1)] %in% c("+=","++")
    if ( (i > 1) & any(operator_i)) {
      path_sums[operator_i,i] <- path_sums[operator_i,i] + path_sums[operator_i,i-1]
    }
  }
  
  routing_new <- NULL
  
  # ------------------------------------------------------------------------------
  # Now check the elements and evaluate at  += the conditions must be adapted in the remaining categories !!
  if (any(unlist(precondition_operator) %in% c("+=","++"))) {
    if (length(routing_def) > 0) { # condition if the probabilities have been defined in their own section
      irow <- 0
      branches_expand_routing <- data.frame(matrix(NA,nrow=nrow(branches_expand)*length(rules_n), ncol = 4))
      colnames(branches_expand_routing) <- c("precondition_sum","modules","rule_old","rule_new")
      branches_expand_routing[,"precondition_sum"] <- rep(precondition_sums,length(rules_n))

      for (i in seq_along(rules_n) ) {
        out_mat <- cbind(precondition_sums,branches_expand[,paste0(c("module_","rule_"),i)])
        # patterns <- apply(out_mat, 1, paste0, collapse = "\r")
        patterns <- Reduce(function(...) paste(...,sep = "\r"), out_mat)
        new_names <- rep(NA,nrow(out_mat))
          for (ii in unique(out_mat[,paste0("rule_",i)])) {
            out_rows <- out_mat[,paste0("rule_",i)] == ii
            letters_new <- add_letters(sum(out_rows))
            iv <- 1
            for (iii in unique(patterns[out_rows])) {
              new_names[patterns%in%iii] <- paste0(ii,letters_new[iv])
              iv <- iv + 1
            }
        }
        branches_expand_routing[seq(nrow(branches_expand))+irow,c("precondition_sum","modules","rule_old")] <- out_mat
        branches_expand_routing[seq(nrow(branches_expand))+irow,c("rule_new")] <- new_names
        irow <- irow + nrow(branches_expand)
        branches_expand[,paste0("rule_",i)] <- new_names
      }

      routing_new <- routing_def[match(branches_expand_routing[!duplicated(branches_expand_routing[,"rule_new"],fromLast=TRUE),"rule_old"],routing_def[,"name"]),]
      routing_new <- data.frame("new_name" = branches_expand_routing[!duplicated(branches_expand_routing[,"rule_new"],fromLast=TRUE),"rule_new"], 
                            "precondition_sum" = branches_expand_routing[!duplicated(branches_expand_routing[,"rule_new"],fromLast=TRUE),"precondition_sum"], 
                            routing_new, stringsAsFactors = FALSE)
      routing_new$precondition_sum <- as.numeric(routing_new$precondition_sum)
    }
    
    # probabilistic
    if (routing_class == "probabilistic") { 
        max_module_path <- data.frame("module" = unlist(branches_expand[,paste0("module_",seq(rules_n))]),
                  "rule" = unlist(branches_expand[,paste0("rule_",seq(rules_n))]),
                  "max_items" = c(path_sums),row.names=seq(nrow(branches_expand)*length(rules_n)),
                  "operator" = unlist(branches_expand[,paste0("operator_",seq(rules_n))]))

        max_module_path <- unique(max_module_path)
        min_precondition <- routing_new[,"precondition_sum"]
        # rescale to start at 1 for index purposes 
        # get changes of names
        change_name <- rle(routing_new[,"name"])$length
        
        min_change <- tapply(routing_new[,"precondition_sum"],rep(seq_len(length(change_name)),change_name),min)
        min_change <- rep(min_change,change_name)
        
        min_precondition <- min_precondition - min_change + 1

        for(i in seq(nrow(routing_new))) {
          new_rule <- strsplit(routing_new[i,"rule"],",")[[1]]
          max_items_module <- max_module_path[max_module_path[,"rule"]%in%routing_new[i,"new_name"],"max_items"]
          operator <- max_module_path[max_module_path[,"rule"]%in%routing_new[i,"new_name"],"operator"]
          # routing_new[i,"rule"] <- paste0(new_rule[1:(length(new_rule) - routing_new[i,"precondition_sum"])],collapse = ",")
          if (operator %in% c("+=","++")) {
              routing_new[i,"rule"] <- paste0(new_rule[seq(min_precondition[i],max_items_module + min_precondition[i])],collapse = ",")
          } 
        }
    }


    if (routing_class == "deterministic") {
      if (length(routing_def) > 0) {
        max_module_path <- data.frame("module" = unlist(branches_expand[,paste0("module_",seq_along(rules_n))]),
                  "rule" = unlist(branches_expand[,paste0("rule_",seq_along(rules_n))]),
                  "max_items" = c(path_sums),row.names=seq(nrow(branches_expand)*length(rules_n)))
        max_module_path <- unique(max_module_path)
        flaged <- NULL # for cases if precondition max is greater than maxSolved in path

        for(i in seq(nrow(routing_new))){
           # forming the sums of the modules
            max_module <- max_module_path[max_module_path[,"rule"]%in%routing_new[i,"new_name"],"max_items"]
            precondition_val <- routing_new[i,"precondition_sum"] 

            out_rows <- as.numeric(strsplit(routing_new[i,"rule"],",")[[1]])

            out_rows[1] <- max(0, out_rows[1] - precondition_val) #minSolved
            out_rows[2] <- max(0,out_rows[2] - precondition_val) #maxSolved
            if(out_rows[1] == out_rows[2]) flaged <- c(flaged,i)
            out_rows[2] <- ifelse(out_rows[2] > max_module, max_module, out_rows[2])
            routing_new[i,"rule"] <- paste(out_rows,collapse=",")
        }
        if(!is.null(flaged)){
          branches_expand <- branches_expand[-flaged,]
          rownames(branches_expand) <- NULL
          # routing_new <- routing_new[-flaged,] # 2021-12-12 muss nicht für alle Fälle zutreffen
          rownames(routing_new) <- NULL
        }

      } else {
        for (i in seq_along(rules_n) ) { # vectorised version
          max_module <- path_sums[,paste0("stage_",i)]
          # forming the sums of the modules
          precondition_val <- rowSums(branches_expand[, grep("value",colnames(branches_expand)),drop=FALSE])

          out_rows <- strsplit(branches_expand[,paste0("rule_",i)],",")
          out_rows <- do.call(rbind,out_rows)
          out_rows <- apply(out_rows,2,as.numeric)

          # adapt only cumulative cases
          if ( any(branches_expand[,paste0("operator_",i)] %in% c("+=","++")) ) {
            cases <- branches_expand[,paste0("operator_",i)] %in% c("+=","++")
            out_rows[cases,1] <-  apply(cbind(0,out_rows[cases,1] - precondition_val[cases]),1,max)#minSolved
            out_rows[cases,2] <- out_rows[cases,2] - precondition_val[cases] #maxSolved         
            out_rows[cases,2] <- ifelse(out_rows[cases,2] > max_module[cases], max_module[cases], out_rows[cases,2])
            # Reduce(function(...) paste(...,sep = "\r"), out_mat)
            branches_expand[cases,paste0("rule_",i)] <- apply(out_rows[cases,],1,paste,collapse=",")

            if (any(!cases) ) {
              out_rows[!cases,1] <- out_rows[!cases,1] #minSolved
              out_rows[!cases,2] <- out_rows[!cases,2] #maxSolved         
              branches_expand[!cases,paste0("rule_",i)] <- apply(out_rows[!cases,],1,paste,collapse=",")
            }

          } # else {
          #   out_rows[!cases,1] <- out_rows[!cases,1] #minSolved
          #   out_rows[!cases,2] <- out_rows[!cases,2] #maxSolved         
          #   branches_expand[!cases,paste0("rule_",i)] <- apply(out_rows[!cases,],1,paste,collapse=",")
          # }
          
        }
      }  
    }
  } 

    branches_expand[,"path"] <- paste0(branches_expand[,"path"],unlist(c(sapply(table(branches_expand[,"path"]),add_letters))))
    for(i in grep("rule",colnames(branches_expand))){
      branches_expand[,i] <- paste0("(",branches_expand[,i],")")
    }

    precondition_p <- grep("precondition",colnames(branches_expand))
    precondition_p <- unlist(c(sapply(precondition_p,function(x) seq(from = x,x+2))))
    branches_new <- branches_expand[,-precondition_p]
    precondition_new <- branches_expand[,precondition_p]
    
    if (!is.null(routing_new)) {
    tmtd_routing <- c()
      for (i in seq(nrow(routing_new))) {
        tmtd_routing <- c(tmtd_routing, paste0(routing_new[i,"new_name"],paste0("=c(",routing_new[i,"rule"],")")))
      }
    } else {
      tmtd_routing <- ""
    }
    tmtd_modules <- apply(modules,1,paste0,collapse="=~")
    
    tmtd_path <- do.call(paste0,branches_new)

    tmtd_new <- c(tmtd_modules,
                      tmtd_routing,
                      tmtd_path)
  # ######################################
  tmtd_out$original <- tmtd
  tmtd_out$tmtd <- tmtd_new
  # tmtd_out$tmtd <- apply(branches_new,1,function(x) paste0(x,collapse=""))
  tmtd_out$modules <- modules
  
  mst_name <- apply(branches_expand[,grep("module",colnames(branches_expand))],1,function(x) paste0("^",x,"$",collapse="-"))
  preconditions_out <- cbind("path" = mst_name,"Start" = branches_expand[,"module_1"],precondition_new[,grep("precondition|value",colnames(precondition_new))])
  tmtd_out$preconditions <- preconditions_out
  tmtd_out$rules <- routing_new
  tmtd_out$paths <- branches_expand
  tmtd_out$precondition_matrix <- precondition_matrix
  # ######################################

  return(tmtd_out)
}
#########################################################################

#########################################################################
# data check
data_check <- function(dat, items){
  precon <- NULL
  if (!missing(items)) {
    if (ncol(dat) != length(items)) {
      precon <- dat[,-match(items,colnames(dat)), drop = FALSE]
    }
    dat <- dat[,items]
  }
  if (is.null(nrow(dat))) stop("There are not enough Persons in your data! \n")
    n <- nrow(dat)
  if(is.null(colnames(dat))){
    datnames <- paste0("column: ",seq_len(ncol(dat)))
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
    warning(paste0("The following items were excluded due to (nearly) full '0' responses: ",
    names( status_i )[ status_i == "0" ],"\n"))
    } 
  if (any(status_i == "1") ){
   warning(paste0("The following items were excluded due to (nearly) full '1' responses: ",
   names( status_i )[ status_i == "1" ],"\n"))
  } 
  if (any(status_i == "NA") ){
    warning(paste0("The following items were excluded due to (nearly) full 'NA': ",
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
    if (!is.null(precon)) {
      out <- list(dat = cbind(precon,dat), status = status)
    } else {
      out <- list(dat = dat, status = status)
    }
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
  string_eval <- eval(parse(text = string))
  string_recode <- gsub("\\s","",string_eval)
  string_out <- paste0("c(",paste0(string_recode,collapse = ","),")")
 string_out
}

#########################################################################
# ascii art
tmt_ascii <- function(){
  cat(" _             _   \n")
  cat("| |_ _ __ ___ | |_ \n")
  cat("| __| '_ ` _ \\| __|\n")
  cat("| |_| | | | | | |_ \n")
  cat(" \\__|_| |_| |_|\\__|\n")
  cat("\n")
}                   

#################################
# function to convert thresholds to probabilities (used in tmt)
#################################
thresholds_to_probs <- function(thres, opts = "mstdesign", row = TRUE){
  if (opts=="mstdesign") {
    cat("--------------------------------------------\n")
    cat("computed colwise differences for 'mstdesign'\n")
    cat("--------------------------------------------\n")
  }
  # check the order of submitted thres
  if (row) {
    uniqueorder <- unique(apply(thres,2,which.max))
  } else {
    uniqueorder <- unique(apply(thres,1,which.max))
  }

  if(!length(uniqueorder) == 1) {
    stop("\nAttention is needed! \nThe order of the submitted thresholds is not the same for all raw scores!\n")
  }

  if (uniqueorder == 1) {
    if (row){
      thres <- matrix(apply(thres,2,rev), nrow = 1) 
    } else {
      thres <- t(matrix(apply(thres,1,rev), ncol = 1) )
    }
  } 

  if (row) {
    if (nrow(thres)==1) {
      thres_diff <- cbind(thres[1,,drop=FALSE],t(diff(thres)))
    } else {
      thres_diff <- cbind(thres[1,],t(diff(thres)))  
    }
    
  } else {
    thres_diff <- t(rbind(thres[,1],diff(t(thres))))
  }
  
  if(!is.null(colnames(thres))){
    colnames(thres_diff) <- colnames(thres)
  } else {
    colnames(thres_diff) <- paste0(seq_len(ncol(thres_diff)))
  }
  
  if("mstdesign" %in% opts){
    out <- list()
    for(i in seq(ncol(thres_diff)) ){
      out[[i]] <- paste0("\np",i," = c(",paste0(thres_diff[,i] , collapse = ", "),")",collapse="")
    }
    cat(unlist(out),"\n\n")

  } else {
    thres_diff
  }
}

########################
add_letters <- function(n) {
    if(n > 26){
      a <- ceiling(log(n, 26))
      m <- sapply(a:1, function(x) {
        rep(rep(seq_len(26), each = 26^(x-1)) , length.out = n)
      })
      a_new <- letters[m]
      dim(a_new) <- dim(m)

      apply(a_new, 1, function(x) paste(x, collapse = ""))
    } else {
      letters[seq_len(n)]
    }
}


########################################
# function to split variables after split criteria with option to keep cols and add new names
# ------------------------------------------------------------------------------

strsplit_storing <- function(variable, split, cols = NULL, new_names = NULL, fixed = TRUE, store_operator = TRUE) {
  if (!fixed) {
    split_inner <- gsub("\\[|\\]","",split)
  } else {
    split_inner <- split
  }
  if (length(split)>1) {
    split_inner <- paste0("[",paste0(split,collapse = "|"),"]+")
    fixed <- FALSE
  }

  if (is.character(cols)) {
    cols <- match(cols,colnames(variable))
  } 

  # splitting one column from matrix:
  if (!is.null(ncol(variable)) & !is.null(cols) & length(cols) == 1) {
    variable_split <- variable[,cols]
    

    variable_operator <- regmatches(variable_split, gregexpr(split_inner, variable_split))
    
    variable_split <- strsplit(variable_split, split_inner, fixed = fixed)

    if (store_operator) {
      for (i in seq(length(variable_split)) ) {        
        split_c <- c(rbind( variable_split[[i]], c(variable_operator[[i]],"") ))
        variable_split[[i]] <- split_c[-length(split_c)]
      }
    }
    variable_split <- do.call(rbind,variable_split)
    if (!is.null(new_names) & ncol(variable_split) > 1) {
      if ( length(new_names) != ncol(variable_split)) {
        colnames(variable_split) <- paste0( rep(paste0(new_names,"_"),length.out = ncol(variable_split)),
                                            rep(seq_len(ceiling(ncol(variable_split)/length(new_names))),each=length(new_names),length.out=ncol(variable_split))
                                          ) 
      } else if (length(new_names) == ncol(variable_split) ) {
        colnames(variable_split) <- new_names
      }
    } else {
      colnames(variable_split) <- colnames(variable)[cols]
    }
    nmax <- ncol(variable)
    out <- cbind(variable[, 0:(cols-1)], variable_split, {if(cols<nmax) variable[,(cols+1):nmax] else NULL})
  } else if (is.null(ncol(variable))) {
    variable_split <- strsplit(variable, split, fixed = fixed)
    if (store_operator) {
      for (i in seq(length(variable_split)) ) {        
        split_c <- c(rbind( variable_split[[i]], rep(split_inner, length(variable_split[[i]])) ))
        variable_split[[i]] <- split_c[-length(split_c)]
      }
    }
      out <- do.call(rbind,variable_split)
  } else {
    stop("Please submit either a vector to split or a matrix/data.frame with specified cols (length = 1)\n")
  }
  return(out)
}

################################################
# function to expand matrix used in mstdesign (routing criteria within design)
# ------------------------------------------------------------------------------
expand.matrix <- function(variable, names, eval = TRUE) {
  # beide Inputmöglichkeiten anbieten. Über min/max (3,5) oder Vector von Scores (3:5)
  eval_operator <- grep(",",variable[,names])
  if(length(eval_operator)>0){
    for (i in names) {
      variable[,i] <- gsub(",",":",variable[,i])
    }
  }
  if (eval) {
  out <- list()
  for(r in seq(nrow(variable))) { 
      eval_names <- lapply(variable[r,names],function(x) eval(parse(text = x)))
      eval_names_expand <- expand.grid(eval_names)
      out[[r]] <- data.frame(variable[rep.int(r, prod(lengths(eval_names))),], stringsAsFactors = FALSE)
      out[[r]][,names] <- eval_names_expand
    }
  out <- do.call(rbind,out)
  }
  return(out)
}
# ------------------------------------------------------------------------------
# 2020-05-09 functions for precon sim
convert <- function(input){
  if (is.list(input)) {
    if (is.null(names(input))) {
      if (all((lengths(input)%%3)==0)) {
        # input = list(c(500,0,1),c(500,0,1))
        out <- do.call(rbind,input)
      } else {
        # input = list(500,500)
        out <- cbind(unlist(input),0,1)
      }
    } else if (all(names(input) %in% c("n","mean","sd")) | all(names(input) %in% c("min","max","r"))) {
      # input = list("n" = c(500,500), "mean" = c(0,0), "sd" = c(1,1))
      out <- do.call(cbind,input)
    } else if (all(lengths(input)==3)){
      out <- do.call(rbind,input)
    } else {
      stop("The submitted list of ppar or precon is not valid.\nPlease provide for:\n- 'ppar' either a list with 'n' for each group or vectors of 'n', 'mean', 'sd', each as named list entries.\n- 'precon' a list with 'min','max' and 'r', 'n' is used from 'ppar'")
    }
  } else if (is.matrix(input) | is.data.frame(input)) {
    if (ncol(input)!=3) stop("The submitted matrix for ppar/precon is not valid.\nPleas provide for:\n- 'ppar' information about 'n', 'mean' and 'sd' in separated columns\n- 'precon' information about 'min', 'max' and 'r'")
    # matrix(rbind(c("n" = 500, "mean" = 0, "sd" = 1), c("n" = 500, "mean" = 0, "sd" = 1)), ncol = 3)
    out <- input
  } else if (is.null(names(input))|all(input > 10)) {
      if (all(input > 10)) { # case if only size is submitted
        # input = 500; input = c(500,500)
        out <- cbind(c(input),0,1)
      } else if ((length(input) %% 3) == 0) {
        # input = c(500,0,1)
        out <- matrix(input,ncol=3)
      } else {
        # input = c(500,1)
        stop("The submitted Information in ppar/precon: 'c(",paste0(input, collapse = ","),")' is not valid. Please submit for:\n- 'ppar' either 'n' or a vector with 'n', 'mean' and 'sd'\n- 'precon' information about 'min', 'max' and 'r'")
      }
  } else {
    out <- rbind(input)
    rownames(out) <- NULL
  }
  return(out)  
}

precon_gen <- function(perspar, precon_conv, seed) {
  out <- list()
  
  precon_bin <- function(y, categories) {
    # if(max(categories) > length(y)) stop("The maximum of categories is defined by the length of ppar.")
    categories <- sort(categories)
    y_pnorm <- stats::pnorm(y, mean = 0, sd = 1)
    catego <- seq(0, 1, length.out = length(seq(categories[1],categories[2]))+1)
    bins <- cut(y_pnorm, breaks = catego, include.lowest = TRUE, labels = seq(categories[1],categories[2]))

    as.integer(as.character(bins))
  }

  # function to generated precon based on perspar and r
  find_y <- function(x, r, categories) {
    # first the value for the correlation needs to be adjusted (see Demirtas & Yavuz, 2015 <DOI:10.1080/10543406.2014.920868>)
    #1. first generate 100000 samples of standard normal distribution 
    s1 <- stats::rnorm(1e+05, 0, 1)
    s2 <- stats::rnorm(1e+05, 0, 1)
    #2. ordinalize on the the variable
    s2_ordinal <- precon_bin(s2,categories)
    #3 order vairables
    s1 <- s1[order(s1)]
    s2 <- s2[order(s2)]
    s2_ordinal <- s2_ordinal[order(s2_ordinal)]

    #4. compute adjusted correlation
    r_adj <- stats::cor(s2_ordinal, s1) / stats::cor(s2, s1)
    r <- r/r_adj
    
    # generate y based on x and given r
    y <- stats::rnorm(length(x), mean = 0, sd = 1)
    x_perp <- stats::residuals(stats::lm(y ~ x))
    r * stats::sd(x_perp) * x + x_perp * stats::sd(x) * sqrt(1 - r^2)
  }



  # if (is.matrix(precon_conv)) {
    
    # if (nrow(precon_conv) != length(perspar)) {
    #   warning("The same precon definition is repeated to match the number of specified groups\n")
    #   precon_conv <- precon_conv[rep(1:nrow(precon_conv),length.out = length(perspar)),]
    # }

    if (length(perspar) < nrow(precon_conv)) {
      if (length(perspar) == 1) {
        perspar <- rep(perspar, nrow(precon_conv))
      } else {
        stop("Please correct the specified number of persons. It is necessary to specify either a total amount or a separate amount for each precondition.")
      }      
    }
    
     if (!is.null(seed)) {
        set.seed(seed)
        for (i in seq_len(nrow(precon_conv))) {
          if (precon_conv[i,"r"] != 1) {
            precon_r <- find_y(x = perspar[[i]], r = precon_conv[i,"r"], categories = c(precon_conv[i,c("min","max")]))
            out[[i]] <- precon_bin(y = precon_r, categories = c(precon_conv[i,c("min","max")]))
          } else {
            out[[i]] <- precon_bin(y = perspar[[i]], categories = c(precon_conv[i,c("min","max")]))
          }
        }
      } else {
        for (i in seq_len(nrow(precon_conv))) {
          if (precon_conv[i,"r"] != 1) {
            precon_r <- find_y(x = perspar[[i]], r = precon_conv[i,"r"], categories = c(precon_conv[i,c("min","max")]))
            out[[i]] <- precon_bin(y = precon_r, categories = c(precon_conv[i,c("min","max")]))
          } else {
            out[[i]] <- precon_bin(y = perspar[[i]], categories = c(precon_conv[i,c("min","max")]))
          }
        }
      }
      
  # } else {
  #     out <- precon_conv 
  # }
  return(out)
}
# ------------------------------------------------------------------------------
# function to generate person parameter and if desired also (correlated) sum scores for pre-conditions 

precon_sim <- function(ppar, precon = NULL,...){
  
  additional_arguments <- list(...)
  seed <- NULL
  if(!is.null(additional_arguments$seed)) seed <- additional_arguments$seed

  out <- list()
  perspar <- NULL
  preconpar <- NULL
  
  if (!is.null(names(ppar))) names(ppar) <- tolower(names(ppar))
  if (!is.null(names(precon))) names(precon) <- tolower(names(precon))

  # if ((length(precon) > length(ppar)) & !is.list(ppar)) {
    # warning("The same number of persons is repeated for each specified precondition\n")
    # ppar <- rep(ppar,length(precon))
  # }

  if (is.list(ppar)) {
      if (all(unlist(lapply(ppar,function(x) x%%1!=0)))) {
      ppar_conv <- ppar
      } else {
      ppar_conv <- convert(ppar)
      colnames(ppar_conv) <- c("n","mean","sd")
      }
  } else if (all(ppar%%1!=0)) {
      ppar_conv <- list(ppar)
  } else {
      ppar_conv <- convert(ppar)
      colnames(ppar_conv) <- c("n","mean","sd")
  }

  if (is.matrix(ppar_conv)) {
     if(!is.null(seed)){
        set.seed(seed)
        perspar <- lapply(seq_len(nrow(ppar_conv)), function(x) stats::rnorm(ppar_conv[x,1],ppar_conv[x,2],ppar_conv[,3]))
      } else {
        perspar <- lapply(seq_len(nrow(ppar_conv)), function(x) stats::rnorm(ppar_conv[x,1],ppar_conv[x,2],ppar_conv[,3]))
      }
  } else {
      perspar <- ppar_conv
  }

  if (!is.null(precon)) {
    if (is.list(precon)) {
        if (length(perspar) == length(precon)) {
        if (all(lengths(perspar) == lengths(precon))) {
            precon_conv <- precon
        } else {
            precon_conv <- convert(precon)
        }
        } else {
        precon_conv <- convert(precon)
    }
    } else if(is.matrix(precon)) {
        precon_conv <- convert(precon)
    } else if (length(precon)==3) {
        precon_conv <- convert(precon)
    } 
    # else {
    #     precon_conv <- list(precon)
    # }

    if (is.matrix(precon_conv)) {
        # sanitary checks
        if (is.null(colnames(precon_conv))) {
          colnames(precon_conv) <- c("min","max","r")
        }
        if (any(precon_conv[,"r"] > 1) | any(precon_conv[,"r"]<0)) {
          stop("Please check the provided 'r' in precon!\nOnly values between 0 and 1 are allowed.")
        }
        if (any(precon_conv[,c("min","max")]%%1!=0)) {
          stop("Please check the provided 'min' and 'max' in precon.\nOnly integers are allowed")
        }

        if (nrow(precon_conv) < length(perspar)) {
          warning("The same precon definition is repeated to match the number of specified groups\n")
          precon_conv <- precon_conv[rep(seq_len(nrow(precon_conv)),length.out = length(perspar)),]
        }
        
    } else if (all(lengths(perspar) == lengths(precon))) {
      precon_check <- unlist(lapply(precon_conv,function(x) any(x%%1!=0)))
      if (any(precon_check)) stop("Please check the provided precon.\nOnly integers are allowed")
    } 
    # else {
    #   precon_check <- unlist(lapply(precon_conv,function(x) any(x[,c("min","max")]%%1!=0)))
    #   if (any(precon_check)) stop("Please check the provided precon.\nOnly integers are allowed")
    # }


    
    if (is.matrix(precon_conv)) {
      preconpar <- precon_gen(perspar, precon_conv, seed)
    } else if (all(lengths(perspar) == lengths(precon))) {
      preconpar <- precon
    }
    # if(any(lengths(preconpar) != lengths(perspar))) stop("the number of person parameters and preconditions do not match!")

    # preconpar <- unlist(preconpar)
  }

  # perspar <- unlist(perspar)

  out <- list("perspar" = perspar, "preconpar" = preconpar)
  return(out)
}
# ------------------------------------------------------------------------------
# function to separate list into chunks
# chunks(seq(10),3)
chunks <- function(x, n, len = length(x), groups = trunc(len/n), overflow = len%%n) { 
    f1 <- as.character(sort(rep(seq_len(n), groups)))
    f <- as.character(c(f1, rep(n, overflow)))
  
    g <- split(x, f)

    g.names <- names(g)
    g.names.ordered <- as.character(sort(as.numeric(g.names)))
  
  return(g[g.names.ordered])
}
# ------------------------------------------------------------------------------

hfun.preconditionoperator <- function(tmtd, preconditions){
   # determine branches and disassemble
  branches <- tmtd[grepl(":=", tmtd, fixed = TRUE)]
  branches <- strsplit_storing(branches,":=")
  colnames(branches) <- c("path","operator_0", "path_original")

  # 2020-04- 17 added new feature to enable splitting with several splitcriteria
  branches <- strsplit_storing(variable = branches,
                               split = c("~","+=","+","++"),
                               cols = "path_original", 
                               fixed = TRUE,
                               new_names = c("module","operator"), 
                               store_operator = TRUE
                               )
    precondition_cols <- colnames(branches)[unique(c(apply(branches,1,function(x) grep(paste0(preconditions$name,collapse="|"),x))))]

  i <- 1
  for (m in precondition_cols){
    branches <- strsplit_storing(branches,"\\(|\\)",m, fixed = FALSE, new_names = paste0(c("precondition_","value_"),i), store_operator = FALSE)
    i <- i + 1
  }
  
  checkdata <- sapply((gregexpr("_", colnames(branches), fixed=TRUE)), function(i) sum(i > 1))

  if (any(checkdata>1)) {
      stop("The routing type for the preconditions is unknown, please change it to either '+', '++' or '+=' and start again.")
    } 
  
  precondition_val <- gsub("value_","",grep("value",colnames(branches), value = TRUE))
  precondition_operator <- unique(branches[,paste0("operator_",precondition_val),drop = FALSE])
  precondition_values <- paste0("value_",precondition_val)
  
  branches[,precondition_values] <- apply(branches[,precondition_values, drop = FALSE],2,function(x) gsub(",",":",x))
  

  preconvalues <- unlist(sapply(branches[,precondition_values, drop = FALSE], function(x) eval(parse(text = x))))

  precondition_matrix <- matrix(NA,nrow = length(precondition_cols), ncol = 3)
  colnames(precondition_matrix) <- c("name","min","max")

  precondition_matrix[,"name"] <- unlist(unique(branches[,grep("precondition",colnames(branches))]))
  precondition_matrix[,"min"] <- min(preconvalues)
  precondition_matrix[,"max"] <- max(preconvalues)

  out <- list()
  out$routing <- ifelse(precondition_operator %in% c("+=","++"),"cumulative","sequential")
  out$precondition_matrix <- precondition_matrix
  return(
    out
  )
}
