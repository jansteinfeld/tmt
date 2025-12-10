#' Function to Translate the \code{mstdesign} Syntax
#' 
#' This function translates the specified multistage design for different purposes and functions used in this package. It is possible to apply this function on deterministic as well as probabilistic multistage designs with either sequential or cumulative routing. A detailed instruciton of the application can be found in the package vignette.
#' 
#' @param mstdesign definition of desired multistage design
#' @param options vector of required output. 'modules' = Matrix with the classification of modules and items. ' simulation' = list of all stages. 'design' = matrix of all branches. 'items' vector of all Items.
#'
#' @author Jan Steinfeld
#' 
#' @return List with following entries
#' 
#'  \item{modules}{Matrix which contains each module with its corresponding items}
#'  \item{simulation}{List of the multistage design. Each element within the list contains a matrix for each stage}
#'  \item{design}{Matrix of all possible branches}
#'  \item{items}{Vector of item names}
#' 
#' 
#' @example ./R/.example_tmt_mstdesign.R
#' 
#' @export
tmt_mstdesign <- function(mstdesign, options = c("design", "simulation", "modules", "items")){

  if(!is.vector(mstdesign)){
    stop("Only character vectors are allowed as parameter for 'mstdesign'\n")
  }
  allowed <- c("design", "simulation", "modules", "items", "start")
  if (!all(options %in% allowed)) {
    stop("Only this options are supported: ", paste("\"",allowed,"\""), "\n please change the input.")
  }

  # ...................................................................................................
  # some possible misspecification
  checinput <- strsplit(mstdesign,"\n")[[1]]
  checkinput <- gsub("\\s","",checinput)
  # misspecified <- c("\\=[^:]|\\=[^~~]|[^~~=]|~=|~~|~|<~|\\*")
  # 2020-03-27 updated list
  misspecified <- "=:|=~~|~~=|~=|~~|<~|\\*|"
  # miss_list <- list()

  # checkinput[gregexpr("=:",checkinput, perl = TRUE)>0]
  # checkinput[gregexpr("=~~",checkinput, perl = TRUE)>0]
  # checkinput[gregexpr("~~=",checkinput, perl = TRUE)>0]
  # checkinput[gregexpr("~=",checkinput, perl = TRUE)>0]
  # checkinput[gregexpr("~~",checkinput, perl = TRUE)>0]
  # checkinput[gregexpr("~",checkinput, perl = TRUE)>0]
  # checkinput[gregexpr("<~",checkinput, perl = TRUE)>0]
  # checkinput[gregexpr("\\*",checkinput, perl = TRUE)>0]

  # ein paar Beispiele ausprobieren
  # checkinput[length(checkinput)+1] <- "das=:istErsterTest"
  #   checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  #   checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]
  # checkinput[length(checkinput)+1] <- "das=~~istZweiterTest"
  #   checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  #   checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]
  # checkinput[length(checkinput)+1] <- "das~~=istDritterTest"
  #   checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  #   checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]
  # checkinput[length(checkinput)+1] <- "das~=istVierterTest"
  #   checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  #   checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]
  # checkinput[length(checkinput)+1] <- "das~~istFuenfterTest"
  #   checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  #   checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]
  # checkinput[length(checkinput)+1] <- "das~istSechsterTest"
  #   checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  #   checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]
  # checkinput[length(checkinput)+1] <- "das<~istSiebterTest"
  #   checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  #   checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]
  # checkinput[length(checkinput)+1] <- "das*istAchterTest"
  #   checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  #   checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]

  checlocation <- gregexpr(misspecified,checkinput, perl = TRUE)
  result_misspecified <- checkinput[unlist(lapply(checlocation,function(x) any(attr(x,"match.length")>0)))]

  # gregexpr("\\+[^~]",checkinput)
  # "~|\\+[^=]|\\+\\="

  # 2020-03-27 fixed loop
    # for(m in 1:length(misspecified)){
    #   miss_list[[m]] <- grep(misspecified[m], checkinput, value = TRUE, perl = TRUE)
    # }

  if (length(result_misspecified)>0) {
    stop("The submitted mstdesign is misspecified, please correct the following expression/s:\n",
      paste0(" - ",result_misspecified,collapse="\n")    
    )
  }

# ...................................................................................................

  # clean mstdesign input:
  tmt.syntax <- mstdesign
  tmt.syntax <- gsub("[#!].*(?=\n)","", tmt.syntax, perl = TRUE)
  tmt.syntax <- gsub(";", "\n", tmt.syntax, fixed = TRUE)
  tmt.syntax <- gsub("[ \t]+", "", tmt.syntax, perl = TRUE)
  tmt.syntax <- gsub("\n{2,}", "\n", tmt.syntax, perl = TRUE)
  tmtd <- unlist( strsplit(tmt.syntax, "\n") )
  tmtd <- tmtd[tmtd!=""]
  undefined <- which(!grepl("[~=:]", tmtd))

  if (length(undefined) > 0) {
    stop("The given multistage design is not correct specified: ",
      tmtd[undefined],
      "Pleas correct the syntax and start again. \n")
  }

  # create list for the simulation function
  # -----------------------------------
  # number of modules & branches

  # für tmtd muss schon überprüft werden, ob es preconditions gibt...dann muss die Simulationsfunktion angepasst werden und die Info in das Design übernommen werden
  # die Paths am besten reduzieren um die Einträge, dann gehen die übrigen Funktionen noch...Evtl. die Info in einer anderen Funktion aufbereiten und dann je nach Bedarf den output hinzufügen. Letztlich muss man allerdings darauf achten, dass die Anzahl der Zeilen dann bspw. im design-output um die preconditions ergänzt wird!!

  # preconditions können mit ~ enden, aber auch mit +=...wie differenzieren? Müssen eingangs oben definiert werden?



  n.modules <- length(grep("=~",tmtd, fixed = TRUE))
  # n.start <- length(grep("==",tmtd, fixed = TRUE))
  n.branches <- length(grep(":=",tmtd, fixed = TRUE))
 
  # l.stages <- nchar(as.character(tmtd[grepl(":=",tmtd, fixed = TRUE)])) -
  #               nchar( gsub("\\+", "", tmtd[grepl(":=",tmtd, perl = TRUE)]))
  # n.stages <- max(l.stages)
  n.preconditions <- length(grep("==",tmtd)) # grep only '~'
  if (n.preconditions != 0) {
    c.preconditions <- grep("==",tmtd, value = TRUE)
    c.preconditions <- strsplit(c.preconditions,"==")
    v.preconditions <- sapply(c.preconditions,"[[",1)
    a.preconditions <- sapply(c.preconditions,"[[",2)
    av.preconditions <- list("name" = v.preconditions, "value" = a.preconditions)
  } else {
    av.preconditions <- NULL
  }
  
  # ---------------------------
  preconditions_sim <- preconditions <- start <- simulation <- design <- items <- NULL


  modules <- hfun.modules(tmtd = tmtd, 
                            n.branches = n.branches, 
                            n.modules = n.modules)
  
  tmtd_sim <- tmtd
  if (n.preconditions != 0) {  
    # sanity check: Operator "==" is now used for preconditions and not for the allocation of starting modules

    if (any(av.preconditions$value%in%modules[,"from"])) stop("The operator '==' is now used for the definition of preconditions!\n * Please integrate the start module into the mst design and \n * start the function again.")

    # nur bei der kumulativen verzweigung muss das Design entsprechend angepasst werden
    routing_type <- hfun.preconditionoperator(tmtd = tmtd, 
                                preconditions = av.preconditions)

    if (all(routing_type$routing %in% "sequential")) {
      preconditions_sim <- routing_type$precondition_matrix
    } else if ("cumulative" %in% routing_type$routing) {
      preconditions <- hfun.preconditions(tmtd = tmtd, 
                                preconditions = av.preconditions, 
                                modules = modules)
      tmtd <- preconditions$tmtd
      n.branches <- nrow(preconditions$paths)
      preconditions_sim <- preconditions$precondition_matrix
      # n.stages <- length(grep("module",colnames(preconditions$paths)))
    } 

  }






  # 2020-05-21 added for probabilistic-cumulative designs
#   branches <- tmtd[grepl(":=", tmtd, fixed = TRUE)]
#   if (any(grepl("+=",branches)) {


#   branches <- strsplit_storing(branches,":=")
#   colnames(branches) <- c("path","operator_0", "path_original")
#   branches <- strsplit_storing(variable = branches,
#                                split = c("~","+=","+"),
#                                cols = "path_original", 
#                                new_names = c("module","operator"))

#   i <- 1
#   for (m in grep("module",colnames(branches),value=TRUE)){
#     branches <- strsplit_storing(branches,"\\(|\\)",m, fixed = FALSE, new_names = paste0(c("module_","rule_"),i), store_operator = FALSE)
#     i <- i + 1
#   }
#   # nun die rules durch die lenght der items ersetzen und dann jeweils um eines reduzieren
#   items <- unlist(regmatches(modules[,"items"], gregexpr( "(?<=\\().+?(?=\\))", modules[,"items"], perl = TRUE)))
#   possible_scores <- sapply(items,function(x) paste0("0:",length(strsplit(x,",")[[1]])) , simplify=TRUE,USE.NAMES=FALSE)
#   modules_score <- cbind(modules[,"from"],possible_scores)
#   colnames(modules_score) <- c("module","score")

  
#   for (o in seq(grep("rule",colnames(branches)))) {
#     pos_module <- match(c(branches[,paste0("module_",o)]), modules_score[,"module"])
#     branches[,paste0("rule_",o)] <- modules_score[,"score"][pos_module]
#   }

#   # nur für die mittleren Fälle
#   cases <- grep("rule",colnames(branches), value = TRUE)
#   cases <- cases[-length(cases)]
  
#   branches_expand <- expand.matrix(variable = branches,
#                 names = cases)
#   branches_expand <- branches_expand[,seq(grep(cases,colnames(branches_expand))+2)]
  
  
#   operator_check <- rowSums(apply(branches_expand[,grep(cases, colnames(branches_expand))+1,drop=FALSE],2,function(x) x%in%"+="))
#   branches_expand <- branches_expand[operator_check==length(cases),]
  
# }
  

  if ("simulation" %in% options) simulation <- hfun.simulation(modules = modules,
                                                              tmtd = tmtd_sim,
                                                              preconditions = preconditions_sim)
  
  if ("design" %in% options) design <- hfun.design(modules = modules, 
                                                  tmtd = tmtd,  
                                                  n.branches = n.branches)
  
  if ("items" %in% options) items <- hfun.items(modules = modules)
  
  if ("start" %in% options){
    if ("simulation" %in% options) {
      start <- simulation$start
    } else {
      start <- hfun.simulation(modules = modules, 
                                tmtd = tmtd_sim, 
                                preconditions = preconditions_sim)$start
    }
  } 

  if (!"modules" %in% options) modules <- NULL
  
  out <- list("modules" = modules,
              "simulation" = simulation,
              "design" = design,
              "items" = items,
              "preconditions" = preconditions,
              "start" = start
            )

  return(out)
    
}
