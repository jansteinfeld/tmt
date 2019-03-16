#' Function to Translate the \code{mstdesign} Syntax
#' 
#' This function translates the multistage design for different purposes.
#' 
#' @param mstdesign definition of desired multistage design
#' @param options vector of required output. 'blocks' = Matrix with the classification of Block and Items. ' simulation' = list of all stages. 'design' = matrix of all branches. 'items' vector of all Items.
#'
#' @author Jan Steinfeld
#' 
#' @return List with following entries
#' 
#'  \item{blocks}{Matrix which contains each block with its corresponding items}
#'  \item{simulation}{List of the multistage design. Each element within the list contains a matrix for each stage}
#'  \item{design}{Matrix of all possible branches}
#'  \item{items}{Vector of item names}
#'  \item{start}{Items of the starting block(s)}
#' 
#' 
#' @example ./R/.example_tmt_mstdesign.R
#' 
#' @export
tmt_mstdesign <- function(mstdesign, options = c("design", "simulation", "blocks", "items")){

  if(!is.vector(mstdesign)){
    stop("Only character vectors are allowed as parameter for 'mstdesign'\n")
  }
  allowed <- c("design", "simulation", "blocks", "items", "start")
  if (!all(options %in% allowed)) {
    stop("Only this options are supported: ", paste0("\"",allowed,"\""), "\n please change the input.")
  }

# ...................................................................................................
  # some possible misspecification
  checinput <- strsplit(mstdesign,"\n")[[1]]
  checkinput <- gsub("\\s","",checinput)
  misspecified <- c("\\b=:\\b|\\b~=~\\b|\\b~~=\\b|\\b~=\\b|\\b~~\\b|\\b~\\b|\\b<~\\b|\\b=\\b")
  miss_list <- list()
    for(m in 1:length(misspecified)){
      miss_list[[m]] <- grep(misspecified[m], checkinput, value = TRUE, perl = TRUE)
    }

  if(length(miss_list[[1]]) > 1){
    stop("The submitted mstdesign is misspecified, please correct this expression/s:\n",
      paste0(unlist(miss_list),sep=" ")
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
  # number of blocks & branches
  n.blocks <- length(grep("=~",tmtd, fixed = TRUE))
  n.start <- length(grep("==",tmtd, fixed = TRUE))
  n.branches <- length(grep(":=",tmtd, fixed = TRUE))
  l.stages <- nchar(as.character(tmtd[grepl(":=",tmtd, fixed = TRUE)])) -
                nchar( gsub("\\+", "", tmtd[grepl(":=",tmtd, perl = TRUE)]))
  n.stages <- max(l.stages)

  # ---------------------------
  blocks <- simulation <- design <- items <- start <- NULL
    blocks <- hfun.blocks(tmtd = tmtd, 
                               n.branches = n.branches, 
                               n.blocks = n.blocks, 
                               n.start = n.start)
  if ("simulation" %in% options) simulation <- hfun.simulation(blocks = blocks, 
                                                                tmtd = tmtd, 
                                                                n.stages = n.stages)
  if ("design" %in% options) design <- hfun.design(blocks = blocks, 
                                                  tmtd = tmtd,  
                                                  n.branches = n.branches)
  if ("items" %in% options) items <- hfun.items(blocks = blocks)
  if ("start" %in% options) start <- hfun.start(blocks = blocks, 
                                                tmtd = tmtd)
  if (!"blocks" %in% options) blocks <- NULL

  return(
    list("blocks" = blocks,
         "simulation" = simulation,
         "design" = design,
         "items" = items,
         "start" = start
         )
    )
}
