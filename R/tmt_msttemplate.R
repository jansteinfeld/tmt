#' Function to create a template for the multistage design used in tmt
#' 
#' This function creates a template for the definition of multistage designs as required by the estimation function (in multistage design cases). The defines multistage design is then handed over to the function\code{tmt_mstdesign}. Essentially, these are the modules, rules and path sections. In the formula-based notation, it is also possible to state additional conditions (constraints) that can be found in the data and are reflected in the multistage design.
#' 
#' @param formula   formula for the desired template of a multistage design. If formula is leaved empty, a matrix as MST design template is generated.
#' @param full      logical if the modules and rules sections should also be created
#' @param eval      logical should the text input be evaluated (e.g. 3:6 = c(3, 4, 5, 6))
#'
#' @author Jan Steinfeld
#' 
#' 
#' @example ./R/.example_tmt_msttemplate.R
#' 
#' @export
#' 
tmt_msttemplate <- function(formula = NULL, full = TRUE, eval = TRUE) {
    
    if (!is.null(formula)) {
    out <- list()

    constr <- gregexpr("~",formula)# match ~
    pls <- gregexpr("\\+[^=]",formula) # match + without = 
    cum <- gregexpr("\\+\\=",formula) # match +=

    constr.ct <- attr(constr[[1]],"match.length")
    pls.ct <- attr(pls[[1]],"match.length")
    cum.ct <- attr(cum[[1]],"match.length")

    constrl <- any(constr.ct > 0)
    plsl <- any(pls.ct > 0)
    cuml <- any(cum.ct > 0)

    elements <- rep(
        c("~","+","+=")[c(constrl,plsl,cuml)], 
        c(length(constr.ct), length(pls.ct), length(cum.ct))[c(constrl,plsl,cuml)]
    )

    frmsplit <- gsub("\\s","",formula)
    frmsplit <- strsplit(frmsplit,"~|\\+[^=]|\\+\\=")[[1]]
    # ------------------------------------------------------------------------------------
    # --------------------
    # modules
    # --------------------
    if (full) {
        modulesout <- list()
        # zunächst die '+' und '+=' heraussuchen.
        if (any(elements%in%c("+","+="))) {
            # +1 for laste entry
            for (i in seq_len(length(elements))+1) {
               if (c((elements%in%c("+","+="))[1],elements%in%c("+","+="))[i]) {
                  modeval <- strsplit(frmsplit[i],"\\(|,|\\)")[[1]]
                  if (length(grep("\\:",modeval))>0 && eval) {
                      modulesout[[length(modulesout) + 1]] <- paste0("\n",modeval[1]," =~ ",eval(parse(text = modeval[-1])))
                  } else {
                      modulesout[[length(modulesout) + 1]] <- c(paste0("\n#--------------------\n","# ",modeval[1],":","\n#--------------------", collapse = " "),
                      paste0("\n",modeval[-1]," =~ ", collapse = " ")
                      )
                  }  
               }
            }
            modules <- c("\n#--------------------",
            "\n# modules",
            "\n#--------------------",
            unlist(modulesout),"\n")  
        } else {
            modules <- c("\n#--------------------",
            "\n# modules",
            "\n#--------------------",
            "\nPlease fill here the specific modules\n",
            "\n")    
        }
    } else {
        modules <- c("\n#--------------------",
            "\n# modules",
            "\n#--------------------",
            "\nPlease fill here the specific modules\n",
            "\n")
    }
    out[[1]] <- modules
    # ------------------------------------------------------------------------------------
    # --------------------
    # rules (probs, deterministc)
    # --------------------
      path <- c("\n#--------------------",
      "\n# rules",
      "\n#--------------------",
      "\nPlease fill here the specific rules\n",
      "\n")
    out[[length(out) + 1]] <- path
    # ------------------------------------------------------------------------------------
    # --------------------
    # path
    # -------------------- 
    if (length(grep("\\(|\\)",frmsplit))!=0) {
        frmpreout <- sapply(frmsplit,strsplit,"\\(|,|\\)")
        frmout <- list()
        evalcrit <- c((elements%in%c("+","+="))[1],elements%in%c("+","+="))
        
        for (i in seq(evalcrit)) {
            if (evalcrit[i]) {
                if (length(grep("\\:",frmpreout[i]))>0 && eval) {
                    frmout[[length(frmout) + 1]] <- paste0(eval(parse(text = frmpreout[[i]][-1])),"( )")
                } else {
                    frmout[[length(frmout) + 1]] <- paste0(frmpreout[[i]][-1],"( )")
                }  
            } else {
                # crit = '~'
              if (length(grep("\\:",frmpreout[i])) > 0 && eval) {
                frmout[[length(frmout) + 1]] <- paste0(frmpreout[[i]][1],"(",eval(parse(text = frmpreout[[i]][-1])),")")
              } else {
                frmout[[length(frmout) + 1]] <- paste0(frmpreout[[i]][1],"(",frmpreout[[i]][-1],")")
              }
            }
        }
    }

    for(i in seq(elements)){
        frmout[[i]] <- paste0(frmout[[i]]," ",elements[i])
    }

    frmout <- expand.grid(frmout)
    frmout <- apply(frmout,1,function(x) {
        paste0("\n", paste(x,collapse = " "))
      })

    path <- c("\n#--------------------",
      "\n# mstdesign",
      "\n#--------------------",
      frmout,"\n")
    
    out[[length(out) + 1]] <- path
  # ------------------------------------------------------------------------------------
  # return structure
  cat(unlist(out))
  } else {
    out <- matrix("", nrow = 2, ncol = 7)
    colnames(out) <- c("mst", "minSolved", "maxSolved", "items", "minSolved_stage", "maxSolved_stage", "probability")
    out[1,] <- c("paths","for each module the minimum raw score, seperated by ';'","for each module the maximum raw score, seperated by ';'","for each module the specific items, seperated by ';'","for each path the cumulative minimum raw score, seperated by ';'","for each path the cumulative maximum raw score, seperated by ';'","for each path and module the probabilites (for deterministic routing use 1)")
    out[2,] <- c("^B4$-^B2$-^B1$", "0;0;0", "2;2;5", "i16,i17,i18,i19,i20;i6,i7,i8,i9,i10;i1,i2,i3,i4,i5", "0;0;0", "2;4;9", "1,1,1,1,1,1;1,1,1,1,1,1;1,1,1,1,1,1")
  return(out)
  }
}
