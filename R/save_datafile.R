#' Save actdata objects where bayesact can get them
#'
#' Need to move these files to a folder in the user's working directory so BayesACT can access them.
#' Make a directory "actdata_dicts_eqns" under the user's working directory (if necessary)
#'
#' @param dataname name of the dataset in actdata
#'
#' @return filename that the object got saved to
save_actdata_input <- function(dataname){


  # TEMPORARY FIX
  # path <- file.path(getwd(), "actdata_dicts_eqns")
  path <- "/Users/aidan/Desktop/School/Grad_school/bayesactgithub/bayesact/data"



  if(!dir.exists(path)){
    dir.create(path)
  }

  if(grepl("dict", dataname)){
    # the object is a dictionary
    class <- "dict"
    filename <- paste0(path, "/", dataname, ".csv")
  } else {
    # the object is an equation set
    class <- "eqn"
    filename = paste0(path, "/", dataname, ".dat")
  }

  save_for_bayesact(dataname, class = class, filename = filename)
  return(filename)
}


#' Save files where bayesact can find them
#'
#' @param dataname name of actdata object
#' @param class string "dict" or "eqn"
#' @param filename string filepath to save under
#'
#' @import actdata
save_for_bayesact <- function(dataname, class, filename){
  data <- get(dataname, asNamespace("actdata"))

  # if the dictionary is type "mean", it needs to have six EPA columns and an institution codes column
  # neither seems to be the case for COV and SD datasets but this needs to be checked.

  if(class == "dict"){
    if(!grepl("COV", dataname) & !grepl("SD", dataname)){
      cols <- ncol(data)
      # 4 or 5 columns: no duplicate set
      if(cols == 4 | cols == 5){
        data$E.2 <- data$E
        data$P.2 <- data$P
        data$A.2 <- data$A
      }
      # 4 or 7 columns: no institution codes
      if(cols == 4 | cols == 7){
        # add a filler row: 11 111111111 111
        data$instcodes <- rep("11 111111111 111", nrow(data))
      }
      # 8 columns now: good
      # else: some other error
      if(ncol(data) != 8){
        stop("Error in saving file for bayesact: wrong number of columns")
      }
    }
  }

  if(class == "dict"){
    utils::write.table(data, filename, sep = ",", row.names = FALSE, col.names = FALSE)
  } else {
    utils::write.table(data, filename, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  }

}

