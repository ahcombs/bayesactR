#' Save actdata objects where bayesact can get them
#'
#' Need to move these files to a folder in the user's working directory so BayesACT can access them.
#' Make a directory "actdata_dicts_eqns" under the user's working directory (if necessary)
#'
#' @param dataname name of the dataset in actdata
save_actdata_input <- function(dataname){
  path <- file.path(getwd(), "actdata_dicts_eqns")

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

  if(class == "dict"){
    utils::write.table(data, filename, sep = ",", row.names = FALSE, col.names = FALSE)
  } else {
    utils::write.table(data, filename, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  }

}

