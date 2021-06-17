#' Save data files
#'
#' Adapted from https://stackoverflow.com/questions/21248065/r-rename-r-object-while-save-ing-it : see Marco Wirthlin's answer
#'
#' @param ... data object
#' @param name desired object name (filename without type and extension)
#' @param type data type (dict or eqn)
#'
#' @export
saveit <- function(..., name, type = NA) {
  if(is.na(type)){
    if(is.element(TRUE, grepl("identities", name)) |
       is.element(TRUE, grepl("behaviors", name)) |
       is.element(TRUE, grepl("mods", name)) |
       is.element(TRUE, grepl("settings", name))) {
      type <- "dict"
    } else if (is.element(TRUE, grepl("emotionid", name)) |
               is.element(TRUE, grepl("impressionabo", name)) |
               is.element(TRUE, grepl("selfdir", name)) |
               is.element(TRUE, grepl("traitid", name))){
      type <- "eqn"
    }
  }

  if(!(type %in% c("dict", "eqn"))){
    stop("Specify dataset type as dict or eqn")
  }

  # this does the .RData files
  x <- list(...)
  names(x) <- paste0(name, "_", type)
  save(list=names(x), file=paste0("data/", name, "_", type, ".RData"), envir=list2env(x))

  # this does the csvs
  write.csv(..., paste0("inst/extdata/", name, "_", type, ".csv"))
}
