#' Convert relative paths to absolute paths
#'
#' First checks if path is relative or absolute, and if relative prepends wd (or other given absolute path)
#'
#' @param path the path to check and convert to absolute
#' @param wd path to prepend to a relative path. Current working directory by default.
#'
#' @return absolute path
#' @keywords internal
absolute_path <- function(path, wd = getwd()){
  if(!grepl("^/", path)){
    path <- file.path(wd, path)
  }
  return(path)
}

#' Create directory if it doesn't already exist
#'
#' @param dir path to directory
#'
#' @return logical whether directory needed to be created or not
#' @keywords internal
create_dir_if_needed <- function(dir){
  if(!dir.exists(dir)){
    dir.create(dir)
    print(paste("Created directory at", dir))
    return(TRUE)
  }
  return(FALSE)
}
