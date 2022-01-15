#' Save files where bayesact can find them
#'
#' @param bayesact_dir top level of bayesact code directory
#' @param dataname name of actdata object
#'
#' @import actdata
save_eqn_actdata <- function(dataname, bayesact_dir){
  # TODO this is SUPER sensitive to the input format of the dataframe. Does it work with every actdata dataset?
  data <- get(dataname, asNamespace("actdata"))
  filename <- paste0(dataname, ".dat")
  filepath <- file.path(bayesact_dir, "data", filename)
  utils::write.table(data, filepath, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
}

#' Given a key and assorted information OR a dataframe, construct and return a sensible file name.
#'
#' This will be in format "key_gender_component_stat.csv" if the dict was provided via actdata key,
#' or "dfname.csv" if the dict was provided as a dataframe.
#'
#' These are all dictionaries and so should be csvs
#'
#' @param df dataframe object
#' @param key key string
#' @param gender gender string
#' @param component component string
#' @param stat stat string
#'
#' @return string with the filename
construct_df_filename <- function(df = NA, key = "", gender = "", component = "", stat = ""){
  if(!is.data.frame(df) & !tibble::is_tibble(df)){
    file <- paste0(paste0(key, "_", gender, "_", component, "_", stat), ".csv")
  } else {
    file <-  paste0("dict_", component,".csv")
  }
  return(file)
}

#' Save dataframe dictionary
#'
#' The newest shiniest save function
#'
#' @param data data object to save
#' @param bayesact_dir filepath to bayesact toplevel directory
#' @param filename name to save under (and return)
#'
#' @return file name
save_dict_df <- function(data, filename, bayesact_dir){
  orig_filename <- filename
  save <- TRUE
  dirpath <- file.path(bayesact_dir, "data")
  create_dir_if_needed(dirpath)

  filename_noext <- gsub("\\.csv", "", filename)
  fileregex <- paste0("^", filename_noext, ".*")

  allversions <- list.files(dirpath, pattern = fileregex)

  # DOES A FILE WITH THIS NAME EXIST HERE ALREADY? IS IT THE SAME?
  # WE ALSO HAVE TO CHECK ALL THE OTHER SUBSET VERSIONS
  for(filename in allversions){
    filepath <- paste0(dirpath, "/", filename)
    otherfile <- utils::read.table(filepath, sep = ",", header = FALSE)
    # if the file is the same in dimensions and elements, don't need to resave
    if(all(dim(data) == dim(otherfile))){
      if((all(data == otherfile))){
        save <- FALSE
        filename_to_return <- filename
      }
    }
  }
  if(save){
    # another file exists under the same name but it is not the same file. Save the new one under a new suffix.
    # is there already a numeric suffix? increment if so
    filepath <- paste0(dirpath, "/", orig_filename)
    while(file.exists(filepath)){
      suffix <- as.numeric(regmatches(filename, regexpr("[[:digit:]]+(?=\\.csv)", filename, perl = TRUE)))
      suffix <- ifelse(length(suffix) == 0, 1, suffix + 1)
      filename <- sub("_*[[:digit:]]*\\.csv", paste0("_", as.character(suffix), ".csv"), filename, perl = TRUE)
      filepath <- paste0(dirpath, "/", filename)
    }

    filename_to_return <- filename
    utils::write.table(data, filepath, sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
  }

  return(filename_to_return)
}
