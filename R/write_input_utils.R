#' Path check
#'
#' This checks if a string is a valid file path. It returns a boolean value or vector of boolean values (if vector passed in)
#'
#' TODO: Where does this look? Will relative paths work?
#'
#' @param dictname string to check
#'
#' @return boolean
#' @keywords internal
fileinput <- function(dictname){
  bool <- c()
  for(i in 1:length(dictname)){
    if(is.character(dictname[[i]])){
      bool <- append(bool, (file.exists(dictname[i]) & !dir.exists(dictname[i])))
    } else {
      bool <- append(bool, FALSE)
    }
  }
  return(bool)
}

#' Checks whether an entry is a valid file, a valid actdata key, or a correctly formatted dataset. Throws an error otherwise.
#'
#' @param dict object to test
#'
#' @return character. file, key, df.
#' @keywords internal
dict_specs <- function(dict){
  types <- c()
  wrongformat <- FALSE
  if(length(dict) == 4){
    for(i in 1:length(dict)){
      d <- dict[[i]]
      if(is.character(d)){
        if(fileinput(d)){
          # input is a file string
          types <- append(types, "file")
        } else if (d %in% actdata::dataset_keys()){
          # input is a string representing a key in actdata
          types <- append(types, "key")
        } else {
          wrongformat <- TRUE
        }
      } else if (is.data.frame(d) | tibble::is_tibble(d)){
        # input is a data frame. There are other necessary format checks but these should happen in the actdata functions.
        types <- append(types, "df")
      } else {
        wrongformat <- TRUE
      }
    }
  } else {
    d <- dict
    if(is.character(d)){
      if(fileinput(d)){
        # input is a file string
        types <- append(types, "file")
      } else if (d %in% actdata::dataset_keys()){
        # input is a string representing a key in actdata
        types <- append(types, "key")
      } else {
        wrongformat <- TRUE
      }
    } else if (is.data.frame(d) | tibble::is_tibble(d)){
      # input is a data frame. There are other necessary format checks but these should happen in the actdata functions.
      types <- append(types, "df")
    } else {
      wrongformat <- TRUE
    }
  }

  if(wrongformat){
    stop("Provided dictionary is not a valid filepath, actdata dataset key, or EPA data frame or tibble.")
  }

  return(types)
}

#' Construct file string (dictionary)
#'
#' this constructs the correct file string from dictionary information
#' if the input is a name from actdata, it saves the dataset in the "actdata_dicts_eqns" folder in the working directory
#'
#' @param dict string
#' @param gender string (\code{"average"}, \code{"female"}, \code{"male"})
#' @param component string (\code{"identity"}, \code{"behavior"}, \code{"setting"}, \code{"modifier"})
#' @param stat string (\code{"mean"}, \code{"sd"}, \code{"cov"})
#' @param bayesact_dir string
#'
#' @return string filepath
#' @keywords internal
make_file_string <- function(dict, spec, key, gender, component, stat, bayesact_dir){
  # We have already checked for validity of everything before so we don't need to repeat that here. We have also reformatted the data frames where they have been provided.

  if(spec == "file"){
    # if the dict is a filepath, we need to save it to the data folder of the bayesact directory
    # use rstudioapi to move the file to avoid needing to load it and possibly messing with format
    termId <- rstudioapi::terminalExecute(command = paste0("cp ", dict, " ", file.path(bayesact_dir, "data")),
                                          show = FALSE)
    file <- basename(dict)
    wait_until_done(termId)
    rstudioapi::terminalKill(termId)
  } else if (spec == "key"){
    file <- save_dict_df(data = dict,
                         filename = construct_df_filename(key = key, gender = gender, component = component, stat = stat),
                         bayesact_dir = bayesact_dir)
  } else if (spec == "df"){
    # This has already been reformatted as necessary. Save it to the folder.
    file <- save_dict_df(data = dict,
                         filename = construct_df_filename(df = dict, component = component),
                         bayesact_dir = bayesact_dir)
  }

  # time <- 0
  # while(!exists(file) & time < 5){
  #   time <- time + .1
  #   Sys.sleep(.1)
  # }
  # if(!exists(file) | length(file) == 0 | !is.character(file) | file == ""){
  #   stop("problem with file name")
  # }
  return(file)
}


#' Construct file string (equation)
#'
#' this builds the correct filepath for the equations and checks if the specified gender is available.
#' if the equations are given as a filepath, it returns the filepath and ignores specified gender.
#' if the input is a name from actdata, it saves the dataset in the "actdata_dicts_eqns" folder in the working directory
#'
#' @param eqn string
#' @param gender string
#' @param component string (\code{"impression"}, \code{"emotion"})
#'
#' @return string filepath
#' @keywords internal
get_eqn_file <- function(key, gender, component, bayesact_dir){
  # if it is a valid filepath, need to copy it to the bayesact data directory
  # use the terminal to avoid having to read it in
  if(fileinput(key)){
    termId <- rstudioapi::terminalExecute(command = paste0("cp ", key, " ", file.path(bayesact_dir, "data")),
                                          show = FALSE)
    filename <- basename(key)
    wait_until_done(termId)
    rstudioapi::terminalKill(termId)
    return(filename)
  } else {
    # # we have already checked that the keyword is valid
    # # get the equation object associated with it
    # eq_obj <- actdata::this_dict(key, class = "equation")

    # # abbreviate gender terms
    # gender = standardize_option(gender, param = "gender", version = "eqn")
    # # gender[gender=="average"] <- "av"
    # # gender[gender=="female"] <- "f"
    # # gender[gender=="male"] <- "m"

    # we now have all components of the file name
    # name <- paste0(eq_obj@key, "_", component, "_", gender, "_eqn")
    name <- paste0(key, "_", component, "_", gender, "_eqn")

    # get the equation dataframe--this also checks validity
    eqndf <- actdata::get_eqn(key = key, equation_type = component, gender = gender)


    # save datafile from actdata to the actdata_dicts_eqns folder in the user's wd so bayesact can find it
    # return the file name
    return(save_eqn_actdata(data = eqndf, dataname = name, bayesact_dir))
  }
}


#' Expand string into vector of given length for line spec
#'
#' If object passed is already a vector, no change
#'
#' @param s the string/vector
#' @param len desired length
#'
#' @return vector of desired length
expand <- function(s, len){
  if(length(s) == 1){
    return(rep(s, len))
  } else if (length(s) == len){
    return(s)
  } else {
    stop("Incorrect entry length")
  }
}

#' standardize_option
#'
#' This function deals with abbreviations in parameter specification and returns the spellings that are used in the datasets.
#'
#' @param input the string to standardize
#' @param param the dictionary parameter expected (gender, component, stat)
#' @param version dict or eqn
#'
#' @return the standardized version of the input string
standardize_option <- function(input, param, version = "dict"){
  input <- trimws(tolower(input))
  for(i in 1:length(input)){
    if(param == "gender" & version == "dict"){
      check_abbrev(input, allowed = c("m", "male", "man", "f", "female", "woman", "a", "av", "average"))
      input[i] <- dplyr::case_when(substr(input[i], 1, 1) == "m" ~ "male",
                                   substr(input[i], 1, 1) == "a" ~ "average",
                                   substr(input[i], 1, 1) %in% c("f", "w") ~ "female",
                                   TRUE ~ input[i])
    } else if(param == "gender" & version == "eqn"){
      check_abbrev(input, allowed = c("m", "male", "man", "f", "female", "woman", "a", "av", "average"))
      input[i] <- dplyr::case_when(substr(input[i], 1, 1) == "m" ~ "m",
                                   substr(input[i], 1, 1) == "a" ~ "av",
                                   substr(input[i], 1, 1) %in% c("f", "w") ~ "f",
                                   TRUE ~ input[i])
    } else if(param == "component"){
      check_abbrev(input, allowed = c("behavior", "b", "beh", "behaviors", "behaviour", "behaviours",
                                      "modifier", "m", "mod", "modifiers",
                                      "identity", "i", "ident","identities"
                                      # "setting", "s", "set", "settings",
                                      ))
      input[i] <- dplyr::case_when(substr(input[i], 1, 1) == "b" ~ "behavior",
                                   substr(input[i], 1, 1) == "m" ~ "modifier",
                                   substr(input[i], 1, 1) == "i" ~ "identity",
                                   # substr(input[i], 1, 1) == "s" ~ "setting",
                                   TRUE ~ input[i])
    } else if(param == "stat"){
      check_abbrev(input, allowed = c("mean", "m", "sd", "standard deviation", "s", "cov", "covar", "covariance", "c"))
      input[i] <- dplyr::case_when(substr(input[i], 1, 1) == "m" ~ "mean",
                                   substr(input[i], 1, 1) == "s" ~ "sd",
                                   substr(input[i], 1, 1) == "c" ~ "cov",
                                   TRUE ~ input[i])
    } else (
      stop("Invalid parameter type provided.")
    )
  }
  return(input)
}
