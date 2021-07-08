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
    bool <- append(bool, (file.exists(dictname[i]) & !dir.exists(dictname[i])))
  }
  return(bool)
}

#' Construct file string (dictionary)
#'
#' this constructs the correct file string from dictionary information
#' if the input is a name from actdata, it saves the dataset in the "actdata_dicts_eqns" folder in the working directory
#'
#' @param dict string
#' @param gender string (\code{"av"}, \code{"female"}, \code{"male"})
#' @param component string (\code{"identities"}, \code{"behaviors"}, \code{"settings"}, \code{"mods"})
#'
#' @return string filepath
#' @keywords internal
make_file_string <- function(dict, gender, component, type, bayesact_dir){
  if(fileinput(dict)){
    return(dict)
  } else {
    g <- dplyr::case_when(gender == "av" ~ "av",
                   gender == "female" ~ "f",
                   gender == "male" ~ "m")
    d <- actdata::this_dict(dict)
    if(type == "mean"){
      name <- paste0(d@key, "_", component, "_", g, "_dict")
    } else {
      upper_type <- toupper(type)
      name <- paste0(d@key, "_", component, "_", g, "_", upper_type, "_dict")
    }
    return(save_actdata_input(name, bayesact_dir))
    # return(paste0("actdata_dicts_eqns/", name, ".csv"))
  }
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
  # if it is a valid filepath, return the same filepath
  if(fileinput(key)){
    return(key)
  } else {
    # we have already checked that the keyword is valid
    # get the equation object associated with it
    eq_obj <- actdata::this_dict(key, class = "equation")

    # abbreviate gender terms
    gender[gender=="average"] <- "av"
    gender[gender=="female"] <- "f"
    gender[gender=="male"] <- "m"
    #
    # # is the specified gender available in the specified dictionary? If not, give an error
    # eqnset_components <- eq_obj@gendercomponents[component == regmatches(eq_obj@gendercomponents, regexpr("^[[:alnum:]]*", eq_obj@gendercomponents))]
    # eqnset_genders <- regmatches(eqnset_components, regexpr("[[:alnum:]]*$", eqnset_components))
    # has_gender <- sapply(gender, function(x) x %in% eqnset_genders)
    # if(!(TRUE %in% has_gender)){
    #   stop("Specified gender is not available for specified equations")
    # }

    # all looks good; keyword is valid and gender is available
    # if gender is a list, take the first one that is available (has_gender has this information)
    # gender_to_use <- names(has_gender[has_gender == TRUE])[1]

    # we now have all components of the file name
    name <- paste0(eq_obj@key, "_", component, "_", gender, "_eqn")

    # save datafile from actdata to the actdata_dicts_eqns folder in the user's wd so bayesact can find it

    # # return the file location
    # return(paste0("actdata_dicts_eqns/", name, ".dat"))
    return(save_actdata_input(name, bayesact_dir))
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
