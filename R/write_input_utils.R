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
    bool <- append(bool, (file.exists(dictname) & !dir.exists(dictname)))
  }
  return(bool)
}


#' Check input for agreement with provided lists
#'
#' It checks that filepaths are valid, but does not check whether the file contents are as required. I assume BayesACT does this?
#'
#' @param input vector
#' @param allowlist vector
#' @param allowlength numeric
#' @param allowsingle boolean
#' @param allowfile boolean
#'
#' @return
#' @keywords internal
check_input_list <- function(input, allowlist, allowlength, allowsingle, allowfile){
  # first check if input is a character vector
  if(is.character(input)){
    # then check length: must be either the given allowable length or length 1 if allowsingle is true
    if((length(input) == allowlength) | (allowsingle & length(input) == 1)) {
      for(entry in input){
        # then check that each entry is in the allowable list, or that they are valid filepaths (if allowed). Can mix and match.
        if(trimws(tolower(entry)) %in% allowlist){
          # is a valid entry
          return(TRUE)
        }
        else if (allowfile & fileinput(entry)) {
          # is a valid filepath and filepaths are allowed
          return(TRUE)
        }
        else {
          # not a valid list entry or a valid filepath
          message <- paste0("Provided input is invalid. Input must be in ", toString(allowlist))
          if(allowfile){
            message <- paste0(message, " or input must be a valid filepath.")
          }
          stop(message)
        }
      }
    }
    else {
      message <- paste0("Input list is of the incorrect length. Allowable length is ", toString(allowlength))
      if(allowsingle){
        message <- paste0(message, ". Single entries also allowed.")
      }
      stop(message)
    }
  }
  else{
    stop("Input must be a character vector")
  }
  return(TRUE)
}


#' Compatibility check: dictionary and type
#'
#' this checks that the provided dictionary has the provided type. This will look different for dictionaries accessed with keywords than dictionaries provided with a filepath.
#'
#' @param dictname string
#' @param dicttype string (\code{"mean"}, \code{"sd"}, or \code{"cov"})
#' @param dicts list of dictionary objects
#'
#' @return
#' @keywords internal
check_dict_type_compatibility <- function(dictname, dicttype, dicts){
  # only one dictionary has more than one type available: the 2015 georgia-duke survey. There is an SD and a COV version available. Jesse included this with the BayesACT package.
  # It is unclear who is represented in this dataset; the values seem not to match any of the four 2015 US datasets available on the UGA ACT website, though it seems like they should be the same study.
  # I have deleted Kim's synthetic identities from the bottom of the identity COV file.

  # loop through the list and check each dict/type combo
  for(i in 1:length(dictname)){
    name <- dictname[i]
    type <- dicttype[i]

    # Dictionary is one of the provided ones: check dictionary info
    if(name %in% dict_subset(dicts)){
      for(element in dicts){
        if(element@key == name){
          d <- element
          break
        }
      }

      if(!(type %in% d@types)){
        message <- paste0("Provided dictionary type ", type, " is not an option for dictionary ", name, ". Available types for this dictionary are ",  d@types, ".")
        stop(message)
      }
    }
  }
  # I think rather than implement checks for dictionaries read from files, I will just pass them to BayesACT and let BayesACT handle it. Probably not good to check things in more than one place.
}


#' Compatibility check: dictionary and component
#'
#' this checks that the provided dictionary does indeed exist for the required component (identity, behavior, modifier). This will look different when things are provided as a list versus as singular.
#'
#' @param dictname string
#' @param dicts list of dictionary objects
#'
#' @return
#' @keywords internal
check_dict_components <- function(dictname, dicts){
  # require different filepaths for identity, behavior, and modifier if provided as filepath
  if(fileinput(dictname)){
    if(length(dictname) == 4){
      if(length(unique(dictname)) < 3){
        stop("Must provide different files for identities, behaviors, and modifiers/emotions")
      }
    }
    else{
      stop("Wrong number of dictionary files provided")
    }
  }
  # dictionaries provided as keywords
  else{
    if(length(dictname) == 1){
      # if only a single dictionary is indicated, that dictionary must have an identity, behavior, and modifier component
      thisdict <- this_dict(dictname)
      if(('identities' %in% thisdict@components) & ("behaviors" %in% thisdict@components) & ("mods" %in% thisdict@components)){
        return(TRUE)
      }
      else{
        stop("The provided dictionary does not contain identities, behaviors, and modifiers.")
      }
    }
    else {
      # otherwise, first entry is identity, then behavior, then identity again, then modifier
      order <- c("identities", "behaviors", "identities", "modifier")
      for(i in 1:4){
        # check that the first dictionary has identities, the second has behaviors, and so on
        thisthing <- order[i]
        thisdictcomp <- this_dict(dictname)@components
        if(thisthing %in% thisdictcomp){
          return(TRUE)
        }
        else{
          stop(paste("Dictionary", dictname[i], "does not contain", thisthing))
        }
      }
    }
  }
}


#' Compatibility check: dictionary and gender
#'
#' this checks that the provided dictionary has the requested gender (if it is an included dict). This will look different when things are provided as a list versus as singular.
#'
#' @param dictname string
#' @param gender string (\code{"av"}, \code{"male"}, \code{"female"})
#' @param dicts list of dictionary objects
#'
#' @return
#' @keywords internal
check_dict_gender <- function(dictname, gender, dicts){
  # require different filepaths for identity, behavior, and modifier
  #  needed only if provided as filepath
  if(fileinput(dictname)){
    return()
  }
  # dictionaries provided as keywords
  else{
    for(d in dictname){
      thisdict <- this_dict(dictname)
      if(!(gender %in% thisdict@genders)){
        stop("At least one requested dictionary does not contain responses from requested gender")
      }
    }
  }
  return()
}


#' Construct file string (dictionary)
#'
#' this constructs the correct file string from dictionary information (dictionaries only, currently)
#'
#' @param dir string directory
#' @param dict string
#' @param gender string (\code{"av"}, \code{"male"}, \code{"female"})
#' @param type string (\code{"mean"}, \code{"sd"}, \code{"cov"})
#'
#' @return
#' @keywords internal
make_file_string <- function(dir, dict, gender, type){
  if(fileinput(dict)){
    return(dict)
  } else {
    g <- case_when(gender == "av" ~ "av",
                   gender == "female" ~ "f",
                   gender == "male" ~ "m")
    d <- this_dict(dict)
    return(paste0(dir, d@key, "_", type, "_", g, d@filetype))
  }
}


#' Construct file string (equation)
#'
#' this builds the correct filepath for the equations and checks if the specified gender is available. If the equations are given as a filepath, it returns the filepath and ignores specified gender.
#'
#' @param eqndir directory where equations are located
#' @param eqns string
#' @param gender string (\code{"neutral"}, \code{"male"}, \code{"female"})
#' @param type string (\code{"mean"}, \code{"sd"}, \code{"cov"})
#'
#' @return
#' @keywords internal
get_eqn_file <- function(eqndir, eqns, gender, type){
  # if it is a valid filepath, return the same filepath
  if(fileinput(eqns)){
    return(eqns)
  } else {
    # we have already checked that the keyword is valid
    # get the equation object associated with it
    if (type == "impression"){
      eq_obj <- this_dict(paste0(eqns, "_impressionabo"), class = "equation")
    } else if(type == "emotion"){
      eq_obj <- this_dict(paste0(eqns, "_emotionid"), class = "equation")
    } else {
      stop("Equation type is not valid.")
    }

    # is the specified gender available in the specified dictionary? If not, give an error
    has_gender <- sapply(gender, function(x) x %in% eq_obj@genders)
    if(!(TRUE %in% has_gender)){
      stop("Specified gender is not available for specified equations")
    }

    # all looks good; keyword is valid and gender is available
    # if gender is a list, take the first one that is available (has_gender has this information)
    gender_to_use <- names(has_gender[has_gender == TRUE])[1]

    if(gender_to_use == "neutral"){
      g <- "av"
    } else if(gender_to_use == "female"){
      g <- "f"
    } else {
      g <- "m"
    }
    # we now have all components of the file name
    return(paste0(eqndir, "/", eq_obj@key, "_", g, eq_obj@filetype))
  }
}

#' Insert lines into template
#'
#' @param file template object
#' @param lines lines to insert
#' @param start line after which to start
#' @param end line before which to end
#' @param insertAt string, whether to put as close as possible to the start or the end (\code{"start"}, \code{"end"})
#'
#' @return
#' @keywords internal
insertLines <- function(file, lines, start, end, insertAt = "end"){
  place1 <- vector()
  place2 <- vector()

  for(i in 1:nrow(file)){
    line <- file[i,]
    if(is.element(TRUE, grepl(start, line))){
      place1 <- append(place1, i)
    }

    if(is.element(TRUE, grepl(end, line))){
      place2 <- append(place2, i)
    }
  }

  # need a unique start
  if(length(place1) > 1){
    stop("Error in template file: multiple start point matches")
    } else if(length(place1) < 1) {
      stop("Error in template file: no start point matches")
      }

  # and need some endpoint that is greater than the start
  if(length(place2) < 1){
    stop("Error in template file: no endpoint match")
    }

  # but the end doesn't have to be unique. If more than one, pick the one that is first after the start
  if(insertAt == "end"){
    spot <- as.numeric(place2[place2 > place1][1]) - 1
  } else if(insertAt == "start"){
    spot <- as.numeric(place1)
  } else{
    stop("Invalid line entry position")
  }

  if(is.na(spot)){
    stop(paste0("Error: line entry point is NA. Place vectors are ", place1, " ", place2))
  }
  return(append(x = unlist(file), values = unlist(lines), after = spot))
}
