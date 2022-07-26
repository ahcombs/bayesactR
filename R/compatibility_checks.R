#' Check input for agreement with provided lists
#'
#' It checks that filepaths are valid, but does not check whether the file contents are as required. I assume BayesACT does this?
#'
#' @param input vector
#' @param allowlist vector
#' @param allowlength numeric
#' @param allowsingle logical
#' @param allowfile logical
#' @param checkindex numeric, the index to verify if the list length is greater than one. Default is NA meaning all indices are verified.
#'
#' @return boolean true if successful check
#' @keywords internal
check_input_list <- function(input, allowlist, allowlength, allowsingle, allowfile = FALSE, checkindex = NA){
  valid <- c()
  # first check if input is a character vector
  if(!is.character(input)){
    stop("Input must be a character vector")
  }

  # then check length: must be either the given allowable length or length 1 if allowsingle is true
  if((length(input) == allowlength) | (allowsingle & length(input) == 1)) {
    if(is.na(checkindex)){
      for(entry in input){
        # then check that each entry is in the allowable list, or that they are valid filepaths (if allowed). Can mix and match.
        if(trimws(tolower(entry)) %in% allowlist){
          # is a valid entry
          valid <- append(valid, TRUE)
        }
        else if (allowfile & fileinput(entry)) {
          # is a valid filepath and filepaths are allowed
          valid <- append(valid, TRUE)
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
    } else {
      if(length(input) > 1){
        thisindex <- checkindex
      } else {
        thisindex <- 1
      }

      if((trimws(tolower(input[thisindex])) %in% allowlist) | (allowfile & fileinput(input[thisindex]))){
        valid <- append(valid, TRUE)
      } else {
        stop(paste0("Provided input is invalid. Input must be in ", toString(allowlist)))
      }
    }
  } else {
    message <- paste0("Input list is of the incorrect length. Allowable length is ", toString(allowlength))
    if(allowsingle){
      message <- paste0(message, ". Single entries also allowed.")
    }
    stop(message)
  }
  return(TRUE)
}


#' Compatibility check: dictionary and stat
#'
#' this checks that the provided dictionary has the provided stat. This will look different for dictionaries accessed with keywords than dictionaries provided with a filepath.
#'
#' @param dictname string
#' @param dictstat string (\code{"mean"}, \code{"sd"}, or \code{"cov"})
#'
#' @return boolean for successful check
#' @keywords internal
check_dict_stat <- function(dict, dictstat, indices = c(1, 2, 3, 4)){
  dicts <- actdata::get_dicts()

  # loop through the list and check each dict/stat combo
  for(i in indices){
    name <- dict[i]
    stat <- dictstat[i]

    # Dictionary is one of the provided ones: check dictionary info
    if(name %in% actdata::dataset_keys(dicts)){
      for(element in dicts){
        if(element@key == name){
          d <- element
          break
        }
      }

      if(!(stat %in% d@stats)){
        message <- paste0("Provided dictionary stat ", stat, " is not an option for dictionary ", name, ". Available stats for this dictionary are ",  d@stats, ".")
        stop(message)
      }
    }
  }
  return(TRUE)
}


#' Compatibility check: dictionary and component
#'
#' this checks that the provided dictionary does indeed exist for the required component (identity, behavior, modifier).
#'
#' @param dictname length 4 list, order: identity, behavior, identity, mod
#'
#' @return boolean for successful check
#' @keywords internal
check_dict_components <- function(dictname, indices = c(1, 2, 3, 4)){
  # files <- fileinput(dictname)
  # valid <- c()
  order <- c("identity", "behavior", "identity", "modifier")
  for(i in indices){
    # entry is a filepath
    # if(files[i]){
    #   valid <- append(valid, TRUE)
    # } else {
      # entry is a keyword (we have checked validity already)--check for correct component
      thisthing <- order[i]
      thisdictcomp <- actdata::this_dict(dictname[i])@components
      if(!(thisthing %in% thisdictcomp)){
        stop(paste("Dictionary", dictname[i], "does not contain", thisthing))
      }
      # valid <- append(valid, TRUE)
    # }

    # # check that if more than one of identities, behaviors, and mods are provided as filepaths, those filepaths are unique
      # I don't think this is necessary; someone could make a dummy dictionary set where these are all the same.
    # if(sum(files, na.rm = TRUE) > 1){
    #   if((files[1] & files[2] & dictname[1] == dictname[2]) |
    #      (files[1] & files[4] & dictname[1] == dictname[4]) |
    #      (files[2] & files[4] & dictname[2] == dictname[4]) |
    #      (files[3] & files[2] & dictname[3] == dictname[2]) |
    #      (files[3] & files[4] & dictname[3] == dictname[4]))
    #      {
    #     stop(message = "Filepaths for identities, behaviors, and modifiers must be unique.")
    #   }
    # }
  }
  return(TRUE)
}


#' Compatibility check: dictionary and gender
#'
#' this checks that the provided dictionary has the requested gender (if it is an included dict). This will look different when things are provided as a list versus as singular.
#'
#' @param dictname string or length 4 vector
#' @param gender string or length 4 vector (\code{"av"}, \code{"m"}, \code{"f"})
#'
#' @return boolean for successful check
#' @keywords internal
check_dict_gender <- function(dictname, gender, indices = c(1, 2, 3, 4)){
  # check for file inputs--if all four entries are file inputs, no need to check
  # file <- TRUE
  # for(i in indices){
  #   if(!fileinput(dictname[i])){
  #     file <- FALSE
  #   }
  # }
  # if(file){
  #   return(TRUE)
  # }
  # dictionaries provided as keywords
  # else{
    # dictionary provided as list length 4
    # if(length(dictname) == 4){
      for(i in indices){
        # is this entry a file? If so skip this check
        # if(!fileinput(dictname[i])){
          d <- dictname[i]
          # if(length(gender) == 4){
            g <- gender[i]
          # } else {
          #   g <- gender
          # }
          thisdict <- actdata::this_dict(d)
          if(!(g %in% thisdict@genders)){
            stop("At least one requested dictionary does not contain responses from requested gender")
          }
        }
      # }
    # }
    # dictionary provided as single keyword
    # else {
    #   thisdict <- actdata::this_dict(dictname)
    #   # gender still may be length 4
    #   if(length(gender) == 4){
    #     for(i in 1:4){
    #       g <- gender[i]
    #       if(!(g %in% thisdict@genders)){
    #         stop("At least one requested dictionary does not contain responses from requested gender")
    #       }
    #     }
    #   } else {
    #     if(!(gender %in% thisdict@genders)){
    #       stop("At least one requested dictionary does not contain responses from requested gender")
    #     }
    #   }
    # }
    return(TRUE)
  # }
}


#' Check that probabilities given sum to 1
#'
#' @param agent_ident_prob list of agent probabilities
#' @param object_ident_prob list of object probabilities
#'
#' @return boolean successful test
check_probs <- function(agent_ident_prob, object_ident_prob){
  agent_ident_prob <- as.numeric(agent_ident_prob)
  if(sum(agent_ident_prob) != 1){
    stop("Agent identity probabilities do not sum to 1.")
  }
  if(!anyNA(object_ident_prob)){
    object_ident_prob <- as.numeric(object_ident_prob)
    if(sum(object_ident_prob) != 1){
      stop(paste0("Object identity probabilities ", object_ident_prob," do not sum to 1."))
    }
  }
  return(TRUE)
}


#' Check that all identities have a corresponding probability and vice versa
#'
#' Are lists the same length?
#'
#' @param ident identity list
#' @param prob probability list
#'
#' @return boolean for successful check
check_identity_prob_match <- function(ident, prob){
  if(length(ident) == length(prob)){
    return(TRUE)
  } else {
    stop("Length of identity lists must match length of probability lists")
  }
}

#' Check agent optional argument input
#'
#'institution, alphas, betas, deltas, numsamples
#'
#' @param opt_args named vector of provided optional arguments
#'
#' @return boolean for successful check
check_agent_opt_args <- function(opt_args){
  for(arg in names(opt_args)){
    # arguments must be one of alphas, betas, deltas, numsamples. Warn if not.
    validargs <- c("alphas", "betas", "deltas", "numsamples")
    if(!(arg %in% validargs)){
      warning(paste0(arg, " is not a recognized agent parameter and will be ignored. Valid parameters are ", paste(validargs, collapse = ", "), "."))
    }
    # alphas, betas, deltas, numsamples cannot be negative and must be correct length
    else if (arg %in% c("alphas", "betas", "deltas", "numsamples")){
      for(val in unlist(opt_args[arg][[1]])){
        if(as.numeric(val) <= 0){
          stop("alphas, betas, deltas, and numsamples must be positive")
        }
      }

      if(arg == "alphas"){
        l <- c(3, 1)
      } else if(arg %in% c("betas", "deltas")){
        l <- c(2, 1)
      } else{
        l <- c(1)
      }

      if(!(length(unlist(opt_args[arg][[1]])) %in% l)){
        stop(message = paste0("Length of ", arg, " must be ", paste(l, collapse = " or "), "."))
      }
    }
  }
  return(TRUE)
}

#' Check institution optional arguments
#'
#' @param opt_args named list of optional arguments
#'
#' @return boolean for successful check
check_interaction_opt_args <- function(opt_args){
  for(arg in names(opt_args)){
    # arguments must be one of institution, rseed. Warn if not.
    validargs <- c("institution", "rseed")
    if(!(arg %in% validargs)){
      warning(paste0(arg, " is not a recognized interaction parameter and will be ignored. Valid parameters are ", paste(validargs, collapse = ", "), "."))
    }
    # institutions must be in given list
    if(arg == "institution"){
      # check that the given institution is a valid one
      valid_insts <- c("overt","surmised","lay","business","law","politics","academe","medicine","religion","family","sexual","monadic","group","corporal")
      this_inst <- tolower(trimws(unlist(opt_args$institution)))
      for(inst in this_inst){
        if(!(inst %in% valid_insts)){
          stop(paste0("Provided institution ", inst, " is invalid. Valid institutions are: ", paste(valid_insts, collapse = ", ")))
        }
      }
    }
    else if (arg == "rseed"){
      if(length(opt_args[arg][[1]]) > 1){
        stop("Must only provide one seed value.")
      }
      s <- as.numeric(opt_args[arg][[1]])
      if(s != round(s)){
        stop("Seed value must be an integer")
      }
    }
  }
  return(TRUE)
}

#' Check events file input for format errors
#'
#' @param events dataframe
#'
#' @return boolean successful check
check_events <- function(events){
  # file format: six columns
  cols <- c("agent", "agent_action", "agent_emotion", "object", "object_action", "object_emotion")
  if(!identical(names(events), cols)){
    stop(paste0("Events file must have columns ", paste(cols, collapse = ", "), " (not all need be populated)."))
  }

  # at least one of agent_behavior/client_behavior must have an entry for each row
  for(i in 1:nrow(events)){
    if((is.na(events$agent_action[i]) | events$agent_action[i] == "") & (is.na(events$object_action[i]) | events$object_action[i] == "")){
      stop("Either agent or object must act on each turn")
    }
  }

  # TODO in future: check that provided behaviors are in the dictionary
  # (not critical; bayesact checks for this too -- but checking here first would allow errors to be caught before sinking time into simulation)

  return(TRUE)
}

#' Check abbreviation validity
#'
#' @param value the entry to check
#' @param allowed the list of allowed abbreviations/alternate spellings
#'
#' @return logical indicating success
check_abbrev <- function(value, allowed){
  for(v in value){
    if(!(v %in% allowed)){
      stop(paste0("Invalid input '", v, ".'"))
    }
  }
  return(TRUE)
}
