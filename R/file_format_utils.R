#' Insert lines into template
#'
#' @param file template object
#' @param lines lines to insert
#' @param start line after which to start
#' @param end line before which to end
#' @param insertAt string, whether to put as close as possible to the start or the end (\code{"start"}, \code{"end"})
#'
#' @return file with additional lines
#' @keywords internal
insert_lines <- function(file, lines, start, end, insertAt = "end"){
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
  return(data.frame(append(x = unlist(file), values = unlist(lines), after = spot)))
}

#' Convert comma-separated strings in cell entries to lists when needed.
#'
#' Useful for when actor or interaction input is read in from a csv file.
#'
#' @param row the dataframe row to convert and return
#'
#' @return row with needed entries converted to lists
get_lists <- function(row){
  for(i in 1:ncol(row)){
    if(is.character(row[,i])){
      if(row[,i] != "" & !is.na(row[,i])){
        row[,i][[1]] <- list(trimws(strsplit(as.character(row[,i]), ",")[[1]]))
      }
    }
  }
  return(row)
}


#' Remove lines that contain a given string
#'
#' @param string the marker string
#' @param file the file to remove from
#'
#' @return file with lines containing string removed
remove_line <- function(string, file){
  places <- c()
  for(i in 1:nrow(file)){
    line <- file[i,]
    if(is.element(TRUE, grepl(string, line))){
      places <- append(places, FALSE)
    } else {
      places <- append(places, TRUE)
    }
  }

  return(data.frame(file[places,]))
}

#' Utility for building a line in the interaction section of the input file
#'
#' @param identities list of identities
#' @param probs list of probabilities
#'
#' @return text string to enter in line
get_actor_prob_line <- function(identities, probs){
  line <- ""

  probs <- add_leading_zeros(probs)

  for(i in 1:length(identities)){
    # when agent doesn't know object, object identity will be NA
    if(is.na(identities[i])){
      line <- " :"
    } else {
      line <- paste0(line, paste0(" : ", identities[i], " : ", probs[i]))
    }
  }

  return(line)
}

#' Add leading zero before decimal point to vectors of numbers if necessary
#'
#' @param vec vector of possibly decimal numbers
#'
#' @return formatted character vector with leading zero before decimal
add_leading_zeros <- function(vec){
  for(i in 1:length(vec)){
    if(!is.na(vec[i])){
      n <- as.numeric(vec[i])
      if(n < 1){
        vec[i] <- as.character(n)
      }
    }
  }

  return(vec)
}


#' Get lines to insert in file for agent optional arguments
#'
#' @param opt_args named list of optional arguments for one agent
#'
#' @return list of lines for file
get_agent_opt_arg_lines <- function(opt_args){
  lines <- c()
  argnames <- names(opt_args)
  if("alphas" %in% argnames){
    a <- opt_args$alphas[[1]]
    a <- add_leading_zeros(a)
    lines <- append(lines, paste0("alphas: ", paste(a, collapse = " : ")))
  }
  if("betas" %in% argnames){
    a <- opt_args$betas[[1]]
    a <- add_leading_zeros(a)
    lines <- append(lines, paste0("betas: ", paste(a, collapse = " : ")))
  }
  if("deltas" %in% argnames){
    a <- opt_args$deltas[[1]]
    a <- add_leading_zeros(a)
    lines <- append(lines, paste0("deltas: ", paste(a, collapse = " : ")))
  }
  if("numsamples" %in% argnames){
    a <- opt_args$numsamples[[1]]
    lines <- append(lines, paste0("numsamples: ", a))
  }
  return(lines)
}

#' Get lines to insert in file for interaction optional arguments
#'
#' @param opt_args optional arguments
#'
#' @return list of lines for file
get_interaction_opt_arg_lines <- function(opt_args){
  lines <- c()
  argnames <- names(opt_args)
  if("institution" %in% argnames){
    a <- opt_args$institution[[1]]
    lines <- append(lines, paste0("institution : ", paste(a, collapse = " : ")))
  }
  if("rseed" %in% argnames){
    a <- opt_args$rseed[[1]]
    lines <- append(lines, paste0("rseed: ", a))
  }
  return(lines)
}

#' extract_dict_list
#'
#' Handles dict input from nodelist tibbles.
#' This can be:
#' 1. a list containing one tibble with four rows containing four strings (if actdata keys or filepaths)
#' 2. or a list of four tibbles representing dictionaries (if a df was passed)
#' 3. A mix of these two formats
#'
#' These are not checked for validity here; that happens later. TODO: Think about whether this check should be moved up (possibly to the nodelist creation).
# Pro: catching mistakes earlier. Con: then the checks have to be repeated, because users might change the cell contents between nodelist creation and input df writing.
#'
#' @param input the input passed from the nodelist df
#'
#' @return a list with four string entries or a tibble with four nested tibble entries
extract_dict_list <- function(input){
  input <- input[[1]]
  wrongLength <- FALSE
  type <- "df"

  # are all entries strings? If so it will be in a bit of a different format.
  if(tibble::is_tibble(input)){
    if(nrow(input) != 4 | ncol(input) != 1){
      wrongLength <- TRUE
    } else {
      type <- "allstrings"
    }
  } else if(length(input) != 4){
    wrongLength <- TRUE
  }

  if(wrongLength){
    stop("Dictionary input is of incorrect length.")
  }

  out <- list()
  for(i in 1:4){
    # unnest each entry in the correct way according to its type
    if(type == "allstrings"){
      # all entries are keys or files
      out <- append(out, input[[i,1]])
    } else if(typeof(input[[i]]) == "character"){
      # there is a mix of dfs and strings and this one is a string
      out <- append(out, input[[i]])
    } else if (typeof(input[[i]]) == "list"){
      # this one is a df
      out <- append(out, input[i])
    }
  }
  return(out)

  # wronglength <- FALSE
  # # is this a list of strings or a list of tibbles?
  # if(typeof(input[[1]][[1]]) == "character"){
  #   type = "string"
  #   if(length(input[[1]][[1]]) != 4){
  #     wronglength <- TRUE
  #   }
  # } else if(typeof(input[[1]][[1]]) == "list"){
  #   type = "df"
  #   if(length(input[[1]]) != 4){
  #     wronglength <- TRUE
  #   }
  # } else {
  #   stop("Dictionary input is of incorrect format.")
  # }
  #
  # if(wronglength){
  #   stop("Dictionary input is of incorrect length.")
  # }
  #
  # if(type == "string"){
  #   out <- c(unlist(input)[[1]], unlist(input)[[2]], unlist(input)[[3]], unlist(input)[[4]])
  # } else {
  #   out <- list(input[[1]][[1]], input[[1]][[2]], input[[1]][[3]], input[[1]][[4]])
  # }
  # return(out)
}
