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
    row[,i][[1]] <- list(trimws(strsplit(row[,i], ",")[[1]]))
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

  return(file[places,])
}
