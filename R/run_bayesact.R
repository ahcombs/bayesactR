#' Run bayesact for a single simulation file
#'
#' Code adapted from that written by Jon Morgan and Kim Rogers
#'
#' TODO: There are more options here (see p. 7-8 of bayesactv2.pdf).
#'
#' @param simfilename the name of the simulation file
#' @param inputdir the directory the file is in
#' @param bayesact_source_dir the directory the bayesact source code is in
#' @param outputdir the directory in which to put the bayesact output
#' @param wd current working directory
#'
#' @return output file path
#'
#' @export
run_bayesact <- function(simfilename, bayesact_source_dir, inputdir = "bayesact_input_files", outputdir = "bayesact_output", wd = getwd()){
  # how to do terminal windows? Maybe this function should be wrapped in another function that essentially loops it, and the higher level function would create a single terminal instance.
  # For now, each run will create and then destroy a single terminal instance.
  # Does this have any prayer of working on windows?

  # Does the output directory exist? If not, create.
  if(!dir.exists(outputdir)){
    dir.create(outputdir)
  }

  #Getting my terminal's unique ID to be sure I am sending commands to one terminal and not spawning a millions others.
  rstudioapi::terminalActivate()
  terminal_list <- rstudioapi::terminalList()
  termId <- terminal_list[[1]]

  # check if the source directory has an executable, and if not, run make
  # a maybe safer option: throw an error and link to instructions that show how people can run make themselves
  # but this is harder for people
  source_files <- list.files(bayesact_source_dir)
  if(!("bayesactsim" %in% source_files)){
    command_list <- vector('list', 3)
    # move to the bayesact source directory
    command_list[[1]] <- paste0("cd ", bayesact_source_dir, "\n")
    # then make to compile the c code
    command_list[[2]] <- c('make\n')
    # then back to the old wd
    command_list[[3]] <- paste0("cd ", wd, "\n")
    for (i in seq_along(command_list)){
      rstudioapi::terminalSend(termId, command_list[[i]])
    }
  }

  # the output file name will be the same as the input file name but with the csv extension
  outname <- paste0(regmatches(simfilename, regexpr("^[^\\.]*", simfilename)), ".csv ")

  # run the simulation
  command_list <- vector('list', 3)
  # move to the bayesact source directory
  command_list[[1]] <- paste0("cd ", bayesact_source_dir, "\n")
  # then send command
  command_list[[2]] <- paste0(file.path(bayesact_source_dir, 'bayesactsim'), " ", file.path(inputdir, simfilename),' -o ', file.path(outputdir, outname), '\n')
  # then back to the old wd
  command_list[[3]] <- paste0("cd ", wd, "\n")
  for (i in seq_along(command_list)){
    rstudioapi::terminalSend(termId, command_list[[i]])
  }
  rstudioapi::terminalKill(termId)
  Sys.sleep(10)

  # return the output file path
  return(file.path(outputdir, outname))
}
