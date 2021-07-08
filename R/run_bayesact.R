#' Run bayesact for a single simulation file
#'
#' Code adapted from that written by Jon Morgan and Kim Rogers
#'
#' TODO: There are more options here (see p. 7-8 of bayesactv2.pdf).
#'
#' @param simfilename the name of the simulation file
#' @param inputdir the directory the file is in
#' @param outputdir the directory in which to put the bayesact output
#' @param wd current working directory
#' @param bayesact_dir the top level directory where the bayesact code lives
#'
#' @return output file path
#'
#' @export
run_bayesact <- function(simfilename, bayesact_dir, inputdir = "bayesact_input", outputdir = "bayesact_output", wd = getwd()){
  # how to do terminal windows? Maybe this function should be wrapped in another function that essentially loops it, and the higher level function would create a single terminal instance.
  # For now, each run will create and then destroy a single terminal instance.
  # Does this have any prayer of working on windows?

  # if any of provided directories are relative paths, prepend wd and make them absolute paths
  outputdir <- absolute_path(outputdir, wd)
  inputdir <- absolute_path(inputdir, wd)
  bayesact_dir <- absolute_path(bayesact_dir, wd)

  # Does the output directory exist? If not, create.
  create_dir_if_needed(outputdir)

  # #Getting my terminal's unique ID to be sure I am sending commands to one terminal and not spawning a millions others.
  # rstudioapi::terminalActivate()
  # terminal_list <- rstudioapi::terminalList()
  # termId <- terminal_list[[1]]
  # print(termId)

  # check if the source directory has an executable, and if not, run make
  # a maybe safer option: throw an error and link to instructions that show how people can run make themselves
  # but this is harder for people
  source_dir <- file.path(bayesact_dir, "source")
  source_files <- list.files(source_dir)
  if(!("bayesactsim" %in% source_files)){
    # command_list <- vector('list', 3)
    # # move to the bayesact source directory
    # command_list[[1]] <- paste0("cd ", source_dir, "\n")
    # # then make to compile the c code
    # command_list[[2]] <- c('make\n')
    # # then back to the old wd
    # command_list[[3]] <- paste0("cd ", wd, "\n")
    # Sys.sleep(5)
    # for (i in seq_along(command_list)){
    #   rstudioapi::terminalSend(termId, command_list[[i]])
    #   print(paste("sent command", command_list[[i]]))
    # }

    print("Running 'make' in the bayesact source directory to create the executable")
    termId <- rstudioapi::terminalExecute(command = "make",
                                          workingDir = source_dir)

    # wait for it to finish before killing the terminal instance and moving on
    while (is.null(rstudioapi::terminalExitCode(termId))) {
      Sys.sleep(0.1)
    }
    rstudioapi::terminalKill(termId)
  }



  # the output file name will be the same as the input file name but with the csv extension
  outname <- paste0(regmatches(simfilename, regexpr("^[^\\.]*", simfilename)), ".csv ")
  outnametxt <- paste0(regmatches(simfilename, regexpr("^[^\\.]*", simfilename)), "_terminaltext.txt ")

  # # run the simulation
  # command_list <- vector('list', 3)
  # # move to the bayesact source directory
  # command_list[[1]] <- paste0("cd ", source_dir, "\n")
  # # then send command
  # command_list[[2]] <- paste0('./bayesactsim', " ", file.path(inputdir, simfilename),' -o ', file.path(outputdir, outname), ' > ', file.path(outputdir, outnametxt), ' \n')
  # # then back to the old wd
  # command_list[[3]] <- paste0("cd ", wd, "\n")
  # Sys.sleep(5)
  # for (i in seq_along(command_list)){
  #   rstudioapi::terminalSend(termId, command_list[[i]])
  #   print(paste("sent command", command_list[[i]]))
  # }


  termId <- rstudioapi::terminalExecute(command = paste0('./bayesactsim', " ", file.path(inputdir, simfilename),' -o ', file.path(outputdir, outname), ' > ', file.path(outputdir, outnametxt)),
                                        workingDir = source_dir)

  # wait for it to finish before killing the terminal instance
  while (is.null(rstudioapi::terminalExitCode(termId))) {
    Sys.sleep(0.1)
  }
  print("Bayesact run complete")
  rstudioapi::terminalKill(termId)

  # return the output file path
  return(file.path(outputdir, outname))
}
