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

  # check if the source directory has a shared library, and if not, run make
  # a maybe safer option: throw an error and link to instructions that show how people can run make themselves
  # but this is harder for people
  source_dir <- file.path(bayesact_dir, "source")
  source_files <- list.files(source_dir)
  if(!("libbayesact.so" %in% source_files)){
    # TODO: test this
    print("Running 'make' in the bayesact source directory to compile the BayesACT C code. See make_terminal_output.txt for output.")
    termId <- rstudioapi::terminalExecute(command = paste0("make > ", file.path(outputdir,"make_terminal_output.txt")),
                                          workingDir = source_dir,
                                          show = FALSE)

    # wait for it to finish before killing the terminal instance and moving on
    wait_until_done(termId)
    rstudioapi::terminalKill(termId)
  }

  # the output file name will be the same as the input file name but with the csv extension
  outname <- paste0(regmatches(simfilename, regexpr("^[^\\.]*", simfilename)), ".csv ")
  outnametxt <- paste0(regmatches(simfilename, regexpr("^[^\\.]*", simfilename)), "_terminaltext.txt ")
  termId <- rstudioapi::terminalExecute(command = paste0('./bayesactsim', " ", file.path(inputdir, simfilename),' -o ', file.path(outputdir, outname), ' > ', file.path(outputdir, outnametxt)),
                                        workingDir = source_dir,
                                        show = FALSE)

  # wait for it to finish before killing the terminal instance
  wait_until_done(termId)

  print("BayesACT run complete")
  rstudioapi::terminalKill(termId)

  # return the output file path
  return(file.path(outputdir, outname))
}

#' Pause until terminal process is done
#'
#' @param termId the terminal id that is running
#'
#' @return boolean success
wait_until_done <- function(termId){
  while (is.null(rstudioapi::terminalExitCode(termId))) {
    Sys.sleep(0.5)
  }
  return(TRUE)
}
