#' Run bayesact for a single simulation file
#'
#' Code adapted from that written by Jon Morgan and Kim Rogers
#'
#' TODO: There are more options here (see p. 7-8 of bayesactv2.pdf).
#'
#' @param simfilename the name of the simulation file
#' @param input_dir the directory the file is in
#' @param output_dir the directory in which to put the bayesact output
#' @param wd current working directory
#' @param bayesact_dir the top level directory where the bayesact code lives
#'
#' @return output file path
#'
#' @export
run_bayesact <- function(simfilename, bayesact_dir, input_dir = "bayesact_input", output_dir = "bayesact_output", wd = getwd()){
  # how to do terminal windows? Maybe this function should be wrapped in another function that essentially loops it, and the higher level function would create a single terminal instance.
  # For now, each run will create and then destroy a single terminal instance.
  # Does this have any prayer of working on windows?

  # if any of provided directories are relative paths, prepend wd and make them absolute paths
  output_dir <- absolute_path(output_dir, wd)
  input_dir <- absolute_path(input_dir, wd)
  bayesact_dir <- absolute_path(bayesact_dir, wd)

  # Does the output directory exist? If not, create.
  create_dir_if_needed(output_dir)

  # check if the source directory has a shared library, and if not, run make
  # a maybe safer option: throw an error and link to instructions that show how people can run make themselves
  # but this is harder for people
  source_dir <- file.path(bayesact_dir, "source")
  source_files <- list.files(source_dir)
  if(!("libbayesact.so" %in% source_files)){
    # TODO: test this
    print("Running 'make' in the bayesact source directory to compile the BayesACT C code. See make_terminal_output.txt for output.")
    termId <- rstudioapi::terminalExecute(command = paste0("make"),
                                          workingDir = source_dir,
                                          show = FALSE)

    #  > ", file.path(outputdir,"make_terminal_output.txt"), " 2>&1"

    # wait for it to finish before writing output, checking exit status, and killing process
    wait_until_done(termId)

    # check exit status
    check_exit(termId, "make_terminal_output.txt")

    # write the contents of the terminal to file
    utils::write.table(rstudioapi::terminalBuffer(termId), file = file.path(output_dir,"make_terminal_output.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

    rstudioapi::terminalKill(termId)
  }

  # the output file name will be the same as the input file name but with the csv extension
  outname <- paste0(regmatches(simfilename, regexpr("^[^\\.]*", simfilename)), ".csv ")
  outnametxt <- paste0(regmatches(simfilename, regexpr("^[^\\.]*", simfilename)), "_terminaltext.txt")
  termId <- rstudioapi::terminalExecute(command = paste0('./bayesactsim', " ", file.path(input_dir, simfilename),' -o ', file.path(output_dir, outname), ' -v'),
                                        workingDir = source_dir,
                                        show = FALSE)

  #  > ', file.path(outputdir, outnametxt)

  # wait for it to finish before writing output, checking exit status, and killing process
  wait_until_done(termId)

  # check exit status
  check_exit(termId, outnametxt)

  # write the contents of the terminal to file
  utils::write.table(rstudioapi::terminalBuffer(termId), file = file.path(output_dir, outnametxt), quote = FALSE, row.names = FALSE, col.names = FALSE)

  rstudioapi::terminalKill(termId)

  # return the output file path
  return(file.path(output_dir, outname))
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

#' Check whether BayesACT completed successfully and print a warning if not.
#'
#' @param outfile path to terminal output file
#' @param termId the terminal process id
check_exit <- function(termId, outfile){
  exit <- rstudioapi::terminalExitCode(termId)
  if(!is.null(exit) & exit != 0){
    warning(paste0("BayesACT run completed with a non-zero terminal exit code, indicating a possible problem. The terminal exit code was ", exit, ". Check terminal output file ", outfile, " for more detail."))
  } else if (exit == 0){
    print("BayesACT completed successfully (terminal exit code 0).")
  } else {
    print("BayesACT still running (terminal exit code null). This may indicate a problem.")
  }
}
