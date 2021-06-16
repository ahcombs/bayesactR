#' Write BayesACT input file
#'
#' Write out a text file in the format required by BayesACT containing all necessary information for all actors
#'
#' @param nodelist a dataframe giving dictionary information for each actor
#' @param edgelist a dataframe delineating starting parameters (actor identity vector, object identity vector, probabilities) for each dyad
#'
#' @return
#' @export
write_input_file <- function(nodelist, edgelist){
  # setup: read in the template so we can add to it.

  # TODO: dataframe input--each row is a run. Load from CSV or build in R directly
  # TODO: parameter generation
  # TODO: documentation--build on Jesse's vignette?
  # TODO: documentation for each function
  # TODO: Dictionary subsetting functionality--specifically terms that are in two dictionaries

  # TODO: This path must be changed to be portable. Not sure how to do this in packages.
  # TODO: Remove setwd
  template <- read.delim("input_simulation_template.txt", stringsAsFactors = FALSE, header = FALSE)


  # agents: for each agent, call the individual agent function


  # interactions: for each interaction, call the individual interaction function


  # events: number of simulations and location of events file, if applicable. Also there are some event options. Write info to template.
}


#' Write agent into input
#'
#' Compile all necessary information for an individual agent and add it to a template object that can be written to a text file
#'
#' @param agenttext file template to add agent information to
#' @param name string agent name
#' @param dictname string, key of an available dictionary OR filepath
#' @param dicttype string (\code{"mean"}, \code{"sd"}, or \code{"cov"})
#' @param dictgender string (\code{"av"}, \code{"male"}, or \code{"female"})
#' @param impression_eqns string, key of an available equation set OR filepath
#' @param emotion_eqns string, key of an available equation set OR filepath
#' @param impression_gender string (\code{"neutral"}, \code{"female"}, or \code{"male"})
#' @param emotion_gender string (\code{"neutral"}, \code{"female"}, or \code{"male"})
#' @param eqndir string, path to directory where equations are located
#'
#' @return
#' @keywords internal
agent <- function(agenttext, name,
                  dictname = "usfullsurveyor2015", dicttype = "mean", dictgender = 'av',
                  impression_eqns = "us2010", emotion_eqns = 'us2010',
                  impression_gender = c('neutral', 'female', 'male'), emotion_gender = c('neutral', 'female', 'male'),
                  # TODO: make filepath portable
                  eqndir = "/Users/aidan/Desktop/interact_data/eq_coefs/clean"){
  # name is a string
  # dictname can be either a list of length four or a single key string that refers to one of the included sets: Indiana, Georgia-Duke, PD
  # dicttype is either a list of length four or a single key string: mean, SD, COV, sample, or mixture (latter two not yet implemented as of bayesact 2.2)
  # dictgender refers to whether to use data from female respondents, male respondents, or the average of the two. Allowable values are "female", "male", and "av". Average is default because in most cases there is little difference. It is NOT the gender of the agent.
  # impression dynamics is an equation keyword (first part only) or a file
  # emotion dynamics is an equation keyword (first part only) or a file
  # if lists, the lists can take either the dictionary key strings or filepaths (for inputting your own dictionaries)

  # calls function to return info for available dictionaries
  dicts <- get_dicts()

  alldictnames <- dict_subset(dicts)

  # directory containing dictionary files
  # TODO: make portable for package
  dictdir <- "/Users/aidan/Desktop/interact_data/"

  # TODO: check everything dictionary related

  # TODO: check error handling

  # coerce name to a string and strip all whitespace.
  # TODO: CHECK what happens when you pass a list or a factor or other things?

  name <- gsub("[[:space:]]", "", toString(name), fixed = TRUE)

  # check that dictname is as allowed (files exist or key strings are specified correctly, list is of proper length)
  tryCatch(check_input_list(dictname,
                          allowlist = alldictnames,
                          allowlength = 4,
                          allowsingle = TRUE,
                          allowfile = TRUE),
           error = function(e){
             message("Error in dictionary input: ", e)
           })

  # same with dicttype
  tryCatch(check_input_list(dicttype,
                          allowlist = c('mean', 'sd', 'cov'),
                          allowlength = 4,
                          allowsingle = TRUE,
                          allowfile = FALSE),
           error = function(e){
             message("Error in dictionary input: ", e)
           })
  # if dicttype is length 1, expand to length 4 to make line specification easier
  if(length(dicttype) == 1){
    dicttype <- rep(dicttype, 4)
  }

  # check that provided dictionaries contain necessary components
  tryCatch(check_dict_components(dictname,
                                 dicts),
           error = function(e){
             message("Error in dictionary input: ", e)
           })

  # check that dictionary has the requested gender
  tryCatch(check_dict_gender(dictname,
                             dictgender,
                             dicts),
           error = function(e){
             message("Error in dictionary input: ", e)
           })

  # check that dynamics are allowed
  # TODO: Should have a custom equation dictionary information print function for these keywords specifically (rather than the first and second level)
  tryCatch(check_input_list(impression_eqns,
                          allowlist = c("canada1985", "canada20012003", "china2000", "egypt2014", "germany2007", "japan1984", "morocco2015", "nc1978", "us2010"),
                          allowlength = 1,
                          allowsingle = TRUE,
                          allowfile = TRUE),
           error = function(e){
             message("Error in equation dynamics specification: ", e)
           })

  # check that emotions are allowed
  tryCatch(check_input_list(emotion_eqns,
                          allowlist = c("canada1985", "canada20012003", "china2000", "egypt2014", "germany2007", "japan1984", "morocco2015", "nc1978", "us2010"),
                          allowlength = 1,
                          allowsingle = TRUE,
                          allowfile = TRUE),
           error = function(e){
             message("Error in equation dynamics specification: ", e)
           })

  # check that the dictname and dicttype are compatible with one another
  tryCatch(check_dict_type_compatibility(dictname, dicttype, dicts),
           error = function(e){
             message("Error in dictionary type specification: ")
           })

  ## now get the proper strings to input for the text file
  # dictionary name


  ## WRITE INTO AGENTTEXT
  # need a name line, 4 dictionary lines, and 2 dynamics lines
  # try line by line
  nametxt <- paste0('agent: ', name)
  if(length(dictname) == 4){
    dict1 <- paste0("dictionary: AGENT : ", make_file_string(dictdir, dictname[1], dictgender, type = "identities"), " : ", toupper(dicttype[1]))
    dict2 <- paste0("dictionary: BEHAVIOR : ", make_file_string(dictdir, dictname[2], dictgender, type = "behaviors"), " : ", toupper(dicttype[2]))
    dict3 <- paste0("dictionary: CLIENT : ", make_file_string(dictdir, dictname[3], dictgender, type = "identities"), " : ", toupper(dicttype[3]))
    dict4 <- paste0("dictionary: EMOTION : ", make_file_string(dictdir, dictname[4], dictgender, type = "mods"), " : ", toupper(dicttype[4]))
  }
  else{
    # single dictionary set provided
    dict1 <- paste0("dictionary: AGENT : ", make_file_string(dictdir, dictname, dictgender, type = "identities"), " : ", toupper(dicttype[1]))
    dict2 <- paste0("dictionary: BEHAVIOR : ", make_file_string(dictdir, dictname, dictgender, type = "behaviors"), " : ", toupper(dicttype[2]))
    dict3 <- paste0("dictionary: CLIENT : ", make_file_string(dictdir, dictname, dictgender, type = "identities"), " : ", toupper(dicttype[3]))
    dict4 <- paste0("dictionary: EMOTION : ", make_file_string(dictdir, dictname, dictgender, type = "mods"), " : ", toupper(dicttype[4]))
  }
  # get full file path for equations, and check that they have the specified gender
  dyn1 <- paste0("dynamics: IMPRESSION : ", get_eqn_file(eqndir, impression_eqns, impression_gender, "impression"))
  dyn2 <- paste0("dynamics: EMOTION : ", get_eqn_file(eqndir, emotion_eqns, emotion_gender, "emotion"))

  end <- "endagent"

  # insert lines into template file and return
  return(insertLines(file = agenttext,
                     lines = c(nametxt, dict1, dict2, dict3, dict4, dyn1, dyn2, end, ""),
                     start = "AGENTDEF",
                     end = "// \\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*",
                     insertAt = "end"))

}


### Interactions
interaction <- function(){

}

### Events
events <- function(){

}

### Write out
# TODO: change path
#' Write to .txt file
#'
#' @param template object to write
#' @param filename name to write to
#' @param dir directory to write to
#'
#' @return
#' @keywords internal
out <- function(template, filename, dir = "/Users/aidan/Desktop/School/Grad_school/inteRact/bayesact/Rscripts/outtest/"){
  write.table(template, file = paste0(dir, filename), sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
}

