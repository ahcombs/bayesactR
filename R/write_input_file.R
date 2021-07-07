#' Write BayesACT input file
#'
#' From information in three dataframes (agents, interactions, events),
#' write out a .txt sim file and a .events file in the format required by BayesACT.
#' These contain information on actors, interactions/dyads, and events/actions.
#'
#' The data format expected for agents and interactions is similar to that used in social network analysis.
#' Information on individual actors (including name, dictionaries, equations, etc) is stored in a node list (one line per actor).
#' Information specific to ties between actors (called interactions here), such as identity distributions, is stored in an edgelist-like format (one line per directed tie).
#'
#' The function also requires an events file, which is simply a dataframe containing information on the actions to be simulated (one line per action)
#' In future I will implement functionality to help build these dataframes.
#'
#' @param nodelist a dataframe giving dictionary information for each actor
#' @param edgelist a dataframe delineating starting parameters (actor identity vector, object identity vector, probabilities) for each dyad
#' @param eventslist a dataframe containing an ordered list of actions to perform
#' @param simfilename file name by which to save the sim file
#' @param eventfilename file name by which to save the events file
#' @param dir the directory at which to save the sim file and events file
#'
#' @export
write_input_file_from_df <- function(nodelist, edgelist, eventslist, simfilename, eventfilename, dir = "bayesact_input_files"){
  # The sim text file contains information on agents and interactions, and a line that points to a separate events file
  # The events file (.events extension) contains the list of actions to perform, in order
  # Though there is a user query mode implemented in bayesact, it doesn't make sense to use it when your goal is to run batches
  # Therefore, this wrapper requires a .events file

  # First, add agent and interaction lines in the designated places in the sim file.
  # Then write the events file, and add the line in the sim file to point to it.





  # for making things easier with current version of bayesact... remove eventually when more portable code is possible
  dir <- "/Users/aidan/Desktop/School/Grad_school/bayesactgithub/bayesact/examples"




  ### AGENTS: for each agent, get lines to add to template, then add them.
  agentlines <- c()
  for(i in 1:nrow(nodelist)){
    mandatory_args <- c("name", "dict", "dict_type", "dict_gender", "eqns", "eqns_gender")
    a <- get_lists(nodelist[i,])
    a_mand <- a[,mandatory_args]
    a_trim <- dplyr::select(a, function(x) (!is.na(x) & x != ""))
    a <- suppressMessages(dplyr::full_join(a_mand, a_trim))

    thenames <- names(a)
    opt_args <- thenames[(!thenames %in% mandatory_args)]

    newlines <- agent(name = unlist(a$name),
                      dict = unlist(a$dict),
                      dict_type = unlist(a$dict_type),
                      dict_gender = unlist(a$dict_gender),
                      eqns = unlist(a$eqns),
                      eqns_gender = unlist(a$eqns_gender),
                      opt_args = subset(a, select = opt_args))
    agentlines <- append(agentlines, newlines)
  }

  curr_template <- insert_lines(file = input_template,
                                lines = agentlines,
                                start = "AGENTDEF",
                                end = "// \\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*",
                                insertAt = "end")

  curr_template <- remove_line("AGENTDEF", curr_template)


  ### INTERACTIONS: for each interaction, get lines to add and add them

  interactionlines <- c()

  for(i in 1:nrow(edgelist)){
    mandatory_args <- c("agent", "object", "agent_ident", "object_ident", "agent_ident_prob", "object_ident_prob")
    a <- get_lists(edgelist[i,])
    a_mand <- a[,mandatory_args]
    a_trim <- dplyr::select(a, function(x) (!is.na(x) & x != ""))
    a <- suppressMessages(dplyr::full_join(a_mand, a_trim))
    thenames <- names(a)

    opt_args <- thenames[(!thenames %in% mandatory_args)]

    newlines <- interaction(agent = unlist(a$agent),
                            object = unlist(a$object),
                            agent_ident = unlist(a$agent_ident),
                            agent_ident_prob = unlist(a$agent_ident_prob),
                            object_ident = unlist(a$object_ident),
                            object_ident_prob = unlist(a$object_ident_prob),
                            opt_args = subset(a, select = opt_args))

    interactionlines <- append(interactionlines, newlines)
  }

  curr_template <- insert_lines(file = curr_template,
                                lines = interactionlines,
                                start = "INTERACTIONDEF",
                                end = "// \\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*",
                                insertAt = "start"
                                )

  curr_template <- remove_line("INTERACTIONDEF", curr_template)


  ### EVENTS: save event dataframe in correct format and add the filepath to the sim file

  eventfilepath <- file.path(dir, eventfilename)
  simfilepath <- file.path(dir, simfilename)

  eventlines <- event_lines(eventslist, filepath = eventfilepath)

  curr_template <- insert_lines(file = curr_template,
                                lines = eventlines,
                                start = "EVENTDEF",
                                end = "",
                                insertAt = "start")

  curr_template <- remove_line("EVENTDEF", curr_template)

  simfile_out(curr_template, simfilename, dir)
  eventfile_out(eventslist, eventfilename, dir)
}


#' Write agent into input
#'
#' Compile all necessary information for an individual agent and add it to a template object that can be written to a text file
#'
#' @param name string agent name
#' @param dict string or length 4 vector, key(s) of an available dictionary OR filepath(s)
#' @param dict_type string or length 4 vector (\code{"mean"}, \code{"sd"}, or \code{"cov"})
#' @param dict_gender string or length 4 vector (\code{"av"}, \code{"male"}, or \code{"female"})
#' @param eqns string or length 2 vector, key(s) of an available equation set OR filepath(s)
#' @param eqns_gender string or length 2 vector (\code{"av"}, \code{"f"}, or \code{"m"})
#'
#' @return file with inserted lines
#' @keywords internal
agent <- function(name,
                  dict = "usfullsurveyor2015", dict_type = "mean", dict_gender = 'av',
                  eqns = "us2010", eqns_gender = c('av', "female"),
                  opt_args = ""){

  ##### NECESSARY ARGUMENTS: name, dict, dict_type, dict_gender, eqns, eqns_gender

  # calls function to return info for available dictionaries
  dicts <- actdata::get_dicts()
  alldictnames <- actdata::dict_subset(dicts)

  # coerce name to a string and strip all whitespace.
  # TODO: CHECK what happens when you pass a list or a factor or other things?
  name <- gsub("[[:space:]]", "", toString(name), fixed = TRUE)

  ### CHECK INPUTS
  # check that dict key is as allowed (files exist or key strings are specified correctly, list is of proper length)
  check_input_list(dict,
                          allowlist = alldictnames,
                          allowlength = 4,
                          allowsingle = TRUE,
                          allowfile = TRUE)

  # same with dict_type
  check_input_list(dict_type,
                          allowlist = c('mean', 'sd', 'cov'),
                          allowlength = 4,
                          allowsingle = TRUE,
                          allowfile = FALSE)

  # same with dict_gender
  check_input_list(dict_gender,
                          allowlist = c("av", "female", "male"),
                          allowlength = 4,
                          allowsingle = TRUE,
                          allowfile = TRUE)

  # check that equations are allowed
  check_input_list(eqns,
                          allowlist = actdata::eqn_subset(actdata::get_eqns()),
                          allowlength = 2,
                          allowsingle = TRUE,
                          allowfile = TRUE)

  # same with equation gender
  check_input_list(eqns_gender,
                          allowlist = c("av", "female", "male"),
                          allowlength = 2,
                          allowsingle = TRUE,
                          allowfile = TRUE)

  # to make line spec and compatibility checking easier, expand single strings to vectors of correct length
  dict <- expand(dict, 4)
  dict_type <- expand(dict_type, 4)
  dict_gender <- expand(dict_gender, 4)
  eqns <- expand(eqns, 2)
  eqns_gender <- expand(eqns_gender, 2)

  ### CHECK COMPATIBILTY

  # check that provided dictionaries contain necessary components
  check_dict_components(dict)

  # check that dictionary has the requested gender
  check_dict_gender(dict, dict_gender)

  # check that the dict_name and dict_type are compatible with one another
  check_dict_type(dict, dict_type)

  # check that equations have specified genders
  check_eqn_gender(eqns, eqns_gender)


  ##### OPTIONAL AGENT ARGUMENTS: alphas, betas, deltas, numsamples
  # check inputs
  check_agent_opt_args(opt_args)

  # get lines
  opt_lines <- get_agent_opt_arg_lines(opt_args)

  ##### GET LINES TO WRITE INTO AGENTTEXT
  # need a name line, 4 dictionary lines, and 2 dynamics lines
  # try line by line
  nametxt <- paste0('agent: ', name)
  # get dictionary filepaths
  dict1 <- paste0("dictionary: AGENT : ", make_file_string(dict[1], dict_gender[1], component = "identities", type = dict_type[1]), " : ", toupper(dict_type[1]))
  dict2 <- paste0("dictionary: BEHAVIOUR : ", make_file_string(dict[2], dict_gender[2], component = "behaviors", type = dict_type[2]), " : ", toupper(dict_type[2]))
  dict3 <- paste0("dictionary: CLIENT : ", make_file_string(dict[3], dict_gender[3], component = "identities", type = dict_type[3]), " : ", toupper(dict_type[3]))
  dict4 <- paste0("dictionary: EMOTION : ", make_file_string(dict[4], dict_gender[4], component = "mods", type = dict_type[4]), " : ", toupper(dict_type[4]))
  # get equation filepaths
  dyn1 <- paste0("dynamics: IMPRESSION : ", get_eqn_file(eqns[1], eqns_gender[1], "impressionabo"))
  dyn2 <- paste0("dynamics: EMOTION : ", get_eqn_file(eqns[2], eqns_gender[2], "emotionid"))

  end <- "endagent"
  blank <- ""

  lines <- c(nametxt, opt_lines, dict1, dict2, dict3, dict4, dyn1, dyn2, end, blank)

  return(lines)
}


### Interactions
interaction <- function(agent, object,
                        agent_ident = "person",
                        agent_ident_prob = "1",
                        object_ident = "person",
                        object_ident_prob = "1",
                        opt_args = ""){

  # TODO: check whether identities are in specified dictionaries--lower priority to implement

  # check whether probabilities sum to 1
  check_probs(agent_ident_prob, object_ident_prob)

  # check whether all correponding identity/probability lists are the same length
  check_identity_prob_match(agent_ident, agent_ident_prob)
  check_identity_prob_match(object_ident, object_ident_prob)

  # check optional arguments (currently institution and random seed)
  check_interaction_opt_args(opt_args)

  # get lines for file
  line1 <- paste0("interaction: ", agent, ": ", object)
  line2 <- paste0(agent, get_actor_prob_line(agent_ident, agent_ident_prob))
  line3 <- paste0(object, get_actor_prob_line(object_ident, object_ident_prob))
  line4 <- "endinteraction"
  line5 <- ""

  opt_lines <- get_interaction_opt_arg_lines(opt_args)

  # TODO: Handle NAs (for when actors don't know each other)

  lines <- c(line1, opt_lines, line2, line3, line4, line5)

  return(lines)
}

### Events
# if the goal of this is batches, then it doesn't make sense to make user-query mode available.
# So the line should be "simtype : events" with an events file.
# num_iterations can be left alone; I am pretty sure it gets overridden by the number of lines in the events file
# see the notes I pasted on github for things to check for in the events file
event_lines <- function(events, filepath){
  check_events(events)
  # the iterations row is not really necessary I don't think; I believe bayesact overwrites it with the number of rows it parses from the
  # events file. But it may be convenient to have it there for informational purposes.
  lines = c(paste0("num_iterations : ", nrow(events)),
            paste0("events: ", filepath),
            "simtype : events")
}

### Write out
# TODO: change path
#' Write to .txt file
#'
#' @param template object to write
#' @param filename name to write to
#' @param dir directory to write to
#'
#' @keywords internal
simfile_out <- function(template, filename, dir){
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  utils::write.table(template, file = file.path(dir, filename), sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
}


#' Write out event file
#'
#' @param template event dataframe
#' @param filename filename to save under
#' @param dir directory name to save in
#'
#' @return filepath saved under
eventfile_out <- function(template, filename, dir){
  if(grepl(".events$", filename) == FALSE){
    stop("Events file must end in extension .events")
  }
  # replace NA values with empty strings and tack an extra empty column on the end so there will be the right number of colons in the file
  template[,ncol(template) + 1] <- NA
  template[is.na(template)] <- ""

  if(!dir.exists(dir)){
    dir.create(dir)
  }
  utils::write.table(template, file = file.path(dir, filename), sep = " : ", row.names = FALSE, col.names = FALSE, quote = FALSE)

  return(file.path(dir, filename))
}

