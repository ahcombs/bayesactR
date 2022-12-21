#' Write BayesACT input files
#'
#' This function takes information in three dataframes (a nodelist created by
#' [blank_nodelist()] and [add_actor()], an edgelist created by [blank_edgelist()]
#' and [add_interaction()], and an eventslist created by [basic_event_df()])
#' and writes properly formatted input files that are needed to run the BayesACT C code.
#'
#' The information in these dataframes is used to write the following files. Generally,
#' users need not open or edit these files and can simply pass the required filepaths
#' and filenames to [run_bayesact()], but the information is provided here for
#' debugging purposes. Examples of these files can be found in the BayesACT C code directory
#' under the "examples" subdirectory.
#'     - a .txt file providing information on actors and relationships between them
#'     (from the nodelist and edgelist). Users set the name of this file using the
#'     "simfilename" argument. This same file name must also be passed to [run_bayesact()]
#'     in order to run the simulation. This is saved to input_dir.
#'     - a file with the extension ".events" that contains specifications for the events
#'     that happen in the simulation (from the eventslist). This is saved to input_dir.
#'     - Four to eight files with ".dat" or ".csv" extensions that provide EPA rating
#'     information for the identities, behaviors, and modifiers that can occur in
#'     simulations. These are written to the "data" folder under the BayesACT C code
#'     top directory.
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
#'
#'
#'
#' @param nodelist a dataframe giving dictionary information for each actor
#'     such as that constructed using [blank_nodelist()] and [add_actor()].
#' @param edgelist a dataframe delineating starting parameters (actor identity
#'     vector, object identity vector, probabilities) for each dyad. Constructed
#'     using [blank_edgelist()] and [add_interaction()].
#' @param eventslist a dataframe containing an ordered list of actions to perform.
#'     Created using [basic_event_df()].
#' @param simfilename file name under which to save the sim file. Should have the
#'     extension ".txt". This same name should be passed to the simfilename
#'     argument of [run_bayesact()] in order to run the simulation.
#' @param eventfilename file name by which to save the events file. Should have the
#'     extension ".events".
#' @param bayesact_dir the path to the top level directory at which the BayesACT C
#'     code is located. Generally this is a folder called "bayesact."
#' @param input_dir the directory in which to save the sim and events files. If the
#'     directory does not already exist, it is created. This same path should be
#'     passed to the input_dir argument of [run_bayesact()] to run the simulation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_input_from_df(some_nodelist, some_edgelist, some_eventslist,
#'     simfilename = "mysimfile.txt", eventfilename = "myeventfile.events",
#'     bayesactdir = "path/to/my/C/code/bayesact",
#'     input_dir = "path/to/directory/to/save/inputfiles")
#' }
write_input_from_df <- function(nodelist, edgelist, eventslist, simfilename, eventfilename, bayesact_dir, input_dir = "bayesact_input"){
  # The sim text file contains information on agents and interactions, and a line that points to a separate events file
  # The events file (.events extension) contains the list of actions to perform, in order
  # Though there is a user query mode implemented in bayesact, it doesn't make sense to use it when your goal is to run batches
  # Therefore, this wrapper requires a .events file

  # First, add agent and interaction lines in the designated places in the sim file.
  # Then write the events file, and add the line in the sim file to point to it.

  ### AGENTS: for each agent, get lines to add to template, then add them.
  agentlines <- c()
  agentfiledf <- data.frame(identity1 = NA, behavior = NA, identity2 = NA, mod = NA)
  for(i in 1:nrow(nodelist)){
    mandatory_args <- c("name", "dict", "dict_stat", "dict_group", "eqns", "eqns_group")
    a <- get_lists(nodelist[i,])
    a_mand <- a[,mandatory_args]
    a_trim <- dplyr::select(a, function(x) (!is.na(x) & x != ""))
    a <- suppressMessages(dplyr::full_join(a_mand, a_trim))

    thenames <- names(a)
    opt_args <- thenames[(!thenames %in% mandatory_args)]

    newlines <- agent(name = unlist(a$name),
                      bayesact_dir = bayesact_dir,
                      dict = extract_dict_list(input = a$dict),
                      dict_stat = unlist(a$dict_stat),
                      dict_group = unlist(a$dict_group),
                      eqns = unlist(a$eqns),
                      eqns_group = unlist(a$eqns_group),
                      opt_args = subset(a, select = opt_args))

    agentlines <- append(agentlines, newlines)

    agentfiledf[i,] <- regmatches(newlines, regexpr("(?<= : ).*\\.csv", newlines, perl = TRUE))
  }

  # CHECK TERM MATCHING FOR IDENTITY AND BEHAVIOR USING FILES SAVED NOW
  # the terms have to match up column-wise in agentfiledf (though not for modifiers)

  for(j in 1:3){
    firstterms <- sort(utils::read.csv2(file.path(bayesact_dir, "data", agentfiledf[1,j]), sep = ",", header = FALSE)[,1])
    for(i in 2:nrow(agentfiledf)){
      theseterms <- sort(utils::read.csv2(file.path(bayesact_dir, "data", agentfiledf[i,j]), sep = ",", header = FALSE)[,1])
      if(!identical(firstterms, theseterms)){
        word <- ifelse(j == 1, "agent identity",
                       ifelse(j == 2, "behavior",
                              "client identity"))
        stop(paste0("The ", word, " dictionaries have different terms for different actors. BayesACT requires that the term sets match between actors for identity and behavior dictionaries. The recommended solution is to subset the dictionaries to the terms that are contained in both. EPA values may differ between actors."))
      }
    }
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

  # if the provided input is a relative path, make it an absolute path by prepending the current working directory
  input_dir <- absolute_path(input_dir)
  eventfilepath <- file.path(input_dir, eventfilename)
  eventlines <- event_lines(eventslist, filepath = eventfilepath)

  curr_template <- insert_lines(file = curr_template,
                                lines = eventlines,
                                start = "EVENTDEF",
                                end = "",
                                insertAt = "start")

  curr_template <- remove_line("EVENTDEF", curr_template)

  simfile_out(curr_template, simfilename, input_dir)
  eventfile_out(eventslist, eventfilename, input_dir)

  return(input_dir)
}


#' Write agent into input
#'
#' Compile all necessary information for an individual agent and add it to a template object that can be written to a text file
#'
#' @param name string agent name
#' @param dict string or length 4 vector, key(s) of an available dictionary OR filepaths OR data frames. If there are any file paths or data frames, it must be a length 4 vector.
#' @param dict_stat string or length 4 vector (\code{"mean"}, \code{"sd"}, or \code{"cov"})
#' @param dict_group string or length 4 vector (\code{"all"}, \code{"male"}, or \code{"female"})
#' @param eqns string or length 2 vector, key(s) of an available equation set OR filepath(s). First is group for impression ABO equation, second is group for emotion equation.
#' @param eqns_group string or length 2 vector (\code{"all"}, \code{"f"}, or \code{"m"}). First is group for impression ABO equation, second is group for emotion equation.
#' @param bayesact_dir top level directory at which the bayesact code lives
#' @param opt_args more agent-level parameters that may or may not be included
#'
#' @return file with inserted lines
#' @keywords internal
agent <- function(name, bayesact_dir,
                  dict = "usfullsurveyor2015", dict_stat = "mean", dict_group = 'all',
                  eqns = "us2010", eqns_group = c('av', "female"),
                  opt_args = ""){

  component_order <- c("identity", "behavior", "identity", "modifier")

  # TODO: I probably don't need all the defaults here, though they aren't hurting anything so I will ignore for now
  # TODO: Need to standardize language between agent/actor, agentlist/nodelist.

  ##### NECESSARY ARGUMENTS: name, dict, dict_stat, dict_group, eqns, eqns_group

  # Three options for dictionary format:
  # 1. A file path. Like before, don't do anything with these, just pass along to bayesact
  # 2. A dataset key from actdict. These need to be grabbed out of actdata (subsetted) and saved to the right place.
  # 3. A dataframe. Pass to the format_for_bayesact function to check whether they are formatted correctly and to reformat for saving.
  #  All else should be rejected.

  # what kind of dictionary specifications were passed?
  # this provides some checks to ensure they're valid strings or objects, so don't need to redo that
  specs <- dict_specs(dict)
  # is the given list of dictionaries the right length?
  if(length(specs) != 4){
    if((length(specs) != 1) | !all(specs == "key")){
      stop("Input list of dictionaries is of the incorrect length. Allowable length is 1 (if it is an actdata key) or 4 (if it includes any file paths or data frames)")
    }
  }

  # Now, all these check functions are really just necessary for actdata strings, not files or datasets
  # Should also work in the case of a mix of specifications... the easiest way to ensure this I think is a loop, though it's less efficient

  # calls function to return info for available dictionaries
  alldictnames <- actdata::dataset_keys()

  # coerce name to a string and strip all whitespace.
  # TODO: CHECK what happens when you pass a list or a factor or other things?
  name <- gsub("[[:space:]]", "", toString(name), fixed = TRUE)

  # check stat validity--needed for all types because we need to put it in the input file line
  check_input_list(dict_stat,
                   allowlist = c('mean', 'sd', 'cov'),
                   allowlength = 4,
                   allowsingle = TRUE)

  # check that equations are allowed
  check_input_list(eqns,
                   allowlist = actdata::equations$key,
                   allowlength = 2,
                   allowsingle = TRUE,
                   allowfile = TRUE)

  # same with equation group: standardize this input first
  eqns_group <- standardize_option(eqns_group, param = "group", version = "eqn")
  check_input_list(eqns_group,
                   allowlist = c("all", "f", "m"),
                   allowlength = 2,
                   allowsingle = TRUE,
                   allowfile = TRUE)

  # to make line spec and compatibility checking easier, expand single strings to vectors of correct length
  dict <- expand(dict, 4)
  dict_stat <- expand(dict_stat, 4)
  dict_group <- expand(dict_group, 4)
  eqns <- expand(eqns, 2)
  eqns_group <- expand(eqns_group, 2)

  ### CHECK COMPATIBILTY
  # this isn't necessary for dfs and files; just check indices that are keys

  indices <- grep("key", specs)

  # check that provided dictionaries contain necessary components
  check_dict_components(dict, indices)

  # check that dictionary has the requested group
  check_dict_group(dict, dict_group, indices)

  # check that the dict_name and dict_stat are compatible with one another
  check_dict_stat(dict, dict_stat, indices)

  # # check that equations have specified groups
  # NOW UNNECESSARY since this is done when I retrieve the file
  # check_eqn_group(eqns, eqns_group)


  ##### OPTIONAL AGENT ARGUMENTS: alphas, betas, deltas, numsamples
  # check inputs
  check_agent_opt_args(opt_args)

  # get lines
  opt_lines <- get_agent_opt_arg_lines(opt_args)

  ### CHECK THE REST OF THE INPUTS AS NEEDED. REFORMAT DATA FRAMES AND GET DICTS FROM KEYS.
  d <- list(tibble::tibble(), tibble::tibble(), tibble::tibble(), tibble::tibble())
  keys <- c("","","","")
  for(i in 1:length(specs)){
    if(specs[i] == "key"){
      # only need to check group if a key is provided
      check_input_list(dict_group,
                       allowlist = c("all", "female", "male"),
                       allowlength = 4,
                       allowsingle = TRUE,
                       checkindex = i)

      # We need to subset the actdata summary stats frame for the given statistics, then save it to the folder
      stats_to_subset <- c("mean")
      if(dict_stat[i] %in% c("sd", "cov")){
        stats_to_subset <- append(stats_to_subset, dict_stat[i])
      }

      keys[i] <- dict[[i]]
      subset <- actdata::epa_subset(dataset = dict[[i]], group = dict_group[i], component = component_order[i], stat = stats_to_subset)
      d[[i]] <- suppressMessages(actdata::format_for_bayesact(subset, stat = dict_stat[i]))

    } else if (specs[i] == "df"){
      # do the reformatting here--this also checks for valid format
      d[[i]] <- suppressMessages(actdata::format_for_bayesact(dict[[i]], stat = dict_stat[i]))
    }
    # nothing more to check for files; pass along
    else {
      d[[i]] <- dict[[i]]
    }
  }


  ##### GET LINES TO WRITE INTO AGENTTEXT
  # need a name line, 4 dictionary lines, and 2 dynamics lines
  # try line by line
  nametxt <- paste0('agent: ', name)
  # get dictionary filepaths
  dict1 <- paste0("dictionary: AGENT : ", make_file_string(dict = d[[1]], spec = specs[1], key = keys[1], group = dict_group[1], component = "identity", stat = dict_stat[1], bayesact_dir = bayesact_dir), " : ", toupper(dict_stat[1]))
  dict2 <- paste0("dictionary: BEHAVIOUR : ", make_file_string(d[[2]], spec = specs[2], key = keys[2], dict_group[2], component = "behavior", stat = dict_stat[2], bayesact_dir), " : ", toupper(dict_stat[2]))
  dict3 <- paste0("dictionary: CLIENT : ", make_file_string(d[[3]], spec = specs[3], key = keys[3], dict_group[3], component = "identity", stat = dict_stat[3], bayesact_dir), " : ", toupper(dict_stat[3]))
  dict4 <- paste0("dictionary: EMOTION : ", make_file_string(d[[4]], spec = specs[4], key = keys[4], dict_group[4], component = "modifier", stat = dict_stat[4], bayesact_dir), " : ", toupper(dict_stat[4]))
  # get equation filepaths
  dyn1 <- paste0("dynamics: IMPRESSION : ", get_eqn_file(key = eqns[1], group = eqns_group[1], component = "impressionabo", bayesact_dir))
  dyn2 <- paste0("dynamics: EMOTION : ", get_eqn_file(eqns[2], eqns_group[2], "emotionid", bayesact_dir))

  end <- "endagent"
  blank <- ""

  lines <- c(nametxt, opt_lines, dict1, dict2, dict3, dict4, dyn1, dyn2, end, blank)

  return(lines)
}


### Interactions

#' Add an interaction line to the simulation text file
#'
#' @param agent agent name
#' @param object object name
#' @param agent_ident agent identities
#' @param agent_ident_prob agent identity probabilities
#' @param object_ident object identities
#' @param object_ident_prob object identity probabilities
#' @param opt_args more arguments
#'
#' @return the lines to add
#' @keywords internal
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

#' Create events line for input file
#'
#' @param events a list of events
#' @param filepath a filepath to save to
#'
#' @return the line as a character vector
#' @keywords internal
event_lines <- function(events, filepath){
  check_events(events)
  # the iterations row is not really necessary I don't think; I believe bayesact overwrites it with the number of rows it parses from the
  # events file. But it may be convenient to have it there for informational purposes.
  lines = c(paste0("num_iterations : ", nrow(events)),
            paste0("events: ", filepath),
            "simtype : events")

  return(lines)
}

### Write out

#' Write sim information to .txt file
#'
#' @param template object to write
#' @param filename name to write to
#' @param dir directory to write to
#'
#' @keywords internal
simfile_out <- function(template, filename, dir){
  create_dir_if_needed(dir)
  utils::write.table(template, file = file.path(dir, filename), sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
}


#' Write out event file
#'
#' @param template event dataframe
#' @param filename filename to save under
#' @param dir directory name to save in
#'
#' @return filepath saved under
#' @keywords internal
eventfile_out <- function(template, filename, dir){
  if(grepl(".events$", filename) == FALSE){
    stop("Events file must end in extension .events")
  }
  # replace NA values with empty strings and tack an extra empty column on the end so there will be the right number of colons in the file
  template[,ncol(template) + 1] <- NA
  template[is.na(template)] <- ""

  create_dir_if_needed(dir)
  utils::write.table(template, file = file.path(dir, filename), sep = " : ", row.names = FALSE, col.names = FALSE, quote = FALSE)

  return(file.path(dir, filename))
}

