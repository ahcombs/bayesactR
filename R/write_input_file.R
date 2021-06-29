#' Write BayesACT input file
#'
#' Write out a text file in the format required by BayesACT containing all necessary information for all actors
#'
#' @param nodelist a dataframe giving dictionary information for each actor
#' @param edgelist a dataframe delineating starting parameters (actor identity vector, object identity vector, probabilities) for each dyad
#'
#' @export
write_input_file_from_df <- function(nodelist, edgelist){
  # setup: read in the template so we can add to it.

  # TODO: parameter generation
  # TODO: documentation--build on Jesse's vignette?

  agentlines <- c()
  # agents: for each agent, call the individual agent function
  for(i in 1:nrow(nodelist)){
    a <- get_lists(nodelist[i,])

    newlines <- agent(name = unlist(a$name),
                      dict = unlist(a$dict),
                      dict_type = unlist(a$dict_type),
                      dict_gender = unlist(a$dict_gender),
                      eqns = unlist(a$eqns),
                      eqns_gender = unlist(a$eqns_gender))
    agentlines <- append(agentlines, newlines)
  }

  curr_template <- insertLines(file = input_template,
                               lines = agentlines,
                               start = "AGENTDEF",
                               end = "// \\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*",
                               insertAt = "end")

  curr_template <- remove_line("AGENTDEF", curr_template)


  # interactions: for each interaction, call the individual interaction function

  for(i in 1:nrow(edgelist)){
    a <- get_lists(edgelist[i,])

    newlines <- interaction(agent = a$agent,
                            object = a$object,
                            agent_ident = a$agent_ident,
                            agent_ident_prob = a$agent_ident_prob,
                            object_ident = a$object_ident,
                            object_ident_prob = a$object_ident_prob
                            )
  }


  # events: number of simulations and location of events file, if applicable. Also there are some event options. Write info to template.

  out(curr_template, "test_062921.txt")
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
                  eqns = "us2010", eqns_gender = c('av', "female")){

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


  ## now get the proper strings to input for the text file

  ## WRITE INTO AGENTTEXT
  # need a name line, 4 dictionary lines, and 2 dynamics lines
  # try line by line
  nametxt <- paste0('agent: ', name)
  # get dictionary filepaths
  dict1 <- paste0("dictionary: AGENT : ", make_file_string(dict[1], dict_gender[1], component = "identities", type = dict_type[1]), " : ", toupper(dict_type[1]))
  dict2 <- paste0("dictionary: BEHAVIOR : ", make_file_string(dict[2], dict_gender[2], component = "behaviors", type = dict_type[2]), " : ", toupper(dict_type[2]))
  dict3 <- paste0("dictionary: CLIENT : ", make_file_string(dict[3], dict_gender[3], component = "identities", type = dict_type[3]), " : ", toupper(dict_type[3]))
  dict4 <- paste0("dictionary: EMOTION : ", make_file_string(dict[4], dict_gender[4], component = "mods", type = dict_type[4]), " : ", toupper(dict_type[4]))
  # get equation filepaths
  dyn1 <- paste0("dynamics: IMPRESSION : ", get_eqn_file(eqns[1], eqns_gender[1], "impressionabo"))
  dyn2 <- paste0("dynamics: EMOTION : ", get_eqn_file(eqns[2], eqns_gender[2], "emotionid"))

  end <- "endagent"
  blank <- ""

  lines <- c(nametxt, dict1, dict2, dict3, dict4, dyn1, dyn2, end, blank)

  return(lines)
}


### Interactions
interaction <- function(agent, object,
                        agent_ident = "person",
                        agent_ident_prob = "1",
                        object_ident = "person",
                        object_ident_prob = "1"){

  # check whether identities are in specified dictionaries--lower priority to implement

  # check whether probabilities sum to 1
  check_probs(agent_ident_prob, object_ident_prob)

  # check whether all correponding identity/probability lists are the same length
  check_identity_prob_match(agent_ident, agent_ident_prob)
  check_identity_prob_match(object_ident, object_ident_prob)

  # get lines for file
  line1 <- paste0("interaction: ", agent, " : ", object)
  line2 <- paste0(agent, get_actor_prob_line(agent_ident, agent_ident_prob))
  line3 <- paste0(object, get_actor_prob_line(object_ident, object_ident_prob))
  line4 <- "endinteraction"
  line5 <- ""

  # TODO: Handle NAs (for when actors don't know each other)

  lines <- c(line1, line2, line3, line4, line5)

  return(lines)
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
#' @keywords internal
out <- function(template, filename, dir = "bayesact_input_files"){
  if(!dir.exists(dir)){
    dir.create(dir)
  }

  utils::write.table(template, file = file.path(dir, filename), sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
}

