#' Create a blank actor nodelist data frame
#'
#' The nodelist is a data frame that contains information for the actors in
#' a simulation, including names and EPA dictionaries and equations to use.
#' This function creates a blank nodelist. Actor lines can be added with
#' [add_actor()] (recommended) or manually.
#'
#' @param use.alphas,use.betas,use.deltas logical indicating whether to include
#'     columns for alpha, beta, or delta parameters. All default to FALSE. See the
#'     BayesACT C documentation for more information on these parameters.
#' @param use.numsamples logical indicating whether to include a numsamples column.
#'     See the BayesACT C documentation for more information on this parameter.
#'
#' @return Empty nodelist data frame with correct column names
#' @export
#'
#' @examples
#' blank_nodelist()
#' blank_nodelist(use.alphas = TRUE, use.numsamples = TRUE)
#' blank_nodelist(use.alphas = TRUE, use.betas = TRUE, use.deltas = TRUE)
blank_nodelist <- function(use.alphas = FALSE, use.betas = FALSE, use.deltas = FALSE, use.numsamples = FALSE){
  cols = c("name", "dict", "dict_stat", "dict_gender", "eqns", "eqns_gender")
  if(use.alphas){ cols <- append(cols, "alphas") }
  if(use.betas){ cols <- append(cols, "betas") }
  if(use.deltas){ cols <- append(cols, "deltas") }
  if(use.numsamples){ cols <- append(cols, "numsamples") }

  df <- data.frame(matrix(nrow = 0, ncol = length(cols)))
  names(df) <- cols
  return(df)
}

#' Create a blank interaction edgelist data frame
#'
#' The edgelist is a data frame containing information on the relationships between
#' actors, including the identities they inhabit in interactions with one another and
#' the labels they ascribe to each other. This function returns a blank edgelist with
#' properly named columns. Lines can be added using [add_interaction()] (recommended)
#' or manually.
#'
#' @param use.institution logical indicating whether to include an institution column
#' @param use.rseed logical indicating whether to include an rseed column
#'
#' @return Empty edgelist data frame with correct column names
#' @export
#'
#' @examples
#' blank_edgelist()
#' blank_edgelist(use.institution = TRUE)
#' blank_edgelist(use.rseed = TRUE)
blank_edgelist <- function(use.institution = FALSE, use.rseed = FALSE){
  cols = c("agent", "object", "agent_ident", "agent_ident_prob", "object_ident", "object_ident_prob")
  if(use.institution){ cols <- append(cols, "institution") }
  if(use.rseed){ cols <- append(cols, "rseed") }

  df <- data.frame(matrix(nrow = 0, ncol = length(cols)))
  names(df) <- cols
  return(df)
}

#' Add an actor line to a actor nodelist data frame
#'
#' The nodelist is a data frame that contains information for the actors in
#' a simulation, including names and EPA dictionaries and equations. Actors
#' also have optional parameters that control how they manage uncertainty in
#' interactions.
#'
#'
#' # Dictionaries and equations
#'
#' Each actor needs four dictionaries representing:
#' 1. the EPA values for identities they assign to themselves
#' 2. the EPA values they assign to behaviors that are valid within the interaction
#' 3. the EPA values for identities they assign to their interaction partners
#' 4. the EPA values they assign to modifiers and emotions.
#'
#' They also need two sets of equation coefficients:
#' 1. Impression equation coefficients which determine ideal elements of A-B-O
#'     (actor-behavior-object) events. In actdata, these are referred to with the
#'     "impressionabo" keyword.
#' 2. Emotion equation coefficients which estimate emotional reactions to events.
#'    In actdata, these are referred to with the "emotionid" keyword.
#'
#' These dictionaries and equations can be provided in one of three ways (mixing and matching is allowed):
#' * If you are working with publicly available ACT sentiment dictionaries and equation
#'     sets, dictionaries may be specified using keywords from the actdata package. This
#'     package is a repository for standardized versions of many publicly available ACT
#'     sentiment dictionaries and equation sets, and it and bayesactR were developed to
#'     complement each other. If using dictionaries and/or equations from actdata, provide
#'     the applicable keyword as the dict or eqns argument. To see information about available
#'     data sets and gender subsets, see the package readme or call [actdata::dict_info()] or
#'     [actdata::eqn_info()].
#' * Dictionaries can also be provided as data frame objects. This is particularly useful
#'     when you wish to use a subset of terms from a public dictionary–for example, perhaps
#'     you only want your agents to be able to take a limited set of behaviors, or identities
#'     from just one institution, rather than having access to the whole list. The
#'     [actdata::epa_subset()] function within actdata makes creating subsets from public
#'     data straightforward.
#' * If you have collected or estimated your own data or otherwise have access to non-public
#'     data sets, you may provide a filepath to the dictionary and equation files in the
#'     dict and eqns arguments. These must be properly formatted for BayesACT. When provided with
#'     filepaths, bayesactR passes them directly to BayesACT with no formatting checks or changes.
#'     See also [actdata::format_for_bayesact()], which can help reformat datasets correctly.
#'
#' # Parameters alpha, beta, and delta
#'
#' There are three parameters that control how actors tend to respond to interactions. In
#' each case, these parameters can be set separately for the relevant elements of the situation
#' (actor, behavior, and client/object), or can be provided as a single number which is applied
#' across elements.
#'
#' For more information on these parameters, see the *BayesACT Version 2: Technical Users Manual*,
#' available in the BayesACT C directory under the subdirectory *bayesact/docs/*. The following is
#' a summary of description found there.
#'
#' * Alpha (a set of three values alpha_a, alpha_b, alpha_c) represents the strength of the affect
#'    control principle (higher value is weaker; default is 0.1).
#' * Beta (a set of two values beta_a and beta_c) represents identity sentiment inertia, or how
#'    stable we expect EPA values for identities to remain over time. Default is 0.01.
#' * Delta (a set of two values delta_a and delta_c) represents denotative identity stability, or
#'    how likely it is that someone is relabeled. Default is 0.1.
#'
#' If the actor is given optional arguments (alphas, betas, deltas, numsamples) that are not
#' already columns in the data frame, these columns will be added.
#'
#' @param nodelist a data frame to add the actor line to
#' @param name a string to use as the actor's name
#' @param dicts one of several options (see Details):
#'     1a. A dictionary key from actdata. Data is pulled from that package and split into
#'     the four required components based on the component column.
#'     1b. A list of four dictionary keys from actdata to be used for, in order, own identities,
#'     behaviors, partner identities, and emotions. The same key may be used for more than one
#'     category.
#'     2a. An EPA data frame with a "component" column that is used to split it into
#'     a list of length 4 following the above order.
#'     2b. A list of four EPA data frames in order: own identities, behaviors, partner
#'     identities, emotions. Use "list()", rather than "c()", to construct this list.
#'     3a. A list of four file paths to csv files containing the required dictionaries.
#' @param dict_stat string or string list length 4: stat of provided dictionaries (mean,
#'     cov, sd). For data sets in actdata, check available stats with [actdata::dict_info()].
#' @param dict_gender string or string list length 4: gender of provided dictionaries (av,
#'     female, male). For data sets in actdata, check available genders with [actdata::dict_info()].
#' @param dict_file_prefix prefix to append to dictionary data files that
#'     are written out. Default is "dict."
#' @param eqns string or string list length 2 providing equations to use. See Details. Entries must either by
#'     equation keys from the actdata package or valid filepaths to coefficient matrices. If
#'     provided as a length 2 list, the first entry is used as the impression ABO coefficients and
#'     the second as the emotion coefficients. If a single entry (only possible for coefficients in
#'     actdata), the function attempts to find these two components itself.
#' @param eqns_gender string or string list length 2: gender of equations to use (av, female,
#'     male). For datasets in actdata, check available genders with [actdata::eqn_info()]. An error
#'     is thrown if the specified equation set is not available for the specified gender.
#' @param alphas numeric or numeric list length 3: alpha value(s) to use. See Details.
#' @param betas numeric or numeric list length 2: beta value(s) to use. See Details.
#' @param deltas numeric or numeric list length 2: delta value(s) to use. See Details.
#' @param numsamples numeric: number of samples to be used by the actor for each interaction.
#'     Default is 1000.
#'
#' @return provided nodelist with actor line appended
#' @export
#'
#' @examples
#' nodelist <- blank_nodelist()
#' nodelist <- add_actor(nodelist, name = "Sally", dict_stat = "cov")
#' nodelist <- add_actor(nodelist, name = "Reem", dicts = "egypt2015", dict_stat = "cov",
#'     eqns = "egypt2014")
#' nodelist <- add_actor(nodelist, name = "Jamal",
#'     dicts = c("indiana2003", "usmturk2015", "indiana2003", "indiana2003"), alphas = .5)
add_actor <- function(nodelist, name,
                      dicts = "usfullsurveyor2015", dict_stat = "mean", dict_gender = "av", dict_file_prefix = "dict",
                      eqns = "us2010", eqns_gender = c("av", "female"),
                      alphas = NA, betas = NA, deltas = NA, numsamples = NA){

  # need to do some manipulating to get the dict column in the right format--nested tibble
  if(is.character(dicts)){
    check_input_list(dicts, allowlist = actdata::dataset_keys(), allowlength = 4, allowsingle = TRUE, allowfile = TRUE)
    if(length(dicts) == 1){
      dicts <- expand(dicts, 4)
    }
    d_entry = list(tibble::tibble(dictstring = dicts))
  } else if (is.data.frame(dicts) | tibble::is_tibble(dicts)){
    d_entry = list(component_split(dicts))
  } else if (is.list(dicts)){
    # not going to type check these here; save for the write input file function with all the other checks
    if(length(dicts) == 4){
      d_entry = list(dicts)
    } else {
      stop("List of dictionary data frames should be of length 4 or of length 1 with a component column demarcating identities, behaviors, and components")
    }
  }
  line <- tibble::tibble(name = name,
                         dict = d_entry,
                         dict_stat = standardize_option(dict_stat, "stat", version = "dict"),
                         dict_gender = standardize_option(dict_gender, "gender", version = "dict"),
                         eqns = toString(eqns),
                         eqns_gender = toString(standardize_option(eqns_gender, "gender", version = "eqn")))
  if(!all(is.na(alphas))){ line$alphas <- toString(alphas) }
  if(!all(is.na(betas))){ line$betas <- toString(betas) }
  if(!all(is.na(deltas))){ line$deltas <- toString(deltas) }
  if(!all(is.na(numsamples))){ line$numsamples <- toString(numsamples) }

   joined <- plyr::rbind.fill(nodelist, line)
   joined <- dplyr::select(joined, intersect(c("name", "dict", "dict_stat", "dict_gender", "eqns", "eqns_gender", "alphas", "betas", "deltas", "numsamples"), names(joined)))
   return(joined)
}

#' Add an interaction line to the edgelist
#'
#' The edgelist is a data frame containing information on the relationships between
#' actors, including the identities they inhabit in interactions with one another and
#' the labels they ascribe to each other. This function adds a line, representing a directed
#' relationship between one actor and another, to an edgelist data frame created by
#' [blank_edgelist()].
#'
#' An edgelist line can be considered to define a relationship between two actors, an agent
#' and an object, in a simulation *from the perspective of the agent*. The agent has a label
#' (or multiple possible labels) for their own identity, and possible label(s) for
#' the object's identity. If a list of multiple identities are provided for either agent
#' or object, then the agent will randomly select one of the provided identities in each
#' simulation repetition. In this case, the selection probabilities are given by
#' agent_ident_prob and object_ident_prob.
#'
#' Because each edgelist line represents the situation from the perspective of just one
#' actor (the agent), each dyadic relationship in a simulation requires two edgelist lines:
#' one from the perspective of each actor. Actors might agree on their identities--for example, both
#' may agree that person 1 will act as a "sister" and person 2 will act as a "brother". In
#' this case the two edgelist lines will be mirror images of each other. Actors may also
#' begin interactions in disagreement about their identities--person 1 may see themselves as
#' a "sister" and see person 2 as a "brother," but person 2 may instead see person 1 as a "bully"
#' and themselves as a "victim."
#'
#' It is possible that person 1 knows person 2 but person 2 does not know person 1. In this case,
#' two edgelist lines are still required. However, the object_ident and object_ident_prob for
#' person 2 (representing their initial label for person 1) will be the empty string "".
#'
#' These identities are only starting points for a simulation. Identities may shift through the
#' course of an interaction and generally may take any value represented in the identity dictionaries
#' provided in the nodelist. If you wish to restrict the possible values, you may remove them
#' from the dictionary provided there, or use the optional "institution" argument in this function
#' to restrict to identities that only apply in particular social contexts.
#'
#' If an interaction is given optional arguments (institution, rseed) that are not already
#' columns in the provided dataframe, those columns are added.
#'
#' @param edgelist interaction edgelist to add to
#' @param agent string agent's name
#' @param object string object's name
#' @param agent_ident string or string list: agent's label(s) for their own identity at the
#'     outset of an interaction. If multiple are provided, the agent chooses one of them, and
#'     the probability of each being chosen is taken from the agent_ident_prob argument.
#'     Default is c("friend", "person").
#' @param agent_ident_prob numeric or numeric list summing to 1: probability of taking
#'     each identity in agent_ident. Default is c(.5, .5).
#' @param object_ident string or string list: agent's label(s) for object's identity at the
#'     outset of an interaction. If multiple are provided, the agent chooses one of them, and
#'     the probability of each being chosen is taken from the object_ident_prob argument.
#'     Default is c("friend", "person").
#' @param object_ident_prob numeric or numeric list summing to 1: probability of taking
#'     each identity in object_ident. Default is c(.5, .5).
#' @param institution string or string list: the institution(s) identities in this relationship
#'     can come from. Valid institutions are lay, business, law, politics, academe, medicine,
#'     religion, family, sexual, monadic, group, corporal, male, and female (see actdata for details).
#'     Default is NA, which allows all institutions.
#' @param rseed an optional seed value to ensure identical simulation results with repeated runs.
#'
#' @return edgelist data frame with added interaction line
#' @export
#'
#' @examples
#' edgelist <- blank_edgelist()
#' edgelist <- add_interaction(edgelist, agent = "Sally", object = "Reem",
#'     agent_ident = "teacher", agent_ident_prob = 1,
#'     object_ident = c("student", "troublemaker"), object_ident_prob = c(.8, .2))
#' edgelist <- add_interaction(edgelist, agent = "Reem", object = "Sally",
#'     agent_ident = c("student", "teenager"), agent_ident_prob = c(.3, .7),
#'     object_ident = "bore", object_ident_prob = 1)
#' edgelist <- add_interaction(edgelist, agent = "Jamal", object = "Sam",
#'     agent_ident = "boss", agent_ident_prob = 1,
#'     object_ident = c("employee", "friend", "do_nothing"), object_ident_prob = c(.5, .25, .25),
#'     institution = "business")
#' edgelist <- add_interaction(edgelist, agent = "Sam", object = "Jamal",
#'     agent_ident = "employee", agent_ident_prob = 1,
#'     object_ident = "", object_ident_prob = "",
#'     institution = "business")
add_interaction <- function(edgelist, agent, object,
                            agent_ident = c("friend", "person"), agent_ident_prob = c(.5, .5),
                            object_ident = c("friend", "person"), object_ident_prob = c(.5, .5),
                            institution = NA, rseed = NA){
  # TODO add check for edgelist format (also nodelist in previous function)
  if(!is.na(institution)){
    # TODO can bayesact accept monadic, group, corporal, male, and female, or just the base 9?
    if(!(institution %in%
         c("lay", "business", "law", "politics", "academe", "medicine", "religion",
           "family", "sexual", "monadic", "group", "corporal", "male", "female"))){
      stop(message(paste0(institution, " is not a valid institution.")))
    }
  }

  line <- data.frame(agent = agent, object = object,
                     agent_ident = tolower(toString(agent_ident)), agent_ident_prob = toString(agent_ident_prob),
                     object_ident = tolower(toString(object_ident)), object_ident_prob = toString(object_ident_prob))
  if(!all(is.na(institution))){ line$institution = toString(institution) }
  if(!all(is.na(rseed))){ line$institution = toString(rseed) }

  joined <- plyr::rbind.fill(edgelist, line)
  joined <- dplyr::select(joined, intersect(c("agent", "object", "agent_ident", "agent_ident_prob", "object_ident", "object_ident_prob", "institution", "rseed"), names(joined)))
  return(joined)
}

#' Split a given data frame into identity, behavior, and modifier components, if possible. Return error if not.
#'
#' @param df given object
#'
#' @return list of length 4--identity, behavior, identity, modifier
#' @keywords internal
component_split <- function(df){
  if(!is.data.frame(df) & !tibble::is_tibble(df)){
    stop("Given dictionary object must be a data frame or a tibble")
  }

  if(!("component" %in% names(df))){
    stop("If a single dictionary data frame or tibble object is provided, the component column must have identity, behavior, and modifier entries.")
  } else if (!all((c("identity", "behavior", "modifier") %in% df$component))){
    stop("If a single dictionary data frame or tibble object is provided, the component column must have identity, behavior, and modifier entries.")
  }

  ident <- df[df$component == "identity",]
  beh <- df[df$component == "behavior",]
  mod <- df[df$component == "modifier",]

  return(list(ident, beh, ident, mod))
}
