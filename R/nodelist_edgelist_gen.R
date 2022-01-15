#' Create a blank actor nodelist in dataframe format
#'
#' This can be added to manually or with the help of add_actor()
#'
#' @param use.alphas boolean whether to include alphas column
#' @param use.betas boolean whether to include betas column
#' @param use.deltas boolean whether to include deltas column
#' @param use.numsamples boolean whether to include numsamples column
#'
#' @return empty nodelist data frame with correct column names
#' @export
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

#' Create a blank interaction edgelist
#'
#' @param use.institution boolean whether to include an institution column
#' @param use.rseed boolean whether to include an rseed column
#'
#' @return empty edgelist data frame with correct column names
#' @export
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
#' If the actor is given optional arguments (alphas, betas, deltas, numsamples) that are not already columns in the data frame, these columns will be added.
#'
#' @param nodelist data frame to add to
#' @param name string actor's name
#' @param dict string or string list length 4: dictionary to use. Entries must either be dictionary keys from the actdata package or valid filepaths to dictionaries.
#' @param dict_stat string or string list length 4: stat of provided dictionaries (mean, cov, sd). For datasets in actdata, check available stats with actdata::dict_info().
#' @param dict_gender string or string list length 4: gender of provided dictionaries (av, female, male). For datasets in actdata, check available genders with actdata::dict_info().
#' @param dict_file_prefix string prefix to append to dictionary data files that are saved out
#' @param eqns string or string list length 2: equations to use. Entries must either by equation keys from the actdata package or valid filepaths to coefficient matrices.
#' @param eqns_gender string or string list length 2: gender of equations to use (av, female, male). For datasets in actdata, check available genders with actdata::eqn_info().
#' @param alphas numeric or numeric list length 3: alpha value(s) to use.
#' @param betas numeric or numeric list length 2: beta value(s) to use.
#' @param deltas numeric or numeric list length 2: delta value(s) to use.
#' @param numsamples numeric: number of samples to draw
#'
#' @return provided nodelist with actor line appended
#' @export
add_actor <- function(nodelist, name,
                      dict = "usfullsurveyor2015", dict_stat = "mean", dict_gender = "av", dict_file_prefix = "dict",
                      eqns = "us2010", eqns_gender = c("av", "female"),
                      alphas = NA, betas = NA, deltas = NA, numsamples = NA){

  # need to do some manipulating to get the dict column in the right format--nested tibble
  if(is.character(dict)){
    check_input_list(dict, allowlist = actdata::dataset_keys(), allowlength = 4, allowsingle = TRUE, allowfile = TRUE)
    if(length(dict) == 1){
      dict <- expand(dict, 4)
    }
    d_entry = list(tibble::tibble(dictstring = dict))
  } else if (is.data.frame(dict) | tibble::is_tibble(dict)){
    d_entry = list(component_split(dict))
  } else if (is.list(dict)){
    # not going to type check these here; save for the write input file function with all the other checks
    if(length(dict) == 4){
      d_entry = list(dict)
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

#' Add an interaction line to the interaction edgelist
#'
#' If an interaction is given optional arguments (institution, rseed) that are not already columns in the provided dataframe, those columns are added.
#'
#' @param edgelist interaction edgelist to add to
#' @param agent string agent's name
#' @param object string object's name
#' @param agent_ident string or string list: agent's understanding of their own identity
#' @param agent_ident_prob numeric or numeric list summing to 1: probability of taking each identity in agent_ident
#' @param object_ident string or string list: agent's understanding of object's identity
#' @param object_ident_prob numeric or numeric list summing to 1: probability of taking each identity in object_ident
#' @param institution string or string list: the institution(s) identities in this relationship can come from
#' @param rseed the random seed
#'
#' @return edgelist data frame with added interaction line
#' @export
add_interaction <- function(edgelist, agent, object,
                            agent_ident = c("friend", "person"), agent_ident_prob = c(.7, .3),
                            object_ident = c("friend", "person"), object_ident_prob = c(.7, .3),
                            institution = NA, rseed = NA){
  # TODO add check for edgelist format (also nodelist in previous function)
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
