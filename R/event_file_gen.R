#' Generate an events file for a dyad
#'
#' This function constructs an events dataframe in the proper format for inputting to write_input_from_df.
#' It constructs basic dataframes only. It is limited to two actors who must act in patterned ways and only
#' perform one type of action throughout the interaction (this can be the optimal action as calculated by
#' bayesact or interact or it can be an action from the behaviors dictionary).
#'
#' It is possible to simulate interaction in larger network structures and with more complex patterns of action.
#' Dataframes created by this function may be useful as templates for creating the more complicated events
#' files needed for this.
#'
#' @param n numeric; number of turns
#' @param actors string vector of length 2; names of agents a1 and a2
#' @param a1_action string; one of "bayesact_optimal", "interact_optimal", or an action from the dictionary. Default "bayesact_optimal".
#' @param a2_action string; one of "bayesact_optimal", "interact_optimal", or an action from the dictionary. Default "bayesact_optimal".
#' @param a1_emotion string; one of "none", "bayesact_optimal", "interact_optimal", or a modifier from the modifier dictionary. Default "none".
#' @param a2_emotion string; one of "none", "bayesact_optimal", "interact_optimal", or a modifier from the modifier dictionary. Default "none".
#' @param noise string vector listing all the places where noise is desired. Options are "none" (default), "a1_action", "a1_emotion", "a2_action", "a2_emotion"
#' @param act_simultaneously boolean whether the actors should act at the same time. Default FALSE--in this case, they will instead take turns.
#' @param perspective_shift boolean; whether to switch which actor is the agent/client when the turn switches. If true, the "perspective" of bayesact is always from the agent's point of view. If false, the perspective is always from one actor's point of view.
#' @param switch_actor_after numeric; how many actions each actor should take before switching. Default 1. If act_simultaneously is TRUE, this is ignored.
#'
#' @return dataframe listing events
#' @export
basic_event_df <- function(n, actors,
                           a1_action = "bayesact_optimal", a2_action = "bayesact_optimal",
                           a1_emotion = "none", a2_emotion = "none",
                           noise = "none",
                           act_simultaneously = FALSE,
                           perspective_shift = TRUE, switch_actor_after = 1){

  # n > 0
  if(!is.numeric(n) | n <= 0 | n %% 1 != 0){
    stop("Number of turns must be a positive integer.")
  }

  # actors length 2, strings
  if(length(actors) != 2){
    stop("Two and only two actors must be provided.")
  }
  for(entry in actors){
    if(!is.character(entry)){
      stop("Actors must have names that are character strings.")
    }
  }

  # action and emotion strings are type character and length 1
  if(!is.character(a1_action) | !is.character(a2_action) | !is.character(a1_emotion) | !is.character(a2_emotion) |
     length(a1_action) > 1 | length(a2_action) > 1 | length(a1_emotion) > 1 | length(a2_emotion) > 1){
    stop("Provided actions and emotions must be strings. Options are bayesact_optimal, interact_optimal (action only), none (emotion only), or a dictionary term.")
  }

  if(a1_action == "none" | a2_action == "none"){
    stop("Actions cannot be none")
  }

  if(a1_emotion == "interact_optimal" | a2_emotion == "interact_optimal"){
    stop("Emotions cannot be interact_optimal")
  }

  # TODO later check that actions provided are in dictionary

  # noise contains only allowed entries
  for(entry in noise){
    if(!(entry %in% c("a1_action", "a2_action", "a1_emotion", "a2_emotion", "none"))){
      stop("noise must be a string or vector containing only the entries a1_action, a2_action, a1_emotion, a2_emotion, or none.")
    }
  }
  # act_simultaneously and perspective_shift are booleans
  if(!is.logical(act_simultaneously)){stop("act_simultaneously must be TRUE/FALSE")}
  if(!is.logical(perspective_shift)){stop("perspective_shift must be TRUE/FALSE")}
  # switch_actor_after > 0 and < n
  if(!is.numeric(switch_actor_after) | switch_actor_after <= 0 | switch_actor_after > n | switch_actor_after %% 1 != 0){
    stop("switch_actor_after must be a positive integer less than or equal to n")
  }

  # columns
  if(!perspective_shift){
    agent <- rep(actors[1], n)
    object <- rep(actors[2], n)
    if(act_simultaneously){
      agent_action <- rep(a1_action, n)
      object_action <- rep(a2_action, n)
      agent_emotion <- rep(a1_emotion, n)
      object_emotion <- rep(a2_emotion, n)
    } else {
      agent_action <- rep(NA, n)
      agent_emotion <- rep(NA, n)
      object_action <- rep(NA, n)
      object_emotion <- rep(NA, n)
      for(i in 1:n){
        if(ceiling(i/switch_actor_after) %% 2 == 1){
          agent_action[i] <- a1_action
          object_action[i] <- "none"
          agent_emotion[i] <- a1_emotion
          object_emotion[i] <- "none"
        } else {
          agent_action[i] <- "none"
          object_action[i] <- a2_action
          agent_emotion[i] <- "none"
          object_emotion[i] <- a2_emotion
        }
      }
    }
  }
  else {
    agent <- rep(NA, n)
    object <- rep(NA, n)
    agent_action <- rep(NA, n)
    agent_emotion <- rep(NA, n)
    object_action <- rep(NA, n)
    object_emotion <- rep(NA, n)
    for(i in 1:n){
      if(ceiling(i/switch_actor_after) %% 2 == 1){
        agent[i] <- actors[1]
        object[i] <- actors[2]
        agent_action[i] <- a1_action
        agent_emotion[i] <- a1_emotion
        if(act_simultaneously){
          object_action[i] <- a2_action
          object_emotion[i] <- a2_emotion
        } else {
          object_action[i] <- "none"
          object_emotion[i] <- "none"
        }
      } else {
        agent[i] <- actors[2]
        object[i] <- actors[1]
        agent_action[i] <- a2_action
        agent_emotion[i] <- a2_emotion
        if(act_simultaneously){
          object_action[i] <- a1_action
          object_emotion[i] <- a1_emotion
        } else {
          object_action[i] <- "none"
          object_emotion[i] <- "none"
        }
      }
    }
  }

  # make df
  df <- data.frame(agent, agent_action, agent_emotion, object, object_action, object_emotion)

  # make "none" entries blank
  df[df == "none"] <- ""

  # make "bayesact_optimal" entries asterisks
  df[df == "bayesact_optimal"] <- "*"

  # make "interact_optimal" entries exclamation points
  df[df == "interact_optimal"] <- "!"

  # add noise
  if(length(noise) != 1 | noise[1] != "none"){
    if("a1_action" %in% noise){
      df$agent_action[df$agent == actors[1]] <- ifelse(df$agent_action[df$agent == actors[1]] != "", paste0(df$agent_action[df$agent == actors[1]], "+"), "")
      df$object_action[df$object == actors[1]] <- ifelse(df$object_action[df$object == actors[1]] != "", paste0(df$object_action[df$object == actors[1]], "+"), "")
    }
    if("a2_action" %in% noise){
      df$agent_action[df$agent == actors[2]] <- ifelse(df$agent_action[df$agent == actors[2]] != "", paste0(df$agent_action[df$agent == actors[2]], "+"), "")
      df$object_action[df$object == actors[2]] <- ifelse(df$object_action[df$object == actors[2]] != "", paste0(df$object_action[df$object == actors[2]], "+"), "")
    }
    if("a1_emotion" %in% noise){
      df$agent_emotion[df$agent == actors[1]] <- ifelse(df$agent_action[df$agent == actors[1]] != "", paste0(df$agent_emotion[df$agent == actors[1]], "+"), "")
      df$object_emotion[df$object == actors[1]] <- ifelse(df$object_action[df$object == actors[1]] != "", paste0(df$object_emotion[df$object == actors[1]], "+"), "")
    }
    if("a2_emotion" %in% noise){
      df$agent_emotion[df$agent == actors[2]] <- ifelse(df$agent_action[df$agent == actors[2]] != "", paste0(df$agent_emotion[df$agent == actors[2]], "+"), "")
      df$object_emotion[df$object == actors[2]] <- ifelse(df$object_action[df$object == actors[2]] != "", paste0(df$object_emotion[df$object == actors[2]], "+"), "")
    }
  }
  return(data.frame(df))
}
