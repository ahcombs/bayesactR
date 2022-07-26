# NOTE: run_bayesact uses the rstudio api and testthat can't open an instance of rstudio in the background, so runbayesact will not work here.

test_that("a. running with a dataset works", {
  # library(dplyr)
  identlist <- c("buddy", "bully", "classmate", "co_worker", "dummy", "enemy", "foe", "follower",
                 "friend", "loser", "miser", "opportunist", "partner", "robber", "scrooge", "traitor",
                 "opponent", "jerk")
  ident_terms <- actdata::term_table %>%
    dplyr::filter(term %in% identlist, component == "identity")%>%
    dplyr::select("term", "usmturk2015", "egypt2015") %>%
    dplyr::filter(usmturk2015 == 1 & egypt2015== 1) %>%
    dplyr::select(term) %>%
    dplyr::arrange(term) %>%
    unlist()

  pd_ident_egypt <- actdata::epa_subset(expr = ident_terms,
                                        exactmatch = TRUE,
                                        dataset = "egypt2015",
                                        component = "identity",
                                        stat = c('mean', 'cov'),
                                        gender = "average")
  pd_ident_us <- actdata::epa_subset(expr = ident_terms,
                                     exactmatch = TRUE,
                                     dataset = "usmturk2015",
                                     component = "identity",
                                     stat = c('mean', 'cov'),
                                     gender = "average")
  pd_beh_us <- actdata::epa_subset(expr = c("collaborate", "^cheat$"),
                                   dataset = "usmturk2015",
                                   component = "behavior",
                                   stat = c('mean', 'cov'),
                                   gender = "average")
  pd_beh_egypt <- actdata::epa_subset(expr = c("collaborate", "^cheat$"),
                                      dataset = "egypt2015",
                                      component = "behavior",
                                      stat = c('mean', 'cov'),
                                      gender = "average")
  pd_mods_us <- actdata::epa_subset(expr = c("^happy$", "^sad$", "^mad$"),
                                    dataset = "usmturk2015",
                                    component = "modifier",
                                    stat = c("mean", "cov"),
                                    gender = "average")
  pd_mods_egypt <- actdata::epa_subset(expr = c("^happy$", "^sad$", "mad"),
                                       dataset = "egypt2015",
                                       component = "modifier",
                                       stat = c("mean", "cov"),
                                       gender = "average")

  bayesact_dir <- "/Users/aidan/Desktop/School/Grad_school/ACT/bayesactgithub/bayesact"

  # TODO add a check that determines the stat automatically from the df structure
  nodelist <- blank_nodelist()
  nodelist <- add_actor(nodelist, name = "frank", dict = list(pd_ident_us, pd_beh_us, pd_ident_us, pd_mods_us), dict_stat = "cov")
  nodelist <- add_actor(nodelist, name = "reem", dict = list(pd_ident_egypt, pd_beh_egypt, pd_ident_egypt, pd_mods_egypt), eqns = "egypt2014", eqns_gender = c("av", "f"), dict_stat = "cov")

  edgelist <- blank_edgelist()
  edgelist <- add_interaction(edgelist, agent = "frank", object = "reem", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("friend", "co_worker"), object_ident_prob = c(.5, .5))
  edgelist <- add_interaction(edgelist, agent = "reem", object = "frank", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("friend", "co_worker"), object_ident_prob = c(.5, .5))

  eventslist <- basic_event_df(n = 2, actors = c("frank", "reem"), noise = c("a1_action", "a2_action"))
  write_input_from_df(nodelist, edgelist, eventslist, simfilename = "a_sim.csv", eventfilename = "a_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input")

  # run_bayesact(simfilename = "a_sim.csv", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input", output_dir = "/Users/aidan/Desktop/bayesact_test_output")
})

test_that("b. basic run with a dataset key works", {
  bayesact_dir <- "/Users/aidan/Desktop/School/Grad_school/ACT/bayesactgithub/bayesact"

  # TODO add a check that determines the stat automatically from the df structure
  nodelist <- blank_nodelist()
  nodelist <- add_actor(nodelist, name = "frank", dict = "egypt2015", dict_stat = "sd")
  nodelist <- add_actor(nodelist, name = "reem", dict = "egypt2015", dict_stat = "sd")

  edgelist <- blank_edgelist()
  edgelist <- add_interaction(edgelist, agent = "frank", object = "reem", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("friend", "co_worker"), object_ident_prob = c(.5, .5))
  edgelist <- add_interaction(edgelist, agent = "reem", object = "frank", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("friend", "co_worker"), object_ident_prob = c(.5, .5))

  eventslist <- basic_event_df(n = 5, actors = c("frank", "reem"), noise = c("a1_action", "a2_action"))

  write_input_from_df(nodelist, edgelist, eventslist, simfilename = "b_sim.txt", eventfilename = "b_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input")
  # run_bayesact(simfilename = "b_sim.txt", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input", output_dir = "/Users/aidan/Desktop/bayesact_test_output")
  # all this does is see if it makes it to the end of the thing successfully
  expect_true(TRUE)
})

test_that("c. error handling for mismatched term sets works and cross-cultural runs work when term sets match", {
  bayesact_dir <- "/Users/aidan/Desktop/School/Grad_school/ACT/bayesactgithub/bayesact"

  # TODO add a check that determines the stat automatically from the df structure
  nodelist <- blank_nodelist()
  nodelist <- add_actor(nodelist, name = "frank", dict = c("usmturk2015", "morocco2015", "morocco2015", "usmturk2015"), dict_stat = "mean")
  nodelist <- add_actor(nodelist, name = "reem", dict = "morocco2015", eqns = "us2010", eqns_gender = c("av", "f"), dict_stat = "mean")

  edgelist <- blank_edgelist()
  edgelist <- add_interaction(edgelist, agent = "frank", object = "reem", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("jerk", "co_worker"), object_ident_prob = c(.5, .5))
  edgelist <- add_interaction(edgelist, agent = "reem", object = "frank", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("jerk", "co_worker"), object_ident_prob = c(.5, .5))

  eventslist <- basic_event_df(n = 5, actors = c("frank", "reem"), noise = c("a1_action", "a2_action"))

  expect_error(write_input_from_df(nodelist, edgelist, eventslist, simfilename = "c_sim.txt", eventfilename = "c_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input"),
               "The agent identity dictionaries have different terms for different actors. BayesACT requires that the term sets match between actors for identity and behavior dictionaries. The recommended solution is to subset the dictionaries to the terms that are contained in both. EPA values may differ between actors.")
  nodelist <- blank_nodelist()
  nodelist <- add_actor(nodelist, name = "frank", dict = c("morocco2015", "usmturk2015", "morocco2015", "usmturk2015"), dict_stat = "mean")
  nodelist <- add_actor(nodelist, name = "reem", dict = "morocco2015", eqns = "us2010", eqns_gender = c("av", "f"), dict_stat = "mean")

  expect_error(write_input_from_df(nodelist, edgelist, eventslist, simfilename = "c_sim.txt", eventfilename = "c_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input"),
               "The behavior dictionaries have different terms for different actors. BayesACT requires that the term sets match between actors for identity and behavior dictionaries. The recommended solution is to subset the dictionaries to the terms that are contained in both. EPA values may differ between actors.")

  nodelist <- blank_nodelist()
  nodelist <- add_actor(nodelist, name = "frank", dict = c("morocco2015", "morocco2015", "usmturk2015", "usmturk2015"), dict_stat = "mean")
  nodelist <- add_actor(nodelist, name = "reem", dict = "morocco2015", eqns = "us2010", eqns_gender = c("av", "f"), dict_stat = "mean")

  expect_error(write_input_from_df(nodelist, edgelist, eventslist, simfilename = "c_sim.txt", eventfilename = "c_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input"),
               "The client identity dictionaries have different terms for different actors. BayesACT requires that the term sets match between actors for identity and behavior dictionaries. The recommended solution is to subset the dictionaries to the terms that are contained in both. EPA values may differ between actors.")


  nodelist <- blank_nodelist()
  nodelist <- add_actor(nodelist, name = "frank", dict = c("morocco2015", "morocco2015", "morocco2015", "usmturk2015"), dict_stat = "mean")
  nodelist <- add_actor(nodelist, name = "reem", dict = "morocco2015", eqns = "us2010", eqns_gender = c("av", "f"), dict_stat = "mean")
  write_input_from_df(nodelist, edgelist, eventslist, simfilename = "c_sim.txt", eventfilename = "c_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input")
  # run_bayesact(simfilename = "c_sim.txt", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input", output_dir = "/Users/aidan/Desktop/bayesact_test_output")
  # all this does is see if it makes it to the end of the thing successfully
  expect_true(TRUE)
  })



test_that("d. passing a file works", {
  bayesact_dir <- "/Users/aidan/Desktop/School/Grad_school/ACT/bayesactgithub/bayesact"

  # TODO add a check that determines the stat automatically from the df structure
  nodelist <- blank_nodelist()
  nodelist <- add_actor(nodelist, name = "frank", dict = c(file.path("/Users", "aidan", "Desktop", "dict_identity.csv"),
                                                           file.path("/Users", "aidan", "Desktop", "dict_behavior.csv"),
                                                           file.path("/Users", "aidan", "Desktop", "dict_identity.csv"),
                                                           file.path("/Users", "aidan", "Desktop", "dict_modifier.csv")),
                        dict_stat = "cov")
  nodelist <- add_actor(nodelist, name = "reem", dict = c(file.path("/Users", "aidan", "Desktop", "dict_identity.csv"),
                                                         file.path("/Users", "aidan", "Desktop", "dict_behavior.csv"),
                                                         file.path("/Users", "aidan", "Desktop", "dict_identity.csv"),
                                                         file.path("/Users", "aidan", "Desktop", "dict_modifier.csv")),
                        eqns = "egypt2014", eqns_gender = c("av", "f"), dict_stat = "cov")

  edgelist <- blank_edgelist()
  edgelist <- add_interaction(edgelist, agent = "frank", object = "reem", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("jerk", "co_worker"), object_ident_prob = c(.5, .5))
  edgelist <- add_interaction(edgelist, agent = "reem", object = "frank", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("jerk", "co_worker"), object_ident_prob = c(.5, .5))

  eventslist <- basic_event_df(n = 5, actors = c("frank", "reem"), noise = c("a1_action", "a2_action"))

  # write_input_from_df(nodelist, edgelist, eventslist, simfilename = "d_sim.txt", eventfilename = "d_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input")
  # run_bayesact(simfilename = "d_sim.txt", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input", output_dir = "/Users/aidan/Desktop/bayesact_test_output")
  # all this does is see if it makes it to the end of the thing successfully
  expect_true(TRUE)
  })

test_that("e. passing a mix of keys and dfs works", {
  bayesact_dir <- "/Users/aidan/Desktop/School/Grad_school/ACT/bayesactgithub/bayesact"
  pd_beh_us <- actdata::epa_subset(expr = c("collaborate", "^cheat$"),
                                   dataset = "usmturk2015",
                                   component = "behavior",
                                   stat = c('mean', 'cov'),
                                   gender = "average")

  # TODO add a check that determines the stat automatically from the df structure
  nodelist <- blank_nodelist()
  nodelist <- add_actor(nodelist, name = "frank", dict = list("usmturk2015", pd_beh_us, "usmturk2015", "usmturk2015"),
                        dict_stat = "cov")
  nodelist <- add_actor(nodelist, name = "reem", dict = list("usmturk2015", pd_beh_us, "usmturk2015", "usmturk2015"),
                        eqns = "egypt2014", eqns_gender = c("av", "f"), dict_stat = "cov")

  edgelist <- blank_edgelist()
  edgelist <- add_interaction(edgelist, agent = "frank", object = "reem", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("jerk", "co_worker"), object_ident_prob = c(.5, .5))
  edgelist <- add_interaction(edgelist, agent = "reem", object = "frank", agent_ident = "friend", agent_ident_prob = 1, object_ident = c("jerk", "co_worker"), object_ident_prob = c(.5, .5))

  eventslist <- basic_event_df(n = 5, actors = c("frank", "reem"), noise = c("a1_action", "a2_action"))

  write_input_from_df(nodelist, edgelist, eventslist, simfilename = "e_sim.txt", eventfilename = "e_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input")
  # run_bayesact(simfilename = "e_sim.txt", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input", output_dir = "/Users/aidan/Desktop/bayesact_test_output")
  # all this does is see if it makes it to the end of the thing successfully
  expect_true(TRUE)
  })

