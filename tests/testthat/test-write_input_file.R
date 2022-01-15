test_that("actor error handling works", {
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

  # TODO add an option to search exactly to the subset function
  pd_ident_egypt <- actdata::epa_subset(expr = ident_terms,
                                        dataset = "egypt2015",
                                        component = "identity",
                                        stat = c('mean', 'cov'),
                                        gender = "average")
  pd_ident_us <- actdata::epa_subset(expr = ident_terms,
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
  edgelist <- add_interaction(edgelist, agent = "frank", object = "reem", agent_ident = "partner", agent_ident_prob = 1, object_ident = c("partner", "co_worker"), object_ident_prob = c(.5, .5))
  edgelist <- add_interaction(edgelist, agent = "reem", object = "frank", agent_ident = "partner", agent_ident_prob = 1, object_ident = c("partner", "co_worker"), object_ident_prob = c(.5, .5))

  eventslist <- basic_event_df(n = 10, actors = c("frank", "reem"), noise = c("a1_action", "a2_action"))

  write_input_from_df(nodelist, edgelist, eventslist, simfilename = "a_sim.csv", eventfilename = "a_event.events", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input")

  run_bayesact(simfilename = "a_sim.csv", bayesact_dir = bayesact_dir, input_dir = "/Users/aidan/Desktop/bayesact_test_input", output_dir = "/Users/aidan/Desktop/bayesact_test_output")
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

