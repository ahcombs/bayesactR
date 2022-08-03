// *******************************************************************************************************************************
// AGENTS
// *******************************************************************************************************************************
//
// each agent has dictionaries for identities and the impression formation equation and emotion equation parameter files
// define the agents and specify a dictionary and dictionary type for each entity type
// the entity types must match entity_type in fvarsample.h
agent: Sally
dictionary: AGENT : dict_identity_7.csv : MEAN
dictionary: BEHAVIOUR : dict_behavior_7.csv : MEAN
dictionary: CLIENT : dict_identity_7.csv : MEAN
dictionary: EMOTION : usfullsurveyor2015_modifier_average_mean_1.csv : MEAN
dynamics: IMPRESSION : us2010_impressionabo_av_eqn.dat
dynamics: EMOTION : us2010_emotionid_f_eqn.dat
endagent

agent: Reem
alphas: 1
dictionary: AGENT : dict_identity_8.csv : MEAN
dictionary: BEHAVIOUR : dict_behavior_8.csv : MEAN
dictionary: CLIENT : dict_identity_8.csv : MEAN
dictionary: EMOTION : egypt2015_modifier_average_mean_1.csv : MEAN
dynamics: IMPRESSION : egypt2014_impressionabo_av_eqn.dat
dynamics: EMOTION : egypt2014_emotionid_f_eqn.dat
endagent

// *******************************************************************************************************************************
// INTERACTIONS
// *******************************************************************************************************************************
// these are the possible pairs of agents who can interact and their identity profiles for each interaction
// each interaction starts with a line with the keyword interaction
// followed by the agent's name
// followed by the client's name
// Then a line for the agent's self identity
// and then a line for the agent's interpretation of the client's identity
// each identity interpretation is a probability distribution such that
// A : p : B : q : C : 1-p-q  means the agent has identity A with probability p B with probability q and C with probability 1-p-q
// probabilities must sum to 1
interaction: Sally: Reem
Sally : teacher : 1
Reem : student : 1
endinteraction

interaction: Reem: Sally
Reem : student : 0.9 : genius : 0.1
Sally : teacher : 0.85 : bore : 0.15
endinteraction

// *******************************************************************************************************************************
// EVENTS
// *******************************************************************************************************************************
num_iterations : 10
events: /Users/aidan/Desktop/School/Grad_school/ACT/inteRact/bayesactR/vignettes/example_sim/readme_eventfile.events
simtype : events
