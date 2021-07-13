
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesactR

<!-- badges: start -->

<!-- badges: end -->

bayesactR provides utilities that allow R users to run simulations using
the [C package BayesACT](https://github.com/jessehoey/bayesact),
developed by Dr. Jesse Hoey and colleagues, entirely from within R. The
current version of bayesactR is designed to work with BayesACT 2.3.8,
last modified on June 19, 2021.

## Installation

You can install the development version of bayesactR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ahcombs/bayesactR")
```

## Overview

BayesACT is a theoretical extension of [affect control
theory](http://affectcontroltheory.org/). It provides testable
predictions regarding identity processes in social interaction. For more
information on BayesACT, see [Schröder, Hoey, and Rogers
(2016)](https://journals.sagepub.com/doi/abs/10.1177/0003122416650963)
and [bayesact.ca](http://bayesact.ca/).

Jesse Hoey and colleagues have developed and released an [implementation
of BayesACT](https://github.com/jessehoey/bayesact) that is written in C
and designed to be interfaced with via the command line. bayesactR is
essentially an R wrapper for this package. The goals of bayesactR are
(1) to make BayesACT more accessible to those whose prefer working in R,
(2) to make setting up and running multiple simulations at one time
simpler, and (3) to facilitate creating analytic workflows that are
easily reproducible.

There are three stages to running BayesACT simulations: a setup stage, a
run stage, and an analysis stage. Details on how this package
facilitates each stage are provided below.

## Setup: Creating input files

BayesACT needs some information about actors, their relationships with
one another, and the structure of their interaction. These parameters
are provided to the C package via specially formatted text files. These
files can be created by hand (and many examples are provided in the
BayesACT C package and documentation), however, bayesactR also provides
functions that generate them automatically. Automatic generation of the
required input files is easier when running batches of simulations, more
reproducible, and less prone to errors than manual creation.

The information needed to generate the proper input files can be divided
into three types: information about individual actors, information about
the actors’ initial relationships with one another, and information
about the kinds of events that can occur in the simulation. Information
specific to individual actors is stored in a data frame structured like
a *nodelist* and information about relationships between actors is
stored in a data frame structured like an *edgelist*. These terms are
used because this structure parallels one often used in social network
analysis. Finally, information about events is stored in a sequentially
ordered data frame. More information on each of these three data
structures and how to construct them in this package is below.

### Actor nodelist

The actor nodelist contains information that is specific to each actor
in a simulation. Specifically, we need to know which sentiment
dictionaries and equations represent each actor’s understanding of the
world, and (optionally) we can define parameter values that control how
they manage uncertainty.

The recommended way to create the nodelist is to generate it using a
pair of provided functions `blank_nodelist()` and `add_actor()`.

Note that theoretically, it should be possible to run interactions where
different actors are assigned different dictionaries, simulating
cross-cultural interaction. However, at the time of this writing this
seems to sometimes cause errors, so assigning all actors the same set of
dictionaries is recommended.

#### Synergy with actdata

Dictionaries can be provided in one of two ways. If you have collected
your own dictionaries or otherwise have access to non-public data sets,
you may provide a filepath to the dictionary and equation files in the
`dict` and `eqns` arguments. However, if you are working with publicly
available ACT sentiment dictionaries and equation sets, it is
substantially easier and more replicable to specify dictionaries using
keywords from the [actdata package](https://github.com/ahcombs/actdata).
This package is a repository for many publicly available ACT sentiment
dictionaries and equation datasets, and it and bayesactR were developed
to complement each other. If using dictionaries and/or equations from
`actdata`, provide the applicable keyword as the `dict` or `eqns`
argument. To see information about available data sets and gender
subsets, call `actdata::dict_info()` or `actdata::eqn_info()`. For more
information about keywords and other details regarding `actdata`, check
the [package readme](https://github.com/ahcombs/actdata) and
documentation.

``` r
library(bayesactR)
#> Loading required package: actdata

# blank_nodelist() creates an empty data frame with the correct column labels
nodelist <- blank_nodelist()

# add_actor() appends a line representing an actor to this data frame. If dictionaries, equations, or dict/eqn types or genders are not specified, they will revert to defaults. 
nodelist <- add_actor(nodelist, name = "Sally", dict = "germany2007", eqns = "germany2007", eqns_gender = "av")

# To add another actor, use add_actor() again. Different parameter values can be specified for each actor.
# For Felix we use the actdata keyword for the Germany 2007 sentiment dictionary and equations, and we use the values collected from men.
nodelist <- add_actor(nodelist, name = "Felix", dict = "germany2007", dict_gender = "male", eqns = "germany2007", eqns_gender = "av", alphas = 1)

knitr::kable(nodelist)
```

| name  | dict        | dict\_type | dict\_gender | eqns        | eqns\_gender | alphas |
| :---- | :---------- | :--------- | :----------- | :---------- | :----------- | :----- |
| Sally | germany2007 | mean       | av           | germany2007 | av           | NA     |
| Felix | germany2007 | mean       | male         | germany2007 | av           | 1      |

### Interaction edgelist

The interaction edgelist contains information that defines relationships
between actors–in particular, the identities they will ascribe to
themselves and their alter at the outset of an interaction. The process
for creating this edgelist is very similar to that for creating the
nodelist:

``` r
# creates a blank data frame with the correct column names
edgelist <- blank_edgelist()

# Note that interactions are directed--how one actor views herself does not necessary match how her partner views her. The focal actor is referred to as the agent, and the partner is referred to as the object. Sally views herself as a teacher and Felix as a student when they interact.
edgelist <- add_interaction(edgelist, agent = "Sally", object = "Felix", agent_ident = "teacher", agent_ident_prob = 1, object_ident = "student", object_ident_prob = 1)

# When he interacts with Sally, Felix usually sees himself as a student (p = .7) but sometimes as a whiz kid (p = .3). He usually sees Sally as a teacher (p - .85) but occasionally as a stuffed shirt (p = .15). 
edgelist <- add_interaction(edgelist, agent = "Felix", object = "Sally", agent_ident = c("student", "whiz_kid"), agent_ident_prob = c(.7, .3), object_ident = c("teacher", "stuffed_shirt"), object_ident_prob = c(.85, .15))

knitr::kable(edgelist)
```

| agent | object | agent\_ident       | agent\_ident\_prob | object\_ident           | object\_ident\_prob |
| :---- | :----- | :----------------- | :----------------- | :---------------------- | :------------------ |
| Sally | Felix  | teacher            | 1                  | student                 | 1                   |
| Felix | Sally  | student, whiz\_kid | 0.7, 0.3           | teacher, stuffed\_shirt | 0.85, 0.15          |

### Event list

The last piece of input information is the event list. The event list is
a data frame that has one line per turn in the simulation and defines
who can act and what they can do on that turn.

bayesactR provides a function for generating relatively simple events
files in which actors take either the bayesact-optimal action, the
interact-optimal action, or a specific action from the dictionary on
each of their turns. Actors must switch off on some regular interval.

It is possible to use more complex events files (see the BayesACT C
package documentation), but this simple structure should suffice for
many applications. It can also be used as a base from which to build
more complex specifications.

The contents of this file are more cryptic than the nodelist and
edgelist. Each row represents one turn in the simulation. On each turn,
one or both actors will have an entry in their action column–this is
what they will do on that turn. Similarly, neither, one or both may have
an entry in their emotion column–this is the emotion they will express.
Asterisks in these columns indicate that the action or emotion will be
the one that is optimal (least deflecting) according to BayesACT.
Exclamation points mean that it is optimal according to affect control
theory (Interact). A plus sign after the entry indicates that a small
amount of noise will be added to the other party’s perception of the
action or emotion.

``` r
# Sally and Felix will take 10 turns using the default specifications: bayesact optimal actions and no emotion expression. A small amount of noise will be added to each person's perception of the other's action--this means that there is a chance actions will be misinterpreted by the observing party. 
eventlist <- basic_event_df(n = 10, actors = c("Sally", "Felix"), noise = c("a1_action", "a2_action"))

knitr::kable(eventlist)
```

| agent | agent\_action | agent\_emotion | object | object\_action | object\_emotion |
| :---- | :------------ | :------------- | :----- | :------------- | :-------------- |
| Sally | \*+           |                | Felix  |                |                 |
| Felix | \*+           |                | Sally  |                |                 |
| Sally | \*+           |                | Felix  |                |                 |
| Felix | \*+           |                | Sally  |                |                 |
| Sally | \*+           |                | Felix  |                |                 |
| Felix | \*+           |                | Sally  |                |                 |
| Sally | \*+           |                | Felix  |                |                 |
| Felix | \*+           |                | Sally  |                |                 |
| Sally | \*+           |                | Felix  |                |                 |
| Felix | \*+           |                | Sally  |                |                 |

### Writing input data frames to file

Now that we have created the three data frames, we need to generate the
text files that the BayesACT C code takes as input. bayesactR provides
the `write_input_from_df()` function for this purpose. In addition to
the three dataframes, this function needs file names for the two text
files that it will write. The sim file contains the actor and
interaction information, and must have the extension .txt. The event
file contains the event information and must have the extension .events.
This function also requires the filepath for the directory that houses
the BayesACT C package on your machine (see the section on downloading
and installing the BayesACT C package below). By default, the function
will put the text files it generates in a directory called
“bayesact\_input” that lives under your current working directory. If
you want the files to be saved to a different directory, provide a
filepath to the `input_dir` argument. This function returns the filepath
at which it saved the input files.

``` r
write_input_from_df(nodelist, edgelist, eventlist, simfilename = "readme_simfile.txt", eventfilename = "readme_eventfile.events", bayesact_dir = "/path/to/my/bayesact/Cpackage/top/level/directory/")
```

## Run

Now that we have the input files ready to go, it’s almost time to run
the simulation\! Before this will work, however, we have to do some
setup work to get the BayesACT C package installed and running. This
only needs to be done once. Do note, however, that BayesACT is still
under active development, and so it may be updated occasionally. I will
do my best to ensure that this package is also updated promptly so that
it works with the most recent version of BayesACT. However, there will
likely be a delay between these updates.

### Prerequisite: downloading and installing the BayesACT C package

In future iterations of this package, I hope to provide functions that
will help users get the BayesACT C package set up, but for now, there
are a few steps that must be done manually.

To get the most recent version of the BayesACT C package, visit [its
GitHub repo](https://github.com/jessehoey/bayesact) and either clone the
repo to a local directory (recommended; this enables you to more easily
update to the latest GitHub version) or download and decompress the ZIP
file. Make a note of where the top level directory lives on your
machine–you will need this file path for the write\_input\_from\_df
function.

In addition, note that BayesACT requires you to have a C compiler and
GSL installed on your machine. See the BayesACT installation
instructions contained in the C package readme and PDF documentation for
more information.

### Running BayesACT using bayesactR

Once the C package is set up, we can run the simulation we have set up\!
The function for this is `run_bayesact()`. It requires the file name we
gave the input sim file, the path to the top level directory of the
Bayesact C package, the path to the directory where the input files were
saved (if something other than “bayesact\_input” under the working
directory, which is the default), and the path where the output should
be saved (“bayesact\_output” under the current working directory is the
default).

This will probably take a minute or two to run (longer if there are more
events).

``` r
run_bayesact(simfilename = "readme_simfile.txt", bayesact_dir = "/path/to/my/bayesact/Cpackage/top/level/directory/")
```

## Analysis

BayesACT output comes in a few different formats which are described
below.

### CSV output

Likely of most use for analyzing results is the csv output. This is
saved to the directory specified in `run_bayesact()`. Read this in with
`read.csv()` or `read_csv()`. This csv file contains one row per actor
turn, and reports a number of different statistics about the state of
the interaction on each turn.

### Terminal output

When BayesACT is run from the command line, it produces a large amount
of text output that is printed to the terminal. The bayesactR package
saves this output to a text file. It is useful for debugging purposes in
the case that BayesACT does not produce the expected output.

### Plots

BayesACT also produces plots that show how actors’ identities shift in
affective space through the course of the interaction. These are saved
to the “output” folder under the BayesACT C package top level directory.

## Batches of simulations

Now that we know how to set up, run, and analyze results from a single
situation, we can think about how to efficiently scale up. It is likely
that many BayesACT applications will require running simulations over a
range of parameter settings, making running batches in a replicable way
very useful.

The functions described above are all amenable to being run inside of
loops. Notably, it may not be necessary to recreate every dataframe for
every run. For example, if a user wants to run a number of simulations
where the probability that the actor takes each of their possible
identities (`agent_ident_prob` in the interaction edgelist) varies, only
the edgelist needs to be modified. The nodelist and events file can be
created once and passed to `write_input_from_df()` repeatedly.

``` r
# What happens if the probability that Felix sees himself as a student versus a whiz kid varies? Let's run 5 simulations with different values of agent_ident_prob to find out.
# This is defined in the edgelist, so the nodelist and events files can just be created once. This code is the same as used above. 

nodelist <- blank_nodelist()
nodelist <- add_actor(nodelist, name = "Sally", dict = "germany2007", dict_gender = "av", eqns = "germany2007", eqns_gender = "av")
nodelist <- add_actor(nodelist, name = "Felix", dict = "germany2007", dict_gender = "male", eqns = "germany2007", eqns_gender = "av", alphas = .3)

eventlist <- basic_event_df(n = 6, actors = c("Sally", "Felix"), noise = c("a1_action", "a2_action"))

# We need to create a different edgelist for each of the simulations and write out different input files for each as well. We will do this in a for loop. 
# the list of probabilities of Felix seeing himself as a student that we will loop over
p_student = seq(.05, .95, .2)

for(i in 1:5){
  this_p_student <- p_student[i]
  # the probabilities of all identities combined must always sum to 1
  this_p_whizkid <- 1 - this_p_student
  
  edgelist <- blank_edgelist()
  edgelist <- add_interaction(edgelist, agent = "Sally", object = "Felix", agent_ident = "teacher", agent_ident_prob = 1, object_ident = "student", object_ident_prob = 1)
  edgelist <- add_interaction(edgelist, agent = "Felix", object = "Sally", agent_ident = c("student", "whiz_kid"), agent_ident_prob = c(this_p_student, this_p_whizkid), object_ident = c("teacher", "stuffed_shirt"), object_ident_prob = c(.85, .15))
  
  # each simulation file needs a different name
  simname <- paste0("readme_simfile_batch_", i, ".txt")
  
  # write out input files
  write_input_from_df(nodelist, edgelist, eventlist, simfilename = simname, eventfilename = "readme_eventfile_batch.events", bayesact_dir = "/path/to/my/bayesact/Cpackage/top/level/directory/")
  
  # run the current simulation
  run_bayesact(simname, bayesact_dir = "/Users/aidan/Desktop/School/Grad_school/bayesactgithub/bayesact")
}
```
