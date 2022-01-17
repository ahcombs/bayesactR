
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesactR

<!-- badges: start -->
<!-- badges: end -->

bayesactR provides utilities that allow R users to run simulations using
the C package BayesACT, developed by Dr. Jesse Hoey and colleagues,
entirely from within R. You need the C code in order to run simulations
using this R package. This is not available publicly, but we will be
happy to provide it to you if you are interested in using it for
research purposes. If you are a researcher interested in using BayesACT,
contact [Dr. Jesse Hoey](https://cs.uwaterloo.ca/~jhoey/) or
[myself](https://sociology.duke.edu/aidan-combs) for a download link.

## Installation of bayesactR

You can install the development version of bayesactR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ahcombs/bayesactR")
```

## This version

Users should be aware that this package is currently in an early-stage
beta state. Key functionality has been implemented and has worked in my
local tests, but testing in other contexts has so far been limited. In
particular, all development and testing of this package has so far been
done on MacOS (11.2.3 - 11.6). If you try this, please expect and be
patient with some inevitable bumps in the road, particularly if you are
not a Mac user! Please get in touch with me (ahc26atduke.edu) if you
encounter any bugs or confusions or have thoughts about how this might
be made a more useful tool–any feedback is very helpful!

The current version of bayesactR is designed to work with BayesACT C
2.3.8, last modified on June 19, 2021.

## Overview

BayesACT is a theoretical extension of [affect control
theory](http://affectcontroltheory.org/). It provides testable
predictions regarding identity processes in social interaction. For more
information on BayesACT, see [Schröder, Hoey, and Rogers
(2016)](https://journals.sagepub.com/doi/abs/10.1177/0003122416650963)
and [bayesact.ca](http://bayesact.ca/).

Jesse Hoey and colleagues have developed and released an implementation
of BayesACT that is written in C and designed to be interfaced with via
the command line. bayesactR is essentially an R wrapper for this
package. The goals of bayesactR are (1) to make BayesACT more accessible
to those whose prefer working in R, (2) to make setting up and running
multiple simulations at one time simpler, and (3) to facilitate creating
analytic workflows that are easily reproducible.

There are three stages to running BayesACT simulations: a setup stage, a
run stage, and an analysis stage. Details on how this package
facilitates each stage are provided below, after instructions on how to
initially set up required prerequisites.

## Prerequisites: downloading and installing the BayesACT C package

If you want to use this package to run BayesACT simulations, you must
first also download and install the BayesACT C package. Contact a
developer and you will be sent a link to download a zip file containing
this code.

Make a note of the file path of the top level directory for the C
code–you will need this for some of the functions in this package. The
version of BayesACT that bayesactR is currently set up to work with is
listed at the top of this readme–I recommend that you install this same
version if possible.

The C package requires a few other pieces of software–see its
documentation for information beyond what is included here. In summary,
they are:

1.  A C compiler. In my testing I have used GCC, but this is a pain to
    set up on Mac, and is not generally preferred for most applications.
    Another option that is much simpler is to download is
    [XCode](https://developer.apple.com/xcode/) which includes the
    complier clang. Fair warning that this is a relatively large program
    and will likely take a long time to download and install. If you
    find that clang does not work, please let me know!

2.  [GSL (the GNU Scientific
    Library)](http://www.gnu.org/software/gsl/). The most recent version
    is recommended. If for some reason you must use an old version, see
    the BayesACT C documentation for instructions on how to modify
    things so that it will work.

3.  [Python
    **3.8**](https://www.python.org/downloads/release/python-380/). Note
    that this is **not** the most recent version of Python, but it is
    the one that the C code requires. On Mac, you must download this
    directly from [the Python
    website](https://www.python.org/downloads/release/python-380/) in
    order for it to be installed in a place where BayesACT can find it.
    Installing it using homebrew, anaconda, or other package managers
    unfortunately will not work!

The C package includes a number of examples and instructions on how to
run them using the command line. These may be helpful for debugging
purposes in order to ensure the C code is set up correctly.

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
about the kinds of events that can occur in the simulation. Within
bayesactR, this information is structured in a way that parallels that
often used in social network analysis and agent based modeling.
Information specific to individual actors is stored in a data frame
structured like a nodelist. Information about relationships between
actors is stored in a data frame structured like an edgelist. Finally,
information about events is stored in a sequentially ordered data frame,
and this is information is conceptually similar to algorithms that
define action in agent-based models. More information on each of these
three data structures and how to construct them in this package is
below.

### Actor nodelist

The actor nodelist contains information that is specific to each actor
in a simulation. Specifically, we need to know which sentiment
dictionaries and equations represent each actor’s understanding of the
world, and (optionally) we can define parameter values that control how
they manage uncertainty.

The recommended way to create the nodelist is to generate it using a
pair of provided functions `blank_nodelist()` and `add_actor()`.

#### Synergy with actdata

Each actor needs four dictionaries representing (a) meanings of
identities they assign to themselves, (b) meanings they assign to
behaviors, (c) meanings of identities they assign to their interaction
partners, and (4) meanings assigned to modifiers and emotions. These
dictionaries can be provided in one of three ways (mixing and matching
is allowed):

1.  If you have collected your own dictionaries or otherwise have access
    to non-public data sets, you may provide a filepath to the
    dictionary and equation files in the `dict` and `eqns` arguments.
    These must be properly formatted for BayesACT.
2.  If you are working with publicly available ACT sentiment
    dictionaries and equation sets, it is substantially easier and more
    replicable to specify dictionaries using keywords from the [actdata
    package](https://github.com/ahcombs/actdata). This package is a
    repository for standardized version of many publicly available ACT
    sentiment dictionaries and equation datasets, and it and bayesactR
    were developed to complement each other. If using dictionaries
    and/or equations from `actdata`, provide the applicable keyword as
    the `dict` or `eqns` argument. To see information about available
    data sets and gender subsets, see the [package
    readme](https://github.com/ahcombs/actdata) or call
    `actdata::dict_info()` or `actdata::eqn_info()`.
3.  Finally, dictionaries can be provided as data frame objects. This is
    particularly useful when you wish to use a subset of terms from a
    public dictionary–for example, perhaps you only want your agents to
    be able to take a limited set of behaviors, or identities from just
    one institution, rather than having access to the whole list. The
    `epa_subset()` function within `actdata` makes creating subsets from
    public data straightforward.

Below is an example showing the syntax for specifying dictionaries using
option 2, dataset keys. Later in this readme there is another option
using option 3, dataframe objects that are subsets of dictionaries.

``` r
library(bayesactR)
#> Loading required package: actdata

# blank_nodelist() creates an empty data frame with the correct column labels
nodelist <- blank_nodelist()

# add_actor() appends a line representing an actor to this data frame. If dictionaries, equations, or dict/eqn stats or genders are not specified, they will revert to defaults. 
nodelist <- add_actor(nodelist, name = "Ingrid", dict = "germany2007", eqns = "germany2007", eqns_gender = "av")

# To add another actor, use add_actor() again. Different parameter values can be specified for each actor.
# For Felix we use the actdata keyword for the Germany 2007 sentiment dictionary and equations, and we use the values collected from men.
nodelist <- add_actor(nodelist, name = "Felix", dict = "germany2007", dict_gender = "male", eqns = "germany2007", eqns_gender = "av", alphas = 1)

knitr::kable(nodelist)
```

| name   | dict                                               | dict_stat | dict_gender | eqns        | eqns_gender | alphas |
|:-------|:---------------------------------------------------|:----------|:------------|:------------|:------------|:-------|
| Ingrid | germany2007, germany2007, germany2007, germany2007 | mean      | average     | germany2007 | av          | NA     |
| Felix  | germany2007, germany2007, germany2007, germany2007 | mean      | male        | germany2007 | av          | 1      |

#### A note about dictionaries and cross-cultural interaction

When two actors are based in the same culture, it is reasonable to
assign them the same set of dictionaries and equations. When two actors
are from different cultures, we may want to instead assign them
different dictionaries and equations. This is possible in BayesACT, but
there is a caveat: the dictionaries for all actors (except for modifier
dictionaries) must contain the same sets of words. If the set of terms
differs between dictionaries, one agent will not be able to comprehend
the action performed or identity assigned by the other, and BayesACT
will crash. The recommended workaround is simply to subset each of the
desired dictionaries so that each contains only the terms that are
present in all others. The `epa_subset()` function in `actdata` makes
this kind of manipulation reasonably straightforward.

#### An example:

In this example, we say that Sally is American and so uses the meanings
in one of the recent U.S. dictionaries. We say Reem is Egyptian and uses
meanings from Egyptian dictionaries. We subset both the U.S. and Egypt
dictionaries to contain the same set of identities and behaviors.

BayesACT takes into account uncertainty around identity meanings. This
can be represented by an arbitrary constant around mean values or
standard deviation or covariance information calculated from EPA
measurement data. Most older public datasets contain only mean values,
but more recent data collections (2015 or newer) also contain standard
deviation and covariance information. In this example we use two recent
datasets and we perform the BayesACT simulation using the covariance
information they contain.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(actdata)

egypt_identity <- actdata::epa_subset(dataset = "egypt2015", component = "identity", gender = "average", stat = c("mean", "cov")) %>% 
  dplyr::semi_join(actdata::epa_subset(dataset = "usfullsurveyor2015", component = "identity", gender = "average"), by = "term")

us_identity <- actdata::epa_subset(dataset = "usfullsurveyor2015", component = "identity", gender = "average", stat = c("mean", "cov")) %>% 
  dplyr::semi_join(actdata::epa_subset(dataset = "egypt2015", component = "identity", gender = "average"), by = "term")

egypt_behavior <- actdata::epa_subset(dataset = "egypt2015", component = "behavior", gender = "average", stat = c("mean", "cov")) %>% 
  dplyr::semi_join(actdata::epa_subset(dataset = "usfullsurveyor2015", component = "behavior", gender = "average"), by = "term")

us_behavior <- actdata::epa_subset(dataset = "usfullsurveyor2015", component = "behavior", gender = "average", stat = c("mean", "cov")) %>% 
  dplyr::semi_join(actdata::epa_subset(dataset = "egypt2015", component = "behavior", gender = "average"), by = "term")

head(egypt_identity)
#> # A tibble: 6 × 19
#>   term    dataset context year  component gender     E     P     A cov_EE cov_EP
#>   <chr>   <chr>   <chr>   <chr> <chr>     <chr>  <dbl> <dbl> <dbl>  <dbl>  <dbl>
#> 1 aborti… egypt2… Egypt   2015  identity  avera… -1.89 -0.58  0.88   5.99   2.59
#> 2 adoles… egypt2… Egypt   2015  identity  avera…  0.17  0.06  0.6    4.45   1.87
#> 3 adult   egypt2… Egypt   2015  identity  avera…  1.37  0.94  0.02   3.31   0.47
#> 4 adulte… egypt2… Egypt   2015  identity  avera… -2.99 -1.88  2.26   4.26   2.27
#> 5 adulte… egypt2… Egypt   2015  identity  avera… -3.44 -2.19  2.14   2.3    1.89
#> 6 air_fo… egypt2… Egypt   2015  identity  avera…  2.17  1.95  0.14   3.51   1.06
#> # … with 8 more variables: cov_EA <dbl>, cov_PE <dbl>, cov_PP <dbl>,
#> #   cov_PA <dbl>, cov_AE <dbl>, cov_AP <dbl>, cov_AA <dbl>, instcodes <chr>

# We can provide these data frames by passing a list to the dict argument. 
# The order is c(agent_identity, agent_behavior, object_identity, agent_emotion).
# Modifier term sets do not have to match, so instead of going to the trouble of creating modifier subsets, 
# here we pass the actdata dataset key for the modifier slot instead.
nodelist <- blank_nodelist()
nodelist <- add_actor(nodelist, name = "Sally", dict = list(us_identity, us_behavior, us_identity, "usfullsurveyor2015"))
# Reem also uses the Egyptian equations (the default, which Sally uses, is us2010).
nodelist <- add_actor(nodelist, name = "Reem", dict = list(egypt_identity, egypt_behavior, egypt_identity, "egypt2015"), eqns = "egypt2014", eqns_gender = c("av", "f"), alphas = 1)
```

### Interaction edgelist

The interaction edgelist contains information that defines relationships
between actors–in particular, the identities they will ascribe to
themselves and their alter at the outset of an interaction. The process
for creating this edgelist is very similar to that for creating the
nodelist:

``` r
# creates a blank data frame with the correct column names
edgelist <- blank_edgelist()

# Note that interactions are directed--how one actor views herself does not necessary match how her partner views her. 
# The focal actor is referred to as the agent, and the partner is referred to as the object or client. 
# Sally views herself as a teacher and Reem as a student when they interact.
edgelist <- add_interaction(edgelist, agent = "Sally", object = "Reem", agent_ident = "teacher", agent_ident_prob = 1, object_ident = "student", object_ident_prob = 1)

# When she interacts with Sally, Reem usually sees herself as a student (p = .9) but sometimes as a genius (p = .1). She usually sees Sally as a teacher (p - .85) but occasionally as a bore (p = .15). 
edgelist <- add_interaction(edgelist, agent = "Reem", object = "Sally", agent_ident = c("student", "genius"), agent_ident_prob = c(.9, .1), object_ident = c("teacher", "bore"), object_ident_prob = c(.85, .15))

knitr::kable(edgelist)
```

| agent | object | agent_ident     | agent_ident_prob | object_ident  | object_ident_prob |
|:------|:-------|:----------------|:-----------------|:--------------|:------------------|
| Sally | Reem   | teacher         | 1                | student       | 1                 |
| Reem  | Sally  | student, genius | 0.9, 0.1         | teacher, bore | 0.85, 0.15        |

### Event list

The last piece of input information is the event list. The event list is
a data frame that has one line per turn in the simulation and defines
who can act and what they can do on that turn.

bayesactR provides a function for generating relatively simple events
files in which actors take either the BayesACT-optimal action, the
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
# Sally and Reem will take 10 turns using the default specifications: bayesact optimal actions and no emotion expression. A small amount of noise will be added to each person's perception of the other's action--this means that there is a chance actions will be misinterpreted by the observing party. 
eventlist <- basic_event_df(n = 10, actors = c("Sally", "Reem"), noise = c("a1_action", "a2_action"))

knitr::kable(eventlist)
```

| agent | agent_action | agent_emotion | object | object_action | object_emotion |
|:------|:-------------|:--------------|:-------|:--------------|:---------------|
| Sally | \*+          |               | Reem   |               |                |
| Reem  | \*+          |               | Sally  |               |                |
| Sally | \*+          |               | Reem   |               |                |
| Reem  | \*+          |               | Sally  |               |                |
| Sally | \*+          |               | Reem   |               |                |
| Reem  | \*+          |               | Sally  |               |                |
| Sally | \*+          |               | Reem   |               |                |
| Reem  | \*+          |               | Sally  |               |                |
| Sally | \*+          |               | Reem   |               |                |
| Reem  | \*+          |               | Sally  |               |                |

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
“bayesact_input” that lives under your current working directory. If you
want the files to be saved to a different directory, provide a filepath
to the `input_dir` argument. This function returns the filepath at which
it saved the input files.

``` r
write_input_from_df(nodelist, edgelist, eventlist, simfilename = "readme_simfile.txt", eventfilename = "readme_eventfile.events", bayesact_dir = "/path/to/my/bayesact/Cpackage/top/level/directory/")
```

## Running BayesACT using bayesactR

The function used to run simulations is `run_bayesact()`. It requires
the file name we gave the input sim file, the path to the top level
directory of the Bayesact C package, the path to the directory where the
input files were saved (if something other than “bayesact_input” under
the working directory, which is the default), and the path where the
output should be saved (“bayesact_output” under the current working
directory is the default).

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
`read.csv()` or `readr::read_csv()`. This csv file contains one row per
actor turn, and reports a number of different statistics about the state
of the interaction on each turn.

It’s easy to see here that many of the identities and behaviors chosen
do not make much sense in context. Subsetting the dictionaries to
identities that only belong to a relevant institution may help here.

``` r
results <- read.csv2("/path/to/output/readme_simfile.csv", sep = ",", header = TRUE)
head(results[,1:6])
```

| iteration | dyad.0 | dyad.1 | AGENT..agent.name | AGENT..agent.ids                                                                                                                       | AGENT..agent.id.probabilities                                                      |
|----------:|-------:|-------:|:------------------|:---------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------|
|         0 |      0 |      1 | Sally             | teacher                                                                                                                                | 1                                                                                  |
|         1 |      1 |      0 | Reem              | genius,non_smoker,consultant,doctor,conservative,surgeon,assistant                                                                     | 0.806,0.082,0.05,0.026,0.024,0.01,0.002                                            |
|         2 |      0 |      1 | Sally             | teacher,skilled_worker                                                                                                                 | 0.94,0.06                                                                          |
|         3 |      1 |      0 | Reem              | genius,conservative,non_smoker,doctor,surgeon,consultant,pediatrician,navy_officer,assistant,scholar,organizer,teacher,colleague,judge | 0.488,0.151,0.145,0.076,0.055,0.04,0.017,0.009,0.006,0.004,0.003,0.002,0.002,0.002 |
|         4 |      0 |      1 | Sally             | teacher,skilled_worker                                                                                                                 | 0.921,0.079                                                                        |
|         5 |      1 |      0 | Reem              | consultant,assistant                                                                                                                   | 0.999,0.001                                                                        |

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
# What happens if the probability that Reem sees Sally as a teacher versus a bore varies? Let's run 5 simulations with different values of object_ident_prob to find out.
# This is defined in the edgelist, so the nodelist and events files can just be created once. This code is the same as used above. 

nodelist <- blank_nodelist()
nodelist <- add_actor(nodelist, name = "Sally", dict = list(us_identity, us_behavior, us_identity, "usfullsurveyor2015"))
nodelist <- add_actor(nodelist, name = "Reem", dict = list(egypt_identity, egypt_behavior, egypt_identity, "egypt2015"), eqns = "egypt2014", eqns_gender = c("av", "f"), alphas = 1)

eventlist <- basic_event_df(n = 6, actors = c("Sally", "Reem"), noise = c("a1_action", "a2_action"))

# We need to create a different edgelist for each of the simulations and write out different input files for each as well. We will do this in a for loop. 
# the list of probabilities of Reem seeing Sally as a teacher that we will loop over
p_teacher = seq(.05, .95, .2)

for(i in 1:5){
  this_p_teacher <- p_teacher[i]
  # the probabilities of all identities combined must always sum to 1
  this_p_bore <- 1 - this_p_teacher
  
  
  edgelist <- blank_edgelist()
  edgelist <- add_interaction(edgelist, agent = "Sally", object = "Reem", agent_ident = "teacher", agent_ident_prob = 1, object_ident = "student", object_ident_prob = 1)
  edgelist <- add_interaction(edgelist, agent = "Reem", object = "Sally", agent_ident = c("student", "genius"), agent_ident_prob = c(.9, .1), object_ident = c("teacher", "bore"), object_ident_prob = c(this_p_teacher, this_p_bore))
  
  # each simulation file needs a different name
  simname <- paste0("readme_simfile_batch_", i, ".txt")
  
  # write out input files
  write_input_from_df(nodelist, edgelist, eventlist, simfilename = simname, eventfilename = "readme_eventfile_batch.events", bayesact_dir = "/path/to/my/bayesact/Cpackage/top/level/directory/")
  
  # run the current simulation
  run_bayesact(simname, bayesact_dir = "/path/to/my/bayesact/Cpackage/top/level/directory/")
}
```
