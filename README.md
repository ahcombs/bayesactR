
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesactR

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/337827226.svg)](https://doi.org/10.5281/zenodo.14652420)
<!-- badges: end -->

bayesactR provides utilities that allow R users to run simulations using
the C package BayesACT, developed by Dr. Jesse Hoey and colleagues,
entirely from within R.

Visit the [Getting Started
page](https://ahcombs.github.io/bayesactR/articles/bayesactR.html) for
installation information. For examples of how to set up and run
simulations, visit [this help
page](https://ahcombs.github.io/bayesactR/articles/run_elements.html).

## Bayesian Affect Control Theory

BayesACT, developed by Drs. Jesse Hoey, Tobias Schröder, Kimberly
Rogers, and colleagues, is a theoretical extension of [affect control
theory](http://affectcontroltheory.org/). A short summary of affect
control theory (ACT) and BayesACT is provided below. See
[affectcontroltheory.org](http://affectcontroltheory.org/), [Schröder,
Hoey, and Rogers
(2016)](https://journals.sagepub.com/doi/abs/10.1177/0003122416650963),
and [bayesact.ca](http://bayesact.ca/) for more information.

ACT is a social psychological theory of interaction that models how the
affective meanings of people and behaviors relate to what we expect to
happen in a situation. Within ACT, words describing people and behaviors
are modeled as having locations in a three-dimensional space of
affective meaning. These dimensions are evaluation (good/bad), potency
(powerful/weak), and activity (active/quiet), and range between
approximately -4 and 4. These affective meanings have been measured
empirically in a number of data collection efforts across several
different countries since the 1960s. For example, in one recent data
collection, the word *teacher* was found to be seen on average as
extremely good (E = 2.62), quite powerful (P = 1.82) and slightly active
(A = 1.3).

These values can be used to run simulations of interactions and the
results of these simulations can tell us what kinds of behaviors we
expect particular kinds of people to engage in, how we expect people who
do certain things to be labeled, how strange particular social
interactions seem to us, and more. These results can and have been
verified through comparisons to empirical data.

In the mathematics of the core theory, meanings are treated as points in
the three-dimensional meaning space. BayesACT, by contrast, treats them
as Gaussian distributions, with a mean and a variance. Simulations in
BayesACT are probabilistic. On each run, values are sampled from the
distributions for the relevant terms, and results are determined based
on those sampled values. The output of a BayesACT simulation is a
compilation or summary of the results across a number of individual
runs.

## Why bayesactR?

Hoey and colleagues have developed and released an implementation of
BayesACT that is written in C and designed to be interfaced with via the
command line. bayesactR is an R wrapper for this tool.

The goals of bayesactR are (1) to make BayesACT more accessible to
social scientists who prefer R-based rather than command-line-based
workflows, (2) to make setting up and running multiple simulations at
one time simpler, and (3) to facilitate creating analytic workflows that
are easily reproducible.

## This is a work in progress!

The current version of bayesactR is designed to work with BayesACT C
2.3.8, last modified on June 19, 2021.

This package is currently in an early-stage beta state. Key
functionality has been implemented and has worked in my local tests, but
testing in other contexts has so far been limited. In particular, all
development and testing of this package has so far been done on MacOS
(11.2.3 - 12.3.1). Development is ongoing, and I can’t promise that
there won’t be breaking changes in future versions. I ask that you bear
with me as I work towards the goal of developing a tool that is as
flexible, useful, and user-friendly as possible!

Please get in touch with me (ahc26atduke.edu) if you encounter any bugs
or confusions or have thoughts about how this might be made a more
useful tool. All feedback is helpful and appreciated!

## You may also be interested in…

This package was developed in conjunction with two other open-source R
packages that may also be of interest to the ACT research community.
Together, the goal of these packages is to make ACT research more
accessible, and to make it possible to use R to do analysis in a
self-contained and completely reproducible and transparent way.

-   [**inteRact**](https://ekmaloney.github.io/inteRact/): Like
    bayesactR, inteRact, developed and maintained by [Em
    Maloney](https://sociology.duke.edu/em-maloney), is a package that
    allows users to run affect control theory simulations in R. It was
    built to be an open source, R-based version of its namesake Java
    program, INTERACT, which has been used by researchers to run affect
    control theory simulations since the 1990s. The conceptual
    difference between inteRact and bayesactR is that inteRact runs
    simulations using the mathematics of the core theory, rather than
    those of the Bayesian extension. In a nutshell, this means that it
    treats EPA values as points, rather than distributions, and
    simulations are deterministic rather than probabilistic. These
    simulations are much less computationally intensive than BayesACT
    simulations, and are easier to set up and run. However, they have
    fewer adjustable parameters and are less useful for some kinds of
    research questions.

-   [**actdata**](https://ahcombs.github.io/actdata/): actdata, which
    [I](https://aidancombs.netlify.app/) develop and maintain, is an R
    package that serves as a data repository. The ACT research community
    has a long and commendable history of making their tools and data
    publicly available. This package provides that data in a
    standardized format alongside functions that help users search,
    subset, and export it in a format that works for their analysis
    program of choice. It makes it unnecessary for researchers to store
    their own local copies of publicly available data, and greatly
    simplifies the process of comparing values across cultures or time
    periods. bayesactR and inteRact both use actdata to supply data for
    simulations.
