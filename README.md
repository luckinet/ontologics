
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ontologics <a href='https://github.com/luckinet/ontologics/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/luckinet/ontologics/workflows/R-CMD-check/badge.svg)](https://github.com/luckinet/ontologics/actions)
[![Coverage
Status](https://codecov.io/gh/luckinet/ontologics/branch/master/graph/badge.svg)](https://codecov.io/github/luckinet/ontologics?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ontologics)](https://cran.r-project.org/package=ontologics)
[![](http://cranlogs.r-pkg.org/badges/grand-total/ontologics)](https://cran.r-project.org/package=geometr)

<!-- badges: end -->

## Overview

The `ontologics` package provides tools to build and work with an
ontology (linked vocabulary) in a tidy workflow.

And ontology is …

We currently use the FAO-caliper standard for ontologies (but other data
structures might be supported in the future), with the following setup …

The key tasks, beyond creating a formally valid ontology, is
*extracting* concepts and their relation to other concepts and *mapping*
new concepts to an existing ontology to capture and set potentially
deviating definitions into relation.

## Installation

1)  Install the official version from CRAN:

``` r
install.packages("ontologics")
```

or the latest development version from github:

``` r
devtools::install_github("luckinet/ontologics")
```

<!-- 2) The [vignette](https://ehrmanns.github.io/geometr/articles/geometr.html) gives an in depth introduction, explains the take on interoperability and discusses the spatial class `geom` that comes with `geometr`. -->
<!-- 3) Have fun being a [geometer](https://en.wikipedia.org/wiki/List_of_geometers)\! -->

## Examples

Create an ontology

``` r
# matches <- tibble(new = c(...),
#                   old = c(...))
```

Extract either the unique concepts …

``` r
# get_concept(label_en = matches$old)
```

… or a tree of concepts related to a focal concept

``` r
# get_tree()
```

Also, get those concepts that are missing from a set

``` r
# get_concept(label_en = matches$old, missing = TRUE)
```

Provide new concepts (that are so far unlinked)

``` r
# set_concept(new = c(),
#             broader = c(), # the labels 'new' should be nested into
#             class = , # try to keep that as conservative as possible and only come up with new classes, if really needed
#             source = thisDataset)
```

Map/link those new concepts to existing concepts

``` r
# set_mapping(concept = ...,
#             external = matches$new,
#             match = , # in most cases that should be "close", see ?newMapping
#             source = thisDataset,
#             certainty = ) # value from 1:3
```
