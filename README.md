
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ontologics <a href='https://github.com/luckinet/ontologics/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ontologics)](https://cran.r-project.org/package=ontologics)
[![R-CMD-check](https://github.com/luckinet/ontologics/workflows/R-CMD-check/badge.svg)](https://github.com/luckinet/ontologics/actions)
[![Coverage
Status](https://codecov.io/gh/luckinet/ontologics/branch/master/graph/badge.svg)](https://app.codecov.io/github/luckinet/ontologics?branch=master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/ontologics)](https://cran.r-project.org/package=geometr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The `ontologics` package provides tools to build and work with an
ontology of [linked (open)
data](https://en.wikipedia.org/wiki/Linked_data) in a tidy workflow. In
the current workflow the data are only available at the three star level
in comma-separated values tables, or actually an R-optimized \*.rds file
of such a table.

The type of ontology is inspired by the [FAO
caliper](https://www.fao.org/statistics/caliper/web/) platform and makes
use of the Simple Knowledge Organisation System
([SKOS](https://www.w3.org/TR/skos-reference/)).

An ontology is any data structure that stores concept of any knowledge
field in a linked way. It is typically built on a set of
standardized/harmonized terms that have a clear definition and
potentially some attributes. According to the SKOS, concepts can have
semantic relations or can be mapped to one another. Semantic relations
describe how *harmonized concepts* **relate** to one another and
mappings describe which concepts *from different vocabularies* should be
**linked** to one another.

The key tasks, beyond creating a formally valid ontology, is
*extracting* concepts and their relation to other concepts and *mapping*
new concepts to an existing ontology to capture and set potentially
deviating definitions into relation.

## Installation

Install the official version from CRAN:

``` r
install.packages("ontologics")
```

Install the latest development version from github:

``` r
devtools::install_github("luckinet/ontologics")
```

Read the vignettes [Create an
ontology](https://luckinet.github.io/ontologics/articles/create_an_ontology.html),
[Map new
concepts](https://luckinet.github.io/ontologics/articles/map_new_concepts.html)
and [Convert Ontology to
RDF](https://luckinet.github.io/ontologics/articles/conversion_to_rdf.html).

## Acknowledgement

This work was supported by funding to Carsten Meyer through the Flexpool
mechanism of the German Centre for Integrative Biodiversity Research
(iDiv) (FZT-118, DFG) and members working on it were additionally
supported by Freigeist funding of the Volkswagenstiftung and the BMBF
GeoKur project.
