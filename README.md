
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
ontology of [linked (open)
data](https://en.wikipedia.org/wiki/Linked_data) in a tidy workflow. In
the current workflow the data are only available at the three star level
in comma-separated values tables, or actually an R-optimised \*.rds file
of such a table.

The type of ontology is inspired by the [FAO
caliper](https://datalab.review.fao.org/datalab/caliper/web/) platform
and makes use of the Simple Knowledge Organisation System
([SKOS](https://www.w3.org/TR/skos-reference/)).

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

## Create an ontology

An ontology is any data structure that stores concept of any knowledge
field in a linked way. It is typically built on a set of
standardised/harmonised terms that have a clear definition and
potentially some attributes. According to the SKOS, concepts can have
semantic relations or can be mapped to one another. Semantic relations
describe how *harmonised concepts* **relate** to one another and
mappings describe which concepts *from different vocabularies* should be
**linked** to one another.

``` r
library(ontologics)

# work in process

# for now, load an internal ontology
cropOntology <- system.file("extdata", "crops.rds", package = "ontologics")
```

## Build an ontology by adding incoming concepts

When an ontology exists already, it can be used either by looking up
which concepts exist in it, which relations the concepts have among each
other or by adding and linking new concepts to the already existing
concepts.

``` r
# already existing ontology for some project about crops
(crops <- load_ontology(name = "crops", path = cropOntology))

# where we have to set the external dataset as new source
crops <- new_source(name = "externalDataset",
                    description = "a vocabulary",
                    homepage = "https://www.something.net",
                    license = "CC-BY-0",
                    ontology = crops)

# new concepts that occur in the external dataset, which should be harmonised with the ontology
newConcepts <- c("Wheat", "NUTS", "Avocado")
```

The new concepts are from different conceptual levels, both ‘Wheat’ and
‘Avocado’ are the crop itself, while ‘NUTS’ is an aggregate of various
crops (such as walnut, hazelnut, etc). Let’s first find out whether
these concepts are in fact new concepts because they are missing from
the ontology.

``` r
(missingConcepts <- get_concept(terms = newConcepts, missing = TRUE, ontology = crops))
#> # A tibble: 1 × 6
#>   code  broader label_en class external sourceName
#>   <chr> <chr>   <chr>    <chr> <chr>    <chr>     
#> 1 <NA>  <NA>    <NA>     <NA>  Avocado  <NA>
```

This tells us that that both ‘NUTS’ and ‘Wheat’ don’t seem to be missing
from the ontology. We try to extract these concepts…

``` r
get_concept(terms = newConcepts[1:2], ontology = crops)
#> # A tibble: 2 × 6
#>   code   broader label_en class external sourceName
#>   <chr>  <chr>   <chr>    <chr> <chr>    <chr>     
#> 1 .02.09 .02     Wheat    class Wheat    harmonised
#> 2 .10    <NA>    NUTS     group NUTS     harmonised
```

… and see that ‘Wheat’ is a *class* and not a *crop* and ‘NUTS’ doesn’t
have any *broader* concept. We should probably check the hierarchical
structure of the ontology for these two concepts as well. For ‘NUTS’ we
can ask directly for the respective *code* and for ‘Wheat’ we would ask
for the *broader* code. We see already here that the code has the same
length, so ‘NUTS’ and whatever the parent of ‘Wheat’ are should be at
the same hierarchical level.

``` r
# note that this time we are not querring 'label_en' but 'code'
get_concept(terms = newConcepts[1], tree = TRUE, ontology = crops)
#> # A tibble: 1 × 6
#>   code   broader label_en class external sourceName
#>   <chr>  <chr>   <chr>    <chr> <chr>    <chr>     
#> 1 .02.09 .02     Wheat    class <NA>     harmonised
get_concept(terms = newConcepts[2], tree = TRUE, ontology = crops)
#> # A tibble: 3 × 6
#>   code   broader label_en   class external sourceName
#>   <chr>  <chr>   <chr>      <chr> <chr>    <chr>     
#> 1 .10    <NA>    NUTS       group <NA>     harmonised
#> 2 .10.01 .10     Treenuts   class <NA>     harmonised
#> 3 .10.02 .10     Other nuts class <NA>     harmonised
```

It seems that ‘NUTS’ is not missing from the ontology, and has two child
concepts. ‘Wheat’ is also not missing, but is defined as the wrong
concept of type *class*, hence the crop ‘Wheat’ is in fact missing.
‘Avocado’ is also missing and has to be specified as new harmonised
concept together with ‘Wheat’.

By studying the ontology (as above), we can identify the semantic
relation between the new concepts and the already harmonised concepts,
in other words, into which already existing concepts the new harmonised
concepts shall be nested. For the new harmonised concepts, we chose the
lower capital letter words to show the difference between those and the
external concepts.

``` r
broaderConcepts <- get_concept(terms = c("Wheat", "Tropical and subtropical Fruit"), 
                               ontology = crops)

crops <- new_concept(new = c("wheat", "avocado"),
                     broader = broaderConcepts,
                     class = "crop",                                         
                     source = "externalDataset", 
                     ontology = crops)
```

Eventually, all new concepts can be mapped to already harmonised
concepts. Even though ‘NUTS’ already exists, this also applies to this
new concept, because the already existing concept ‘NUTS’ doesn’t
necessarily have to be the same as the new concept ‘NUTS’ (*note: yes,
this is a deliberate example, to make this exact point!*). This all
depends on the respective definitions. When setting a new mapping, the
type and the certainty of the match have to be defined. For ‘wheat’ this
is a *close* match, because the concepts are very related. for ‘NUTS’
this is, after checking the theoretical definitions also a *close*
match. ‘Avocado’ is nested into ‘Tropical Fruit’ and thus has the match
type *broad* (because Tropical Fruit is broader than Avocado).

``` r
toMap <- get_concept(terms = c("wheat", "NUTS", "avocado"), 
                     ontology = crops)

crops <- new_mapping(concept = toMap,
                     new = newConcepts,
                     match = c("close", "close", "broad"), 
                     source = "externalDataset",
                     certainty = 3, 
                     ontology = crops)
```

Now we can check whether the updated ontology is as we’d expect, for
example by looking at the tree of the respective items again. We should
expect that the new harmonised concepts now appear in the ontology and
that they have some link to an external concept defined.

``` r
get_concept(terms = "Wheat", tree = TRUE, ontology = crops)
#> # A tibble: 2 × 6
#>   code     broader label_en class external               sourceName     
#>   <chr>    <chr>   <chr>    <chr> <chr>                  <chr>          
#> 1 .02.09   .02     Wheat    class <NA>                   harmonised     
#> 2 .02.0901 .02.09  wheat    crop  externalDataset.C3.902 externalDataset
get_concept(terms = "NUTS", tree = TRUE, ontology = crops)
#> # A tibble: 3 × 6
#>   code   broader label_en   class external               sourceName
#>   <chr>  <chr>   <chr>      <chr> <chr>                  <chr>     
#> 1 .10    <NA>    NUTS       group externalDataset.C3.903 harmonised
#> 2 .10.01 .10     Treenuts   class <NA>                   harmonised
#> 3 .10.02 .10     Other nuts class <NA>                   harmonised
get_concept(terms = "FRUIT", tree = TRUE, ontology = crops)
#> # A tibble: 9 × 6
#>   code     broader label_en                       class external      sourceName
#>   <chr>    <chr>   <chr>                          <chr> <chr>         <chr>     
#> 1 .06      <NA>    FRUIT                          group <NA>          harmonised
#> 2 .06.01   .06     Berries                        class <NA>          harmonised
#> 3 .06.02   .06     Citrus Fruit                   class <NA>          harmonised
#> 4 .06.03   .06     Grapes                         class <NA>          harmonised
#> 5 .06.04   .06     Pome Fruit                     class <NA>          harmonised
#> 6 .06.05   .06     Stone Fruit                    class <NA>          harmonised
#> 7 .06.06   .06     Tropical and subtropical Fruit class <NA>          harmonised
#> 8 .06.0601 .06.06  avocado                        crop  externalData… externalD…
#> 9 .06.07   .06     Other fruit                    class <NA>          harmonised
```
