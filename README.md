
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
(crops <- load_ontology(ontoDir = cropOntology))
#> # A tibble: 73 × 6
#>    code  broader label_en              class source     external
#>    <chr> <chr>   <chr>                 <chr> <chr>      <chr>   
#>  1 _01   <NA>    BIOENERGY CROPS       group harmonised <NA>    
#>  2 _0101 _01     Bioenergy herbaceous  class harmonised <NA>    
#>  3 _0102 _01     Bioenergy woody       class harmonised <NA>    
#>  4 _0103 _01     Other bioenergy crops class harmonised <NA>    
#>  5 _02   <NA>    CEREALS               group harmonised <NA>    
#>  6 _0201 _02     Barley                class harmonised <NA>    
#>  7 _0202 _02     Maize                 class harmonised <NA>    
#>  8 _0203 _02     Millets               class harmonised <NA>    
#>  9 _0204 _02     Oats                  class harmonised <NA>    
#> 10 _0205 _02     Other cereals         class harmonised <NA>    
#> # … with 63 more rows

# new concepts that occur in some dataset, which should be harmonised with the ontology
newConcepts <- c("Wheat", "NUTS", "Avocado")
```

The new concepts are from different conceptual levels, both ‘Wheat’ and
‘Avocado’ are the crop itself, while ‘NUTS’ is an aggregate of various
crops (such as walnut, hazelnut, etc). Let’s first find out, whether
these concepts are in fact new concepts because they are missing from
the ontology.

``` r
(missingConcepts <- get_concept(label_en = newConcepts, missing = TRUE, ontoDir = cropOntology))
#> [1] "Avocado"
```

This tells us that that both ‘NUTS’ and ‘Wheat’ don’t seem to be missing
from the ontology. We try to extract these concepts…

``` r
get_concept(label_en = newConcepts[1:2], ontoDir = cropOntology)
#> # A tibble: 2 × 6
#>   code  broader label_en class source     external
#>   <chr> <chr>   <chr>    <chr> <chr>      <chr>   
#> 1 _0209 _02     Wheat    class harmonised <NA>    
#> 2 _10   <NA>    NUTS     group harmonised <NA>
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
get_concept(code = c("_02"), tree = TRUE, ontoDir = cropOntology)
#> # A tibble: 10 × 6
#>    code  broader label_en      class source     external
#>    <chr> <chr>   <chr>         <chr> <chr>      <chr>   
#>  1 _02   <NA>    CEREALS       group harmonised <NA>    
#>  2 _0201 _02     Barley        class harmonised <NA>    
#>  3 _0202 _02     Maize         class harmonised <NA>    
#>  4 _0203 _02     Millets       class harmonised <NA>    
#>  5 _0204 _02     Oats          class harmonised <NA>    
#>  6 _0205 _02     Other cereals class harmonised <NA>    
#>  7 _0206 _02     Rice          class harmonised <NA>    
#>  8 _0207 _02     Rye           class harmonised <NA>    
#>  9 _0208 _02     Sorghum       class harmonised <NA>    
#> 10 _0209 _02     Wheat         class harmonised <NA>
get_concept(code = c("_10"), tree = TRUE, ontoDir = cropOntology)
#> # A tibble: 3 × 6
#>   code  broader label_en   class source     external
#>   <chr> <chr>   <chr>      <chr> <chr>      <chr>   
#> 1 _10   <NA>    NUTS       group harmonised <NA>    
#> 2 _1001 _10     Treenuts   class harmonised <NA>    
#> 3 _1002 _10     Other nuts class harmonised <NA>
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
new_concept(new = c("wheat", "avocado"),
            broader = c("Wheat", "Tropical and subtropical Fruit"),
            class = "crop",                                         
            source = "external_dataset", 
            ontoDir = cropOntology)
```

Eventually, all new concepts can be mapped to already harmonised
concepts. Even though ‘NUTS’ already exists, this also applies to this
new concept, because the already existing concept ‘NUTS’ doesn’t
necessarily have to be the same as the new concept ‘NUTS’. This all
depends on the respective definitions. When setting a new mapping, the
type and the certainty of the match have to be defined. For ‘wheat’ this
is a *close* match, because the concepts are very related. for ‘NUTS’
this is, after checking the theoretical definitions also a *close*
match. ‘Avocado’

``` r
set_mapping(concept = c("wheat", "NUTS", "avocado"),
            external = newConcepts,
            match = c("close", "close", "broad"), 
            source = "external_dataset",
            certainty = 3, 
            ontoDir = cropOntology)
```

Now we can check whether the updated ontology is as we’d expect, for
example by looking at the tree of the respective items again. We should
expect that the new harmonised concepts now appear in the ontology and
that they have some link to an external concept defined.

``` r
get_concept(label_en = "Wheat", tree = TRUE, ontoDir = cropOntology)
#> # A tibble: 2 × 6
#>   code    broader label_en class source     external             
#>   <chr>   <chr>   <chr>    <chr> <chr>      <chr>                
#> 1 _0209   _02     Wheat    class harmonised <NA>                 
#> 2 _020901 _0209   wheat    crop  imported   external_dataset_C3_1
get_concept(label_en = "NUTS", tree = TRUE, ontoDir = cropOntology)
#> # A tibble: 3 × 6
#>   code  broader label_en   class source     external             
#>   <chr> <chr>   <chr>      <chr> <chr>      <chr>                
#> 1 _10   <NA>    NUTS       group harmonised external_dataset_C3_2
#> 2 _1001 _10     Treenuts   class harmonised <NA>                 
#> 3 _1002 _10     Other nuts class harmonised <NA>
get_concept(label_en = "FRUIT", tree = TRUE, ontoDir = cropOntology)
#> # A tibble: 9 × 6
#>   code    broader label_en                       class source     external      
#>   <chr>   <chr>   <chr>                          <chr> <chr>      <chr>         
#> 1 _06     <NA>    FRUIT                          group harmonised <NA>          
#> 2 _0601   _06     Berries                        class harmonised <NA>          
#> 3 _0602   _06     Citrus Fruit                   class harmonised <NA>          
#> 4 _0603   _06     Grapes                         class harmonised <NA>          
#> 5 _0604   _06     Pome Fruit                     class harmonised <NA>          
#> 6 _0605   _06     Stone Fruit                    class harmonised <NA>          
#> 7 _0606   _06     Tropical and subtropical Fruit class harmonised <NA>          
#> 8 _060601 _0606   avocado                        crop  imported   external_data…
#> 9 _0607   _06     Other fruit                    class harmonised <NA>
```

We can, moreover check whether the external concepts are properly
defined

``` r
crops_updated <- read_rds(file = cropOntology)
crops_updated$attributes %>% 
  arrange(source)
#> # A tibble: 78 × 4
#>    code                  source           label_en              class
#>    <chr>                 <chr>            <chr>                 <chr>
#>  1 external_dataset_B3_3 external_dataset Avocado               <NA> 
#>  2 external_dataset_C3_1 external_dataset Wheat                 <NA> 
#>  3 external_dataset_C3_2 external_dataset NUTS                  <NA> 
#>  4 _01                   harmonised       BIOENERGY CROPS       group
#>  5 _0101                 harmonised       Bioenergy herbaceous  class
#>  6 _0102                 harmonised       Bioenergy woody       class
#>  7 _0103                 harmonised       Other bioenergy crops class
#>  8 _02                   harmonised       CEREALS               group
#>  9 _0201                 harmonised       Barley                class
#> 10 _0202                 harmonised       Maize                 class
#> # … with 68 more rows
```
