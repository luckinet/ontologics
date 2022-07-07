devtools::load_all()
library(tibble)
library(dplyr, warn.conflicts = FALSE)

# cropOntology <- system.file("C:/Users/arue/Downloads/luckiOnto.rds", package = "ontologics")

crops <- load_ontology(path = "C:/Users/arner/Downloads/luckiOnto.rds")
crops@sources[3, "homepage"] <- "www.example.org/persistence#"
crops@sources[4, "homepage"] <- "www.example.org/life-form#"
crops@sources[5, "homepage"] <- "www.example.org/use-type#"

crops@classes$external[1, "id"] <- ".xx"
crops@classes$external[1, "label"] <- "ext Test"
crops@classes$external[1, "description"] <- "this is a test"
crops@classes$external[1, "has_source"] <- "3"

crops@classes$external[2, "id"] <- ".xx.xx"
crops@classes$external[2, "label"] <- "ext Test2"
crops@classes$external[2, "has_source"] <- "2"

crops@classes$harmonised[2, "has_narrower_match"] <- "ext Test.3 | ext Test2.2"
crops@classes$harmonised[3, "has_narrower_match"] <- "ext Test2"


export_as_rdf(ontology = crops, filename = "ex.ttl")