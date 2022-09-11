#' Exports an ontology as RDF
#'
#' @param ontology [`ontology(1)`][list]\cr an already loaded
#' or created ontology object.
#' @param filename [`character(1)`][character]\cr the filename of
#' the exported ontology. The format of the exported ontology is
#' guessed by the extension of the filename. The guessing is performed
#' by the rdflib package. Valid extensions are ".rdf" for "rdfxml",
#' ".nt" for "ntriples", ".ttl" for "turtle" or ".json" for "jsonld".
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # export_as_rdf(ontology = onto, filename = "onto.ttl")
#' @importFrom checkmate assertCharacter
#' @importFrom stringr str_ends str_split
#' @importFrom utils URLencode
#' @importFrom dplyr na_if pull
#' @importFrom rdflib rdf rdf_add rdf_serialize rdf_free
#' @export

export_as_rdf <- function(ontology, filename) {

    assertCharacter(x = filename, len = 1, any.missing = FALSE)

    make_resource <- function(prefix, id) {
        return(URLencode(paste0(prefix, id), reserved = FALSE))
    }

    mapping_relations <- c(
        exactMatch = "has_exact_match",
        closeMatch = "has_close_match",
        broadMatch = "has_broader_match",
        narrowMatch = "has_narrower_match"
    )

    prefixes <- ontology@sources
    # check if each source contains a label and homepage,
    # if not: fill placeholders
    for (i in seq_len(nrow(prefixes))) {
        if (prefixes$label[i] == "") {
            prefixes$label[i] <- paste0("nosrc", i)
        }
        if (is.na(prefixes$homepage[i])) {
            prefixes$homepage[i] <- paste0("no-source-homepage-entered", i)
        }
    }

    for (i in seq_len(nrow(prefixes))) {
        # make sure labels are valid RDF prefixes (we just do this by URL encoding them)
        # TODO: don't use URLencode, but do it in some better way
        prefixes[i, "label"] <- URLencode(prefixes[i, "label"], reserved = FALSE)
        # check if string ends with '/' or '#'; if not concatenate '#'.
        if (!str_ends(prefixes[i, "homepage"], "(/|#)")) {
            prefixes[i, "homepage"] <- paste0(prefixes[i, "homepage"], "#")
        }
    }


    rdf <- rdf()

    # TODO: use labels of ontology$sources as shorthands for prefixes
    # build namespaces for rdf doc
    # namespaces <- do.call(rbind, list(prefixes$homepage))
    # colnames(namespaces) <- prefixes$label
    # namespaces <- enframe(namespaces)

    # print(namespaces)
    namespaces <- c(
        skos = "http://www.w3.org/2004/02/skos/core#",
        rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        rdfs = "http://www.w3.org/2000/01/rdf-schema#",
        owl = "http://www.w3.org/2002/07/owl#",
        xsd = "http://www.w3.org/2001/XMLSchema#",
        dct = "http://purl.org/dc/terms/"
    )


    # convert sources table
    for (i in seq_len(nrow(prefixes))) {
        rdf %>% rdf_add(
            subject = make_resource(prefixes[i, "homepage"], ""),
            predicate = make_resource(namespaces["rdf"], "type"),
            object = make_resource(namespaces["skos"], "ConceptScheme")
        )
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(prefixes[i, "label"], ""))) {
            rdf %>% rdf_add(
                subject = make_resource(prefixes[i, "homepage"], ""),
                predicate = make_resource(namespaces["skos"], "prefLabel"),
                object = prefixes[i, "label"],
                objectType = "literal"
            )
        }
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(prefixes[i, "description"], ""))) {
            rdf %>% rdf_add(
                subject = make_resource(prefixes[i, "homepage"], ""),
                predicate = make_resource(namespaces["skos"], "definition"),
                object = prefixes[i, "description"],
                objectType = "literal"
            )
        }
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(prefixes[i, "notes"], ""))) {
            rdf %>% rdf_add(
                subject = make_resource(prefixes[i, "homepage"], ""),
                predicate = make_resource(namespaces["skos"], "note"),
                object = prefixes[i, "notes"],
                objectType = "literal"
            )
        }
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(prefixes[i, "license"], ""))) {
            rdf %>% rdf_add(
                subject = make_resource(prefixes[i, "homepage"], ""),
                predicate = make_resource(namespaces["dct"], "license"),
                object = prefixes[i, "license"]
            )
        }
    }

    # currently both internal and external classes have no actual IDs
    # in the id row. For now I rewrite the id column with urlencoded
    # contents of the labels column.
    # TODO: adjust this when there are real IDs
    harmonised_classes <- ontology@classes$harmonised
    for (i in seq_len(nrow(harmonised_classes))) {
        harmonised_classes[i, "id"] <- URLencode(harmonised_classes[i, "label"], reserved = FALSE)
    }
    external_classes <- ontology@classes$external
    for (i in seq_len(nrow(external_classes))) {
        external_classes[i, "id"] <- URLencode(external_classes[i, "label"], reserved = FALSE)
    }

    # convert classes$harmonised table
    for (i in seq_len(nrow(harmonised_classes))) {
        prefix <- pull(prefixes[prefixes["label"] == "harmonised", "homepage"])
        sub <- make_resource(prefix, paste0("class#", harmonised_classes[i, "id"]))
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["rdf"], "type"),
            object = make_resource(namespaces["skos"], "Concept")
        )
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["rdf"], "type"),
            object = make_resource(namespaces["rdfs"], "Class")
        )
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["skos"], "inScheme"),
            object = make_resource(prefix, "")
        )
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(harmonised_classes[i, "label"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "prefLabel"),
                object = harmonised_classes[i, "label"],
                objectType = "literal"
            )
        }
        if (!is.na(na_if(harmonised_classes[i, "description"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "definition"),
                object = harmonised_classes[i, "description"],
                objectType = "literal"
            )
        }
        # semantic relations (skos:broader & skos:narrower)
        if (!is.na(na_if(harmonised_classes[i, "has_broader"], ""))) {
            broader <- paste0("class#", URLencode(harmonised_classes[i, "has_broader"], FALSE))
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "broader"),
                object = make_resource(prefix, broader),
                objectType = "uri"
            )
            rdf %>% rdf_add(
                subject = make_resource(prefix, broader),
                predicate = make_resource(namespaces["skos"], "narrower"),
                object = sub,
                objectType = "uri"
            )
        }
        # if has_broader == empty -> concept is top-concept of harmonised scheme
        else {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "topConceptOf"),
                object = make_resource(prefix, ""),
                objectType = "uri"
            )
            rdf %>% rdf_add(
                subject = make_resource(prefix, ""),
                predicate = make_resource(namespaces["skos"], "hasTopConcept"),
                object = sub,
                objectType = "uri"
            )
        }

        # skos mapping relations
        for (mapping_relation in names(mapping_relations)) {
            if (!is.na(na_if(harmonised_classes[i, mapping_relations[mapping_relation]], ""))) {
                mappings_certainty <- str_split(pull(ontology@classes$harmonised[i, mapping_relations[mapping_relation]]), pattern = " [|] ")
                for (mapping in mappings_certainty[[1]]) {
                    matched_class_id <- URLencode(str_split(mapping, pattern = "[.]")[[1]][1], FALSE)
                    matched_class_prefix <- pull(prefixes[prefixes["id"] == pull(external_classes[external_classes["id"] == matched_class_id, "has_source"]), "homepage"])
                    matched_class <- make_resource(matched_class_prefix, matched_class_id)
                    rdf %>% rdf_add(
                        subject = sub,
                        predicate = make_resource(namespaces["skos"], mapping_relation),
                        object = matched_class,
                        objectType = "uri"
                    )
                }
            }
        }
    }

    # convert classes$external table
    for (i in seq_len(nrow(external_classes))) {
        prefix <- pull(prefixes[prefixes["id"] == pull(external_classes[i, "has_source"]), "homepage"])
        sub <- make_resource(prefix, external_classes[i, "id"])
        # we don't explicitly type external resources as skos:Concept
        # rdf %>% rdf_add(
        #     subject = sub,
        #     predicate = make_resource(namespaces["rdf"], "type"),
        #     object = make_resource(namespaces["skos"], "Concept")
        # )
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["rdf"], "type"),
            object = make_resource(namespaces["rdfs"], "Class")
        )
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["skos"], "inScheme"),
            object = make_resource(prefix, "")
        )
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(external_classes[i, "label"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "prefLabel"),
                object = external_classes[i, "label"],
                objectType = "literal"
            )
        }
        if (!is.na(na_if(external_classes[i, "description"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "definition"),
                object = external_classes[i, "description"],
                objectType = "literal"
            )
        }
    }

    # convert concepts$harmonised table
    for (i in seq_len(nrow(ontology@concepts$harmonised))) {
        prefix <- pull(prefixes[prefixes["label"] == "harmonised", "homepage"])
        sub <- make_resource(prefix, paste0("concept#", ontology@concepts$harmonised[i, "id"]))
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["rdf"], "type"),
            object = make_resource(namespaces["skos"], "Concept")
        )
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["skos"], "inScheme"),
            object = make_resource(prefix, "")
        )
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(ontology@concepts$harmonised[i, "label"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "prefLabel"),
                object = ontology@concepts$harmonised[i, "label"],
                objectType = "literal"
            )
        }
        if (!is.na(na_if(ontology@concepts$harmonised[i, "description"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "definition"),
                object = ontology@concepts$harmonised[i, "description"],
                objectType = "literal"
            )
        }
        if (!is.na(na_if(ontology@concepts$harmonised[i, "class"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["rdf"], "type"),
                object = make_resource(prefix, paste0("class#", URLencode(ontology@concepts$harmonised[i, "class"], FALSE))),
                objectType = "uri"
            )
        }
        # semantic relations (skos:broader and skos:narrower)
        if (!is.na(na_if(ontology@concepts$harmonised[i, "has_broader"], ""))) {
            broader <- paste0("concept#", ontology@concepts$harmonised[i, "has_broader"])
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "broader"),
                object = make_resource(prefix, broader),
                objectType = "uri"
            )
            rdf %>% rdf_add(
                subject = make_resource(prefix, broader),
                predicate = make_resource(namespaces["skos"], "narrower"),
                object = sub,
                objectType = "uri"
            )
        }
        # if has_broader == empty -> concept is top-concept of harmonised scheme
        else {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "topConceptOf"),
                object = make_resource(prefix, ""),
                objectType = "uri"
            )
            rdf %>% rdf_add(
                subject = make_resource(prefix, ""),
                predicate = make_resource(namespaces["skos"], "hasTopConcept"),
                object = sub,
                objectType = "uri"
            )
        }
        # skos mapping relations
        for (mapping_relation in names(mapping_relations)) {
            if (!is.na(na_if(ontology@concepts$harmonised[i, mapping_relations[mapping_relation]], ""))) {
                mappings_certainty <- str_split(pull(ontology@concepts$harmonised[i, mapping_relations[mapping_relation]]), pattern = " [|] ")
                for (mapping in mappings_certainty[[1]]) {
                    matched_concept_id <- str_split(mapping, pattern = "[.]")[[1]][1]
                    # not possible to assign a certainty to a match in SKOS
                    # match_certainty <- str_split(mapping, pattern = "[.]")[[1]][2]
                    matched_concept_prefix <- pull(prefixes[prefixes["id"] == pull(ontology@concepts$external[ontology@concepts$external["id"] == matched_concept_id, "has_source"]), "homepage"])
                    matched_concept <- make_resource(matched_concept_prefix, matched_concept_id)
                    rdf %>% rdf_add(
                        subject = sub,
                        predicate = make_resource(namespaces["skos"], mapping_relation),
                        object = matched_concept,
                        objectType = "uri"
                    )
                }
            }
        }
    }

    # convert concepts$external table
    for (i in seq_len(nrow(ontology@concepts$external))) {
        prefix <- pull(prefixes[prefixes["id"] == pull(ontology@concepts$external[i, "has_source"]), "homepage"])
        sub <- make_resource(prefix, URLencode(ontology@concepts$external[i, "id"]))
        # we don't explicitly type external resources as skos:Concept
        # rdf %>% rdf_add(
        #     subject = sub,
        #     predicate = make_resource(namespaces["rdf"], "type"),
        #     object = make_resource(namespaces["skos"], "Concept")
        # )
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["skos"], "inScheme"),
            object = make_resource(prefix, "")
        )
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(ontology@concepts$external[i, "label"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "prefLabel"),
                object = ontology@concepts$external[i, "label"],
                objectType = "literal"
            )
        }
        if (!is.na(na_if(ontology@concepts$external[i, "description"], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "definition"),
                object = ontology@concepts$external[i, "description"],
                objectType = "literal"
            )
        }
    }

    rdf_serialize(rdf, filename, namespace = namespaces)
    rdf_free(rdf)
}
