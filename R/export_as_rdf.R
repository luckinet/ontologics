#' Export an ontology as RDF
#'
#' @param ontology [`ontology(1)`][list]\cr an already loaded or created
#'   ontology object.
#' @param filename [`character(1)`][character]\cr the filename of the exported
#'   ontology. The format of the exported ontology is guessed by the extension
#'   of the filename. The guessing is performed by the rdflib package. Valid
#'   extensions are ".rdf" for "rdfxml", ".nt" for "ntriples", ".ttl" for
#'   "turtle" or ".json" for "jsonld".
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' \dontrun{
#'
#'   export_as_rdf(ontology = onto, filename = "onto.ttl")
#' }
#' @return No return value, called for the side effect of exporting an ontology.
#' @importFrom checkmate assertCharacter
#' @importFrom stringr str_ends str_split str_replace_all
#' @importFrom readr read_file write_file
#' @importFrom utils URLencode
#' @importFrom dplyr na_if pull
#' @importFrom rdflib rdf rdf_add rdf_serialize rdf_free
#' @export
#'

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
    sources <- ontology@sources
    # if harmonised onto does not have `uri_prefix`, set to www.example.org/ontologics
    if(is.na(sources[sources$label == "harmonised", "uri_prefix"])){
        sources[sources$label == "harmonised", "uri_prefix"] <- "http://www.example.org/ontologics"
    }
    # if harmonised onto has version add after prefix
    if (!is.na(sources[sources$label == "harmonised", "version"])) {
        version <- sources[sources$label == "harmonised", "version"]
        prefix <- sources[sources$label == "harmonised", "uri_prefix"]
        if (str_ends(prefix, "/")) {
            prefix <- paste0(prefix, version, "/")
        } else if (str_ends(prefix, "#")) {
            prefix <- paste0(prefix, version, "#")
        } else {
            prefix <- paste0(prefix, "/", version, "/")
        }
        sources[sources$label == "harmonised", "uri_prefix"] <- prefix
    }

    # exclude sources that dont have a uri_prefix
    exclude_sources <- list()
    for (i in seq_len(nrow(sources))) {
        if (is.na(sources$uri_prefix[i])) {
            exclude_sources <- append(exclude_sources, sources$id[i])
        }
    }


    for (i in seq_len(nrow(sources))) {
        # make sure labels are valid RDF sources (we just do this by URL encoding them)
        # TODO: don't use URLencode, but do it in some better way
        sources[i, "label"] <- URLencode(sources[i, "label"], reserved = FALSE)
        # check if existing uri_prefixes end with '/' or '#'; if not concatenate '#'.
        if(!is.na(sources[i, "uri_prefix"])) {
            if (str_ends(sources[i, "uri_prefix"], "(/|#)", negate=TRUE)) {
                sources[i, "uri_prefix"] <- paste0(sources[i, "uri_prefix"], "#")
            }
        }
    }


    rdf <- rdf()

    # TODO: use labels of ontology$sources as shorthands for sources
    # build namespaces for rdf doc
    # namespaces <- do.call(rbind, list(sources$uri_prefix))
    # colnames(namespaces) <- sources$label
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
    for (i in seq_len(nrow(sources))) {
        if(!is.na(sources[i, "uri_prefix"])){
            rdf %>% rdf_add(
                subject = make_resource(sources[i, "uri_prefix"], ""),
                predicate = make_resource(namespaces["rdf"], "type"),
                object = make_resource(namespaces["skos"], "ConceptScheme")
            )
            # ignore if Obj == NULL or ""
            if (!is.na(na_if(sources[[i, "label"]], ""))) {
                rdf %>% rdf_add(
                    subject = make_resource(sources[i, "uri_prefix"], ""),
                    predicate = make_resource(namespaces["skos"], "prefLabel"),
                    object = paste0(sources[i, "label"], "@en"),
                    objectType = "literal"
                )
            }
            # ignore if Obj == NULL or ""
            if (!is.na(na_if(sources[[i, "description"]], ""))) {
                rdf %>% rdf_add(
                    subject = make_resource(sources[i, "uri_prefix"], ""),
                    predicate = make_resource(namespaces["skos"], "definition"),
                    object = paste0(sources[i, "description"], "@en"),
                    objectType = "literal"
                )
            }
            # ignore if Obj == NULL or ""
            if (!is.na(na_if(sources[[i, "notes"]], ""))) {
                rdf %>% rdf_add(
                    subject = make_resource(sources[i, "uri_prefix"], ""),
                    predicate = make_resource(namespaces["skos"], "note"),
                    object = paste0(sources[i, "notes"], "@en"),
                    objectType = "literal"
                )
            }
            # ignore if Obj == NULL or ""
            if (!is.na(na_if(sources[[i, "license"]], ""))) {
                rdf %>% rdf_add(
                    subject = make_resource(sources[i, "uri_prefix"], ""),
                    predicate = make_resource(namespaces["dct"], "license"),
                    object = sources[i, "license"]
                )
            }
        }
    }

    # add creation date to harmonised source if available
    # if (!is.na(sources[sources$label == "harmonised", "date"])) {
    #     rdf %>% rdf_add(
    #                 subject = make_resource(sources[sources$label == "harmonised", "uri_prefix"], ""),
    #                 predicate = make_resource(namespaces["dct"], "created"),
    #                 object = paste0(
    #                     sources[sources$label == "harmonised", "date"],
    #                     "^^xsd:date"
    #                 )
    #             )
    # }

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
        prefix <- pull(sources[sources["label"] == "harmonised", "uri_prefix"])
        sub <- make_resource(prefix, paste0("class-", harmonised_classes[i, "id"]))
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
            object = make_resource(prefix, ""),
            objectType = "uri"
        )
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(harmonised_classes[[i, "label"]], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "prefLabel"),
                object = paste0(harmonised_classes[i, "label"], "@en"),
                objectType = "literal"
            )
        }
        if (!is.na(na_if(harmonised_classes[[i, "description"]], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "definition"),
                object = paste0(harmonised_classes[i, "description"], "@en"),
                objectType = "literal"
            )
        }
        # semantic relations (skos:broader & skos:narrower)
        if (!is.na(na_if(harmonised_classes[[i, "has_broader"]], ""))) {
            broader <- paste0("class-", URLencode(harmonised_classes[i, "has_broader"], FALSE))
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
            if (!is.na(na_if(harmonised_classes[[i, mapping_relations[mapping_relation]]], ""))) {
                mappings_certainty <- str_split(pull(ontology@classes$harmonised[i, mapping_relations[mapping_relation]]), pattern = " [|] ")
                for (mapping in mappings_certainty[[1]]) {
                    matched_class_id <- URLencode(str_split(mapping, pattern = "[.]")[[1]][1], FALSE)
                    # check if the source of the matched_class_id has a uri_prefix
                    source_id = pull(external_classes[external_classes["id"] == matched_class_id, "has_source"])
                    if (!(source_id %in% exclude_sources)) {
                        matched_class_prefix <- pull(sources[sources["id"] == source_id, "uri_prefix"])
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
    }

    # convert classes$external table
    for (i in seq_len(nrow(external_classes))) {
        prefix <- pull(sources[sources["id"] == pull(external_classes[i, "has_source"]), "uri_prefix"])
        if(!is.na(prefix)){
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
                object = make_resource(prefix, ""),
                objectType = "uri"
            )
            # ignore if Obj == NULL or ""
            if (!is.na(na_if(external_classes[[i, "label"]], ""))) {
                rdf %>% rdf_add(
                    subject = sub,
                    predicate = make_resource(namespaces["skos"], "prefLabel"),
                    object = paste0(external_classes[i, "label"], "@en"),
                    objectType = "literal"
                )
            }
            if (!is.na(na_if(external_classes[[i, "description"]], ""))) {
                rdf %>% rdf_add(
                    subject = sub,
                    predicate = make_resource(namespaces["skos"], "definition"),
                    object = paste0(external_classes[i, "description"], "@en"),
                    objectType = "literal"
                )
            }
        }

    }

    # convert concepts$harmonised table
    for (i in seq_len(nrow(ontology@concepts$harmonised))) {
        prefix <- pull(sources[sources["label"] == "harmonised", "uri_prefix"])
        sub <- make_resource(prefix, paste0("concept-", ontology@concepts$harmonised[i, "id"]))
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["rdf"], "type"),
            object = make_resource(namespaces["skos"], "Concept")
        )
        rdf %>% rdf_add(
            subject = sub,
            predicate = make_resource(namespaces["skos"], "inScheme"),
            object = make_resource(prefix, ""),
            objectType = "uri"
        )
        # ignore if Obj == NULL or ""
        if (!is.na(na_if(ontology@concepts$harmonised[[i, "label"]], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "prefLabel"),
                object = paste0(ontology@concepts$harmonised[i, "label"], "@en"),
                objectType = "literal"
            )
        }
        if (!is.na(na_if(ontology@concepts$harmonised[[i, "description"]], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "definition"),
                object = paste0(ontology@concepts$harmonised[i, "description"], "@en"),
                objectType = "literal"
            )
        }
        if (!is.na(na_if(ontology@concepts$harmonised[[i, "class"]], ""))) {
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["rdf"], "type"),
                object = make_resource(prefix, paste0("class-", URLencode(ontology@concepts$harmonised[i, "class"], FALSE))),
                objectType = "uri"
            )
        }
        # semantic relations (skos:broader and skos:narrower)
        if (!is.na(na_if(ontology@concepts$harmonised[[i, "has_broader"]], ""))) {
            broader <- paste0("concept-", ontology@concepts$harmonised[i, "has_broader"])
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
            if (!is.na(na_if(ontology@concepts$harmonised[[i, mapping_relations[mapping_relation]]], ""))) {
                mappings_certainty <- str_split(pull(ontology@concepts$harmonised[i, mapping_relations[mapping_relation]]), pattern = " [|] ")
                for (mapping in mappings_certainty[[1]]) {
                    matched_concept_id <- str_split(mapping, pattern = "[.]")[[1]][1]
                    # not possible to assign a certainty to a match in SKOS
                    # match_certainty <- str_split(mapping, pattern = "[.]")[[1]][2]
                    source_id <- pull(ontology@concepts$external[ontology@concepts$external["id"] == matched_concept_id, "has_source"])
                    matched_concept_external_id <- pull(ontology@concepts$external[ontology@concepts$external["id"] == matched_concept_id, "label"])
                    if (!(source_id %in% exclude_sources)) {
                        matched_concept_prefix <- pull(sources[sources["id"] == source_id, "uri_prefix"])
                        matched_concept <- make_resource(matched_concept_prefix, matched_concept_external_id)
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
    }

    # convert concepts$external table
    for (i in seq_len(nrow(ontology@concepts$external))) {
        prefix <- pull(sources[sources["id"] == pull(ontology@concepts$external[i, "has_source"]), "uri_prefix"])
        # exclude concept with no uri_prefix
        if(!is.na(prefix)) {
            sub <- make_resource(prefix, URLencode(ontology@concepts$external[i, "label"]))
            # we don't explicitly type external resources as skos:Concept
            # rdf %>% rdf_add(
            #     subject = sub,
            #     predicate = make_resource(namespaces["rdf"], "type"),
            #     object = make_resource(namespaces["skos"], "Concept")
            # )
            rdf %>% rdf_add(
                subject = sub,
                predicate = make_resource(namespaces["skos"], "inScheme"),
                object = make_resource(prefix, ""),
                objectType = "uri"
            )
            # ignore if Obj == NULL or ""
            # the label col contains the external concept id, whcih is used to build the `sub`.
            # -> effectively there is no label, currently.
            # if (!is.na(na_if(ontology@concepts$external[[i, "label"]], ""))) {
            #     rdf %>% rdf_add(
            #         subject = sub,
            #         predicate = make_resource(namespaces["skos"], "prefLabel"),
            #         object = paste0(ontology@concepts$external[i, "label"], "@en"),
            #         objectType = "literal"
            #     )
            # }
            if (!is.na(na_if(ontology@concepts$external[[i, "description"]], ""))) {
                rdf %>% rdf_add(
                    subject = sub,
                    predicate = make_resource(namespaces["skos"], "definition"),
                    object = paste0(ontology@concepts$external[i, "description"], "@en"),
                    objectType = "literal"
                )
            }
        }
    }

    rdf_serialize(rdf, filename, namespace = namespaces)
    rdf_free(rdf)
    rdfstring <- read_file(filename)
    rdfstring <- str_replace_all(rdfstring, "@en\"", "\"@en")
    # rdfstring <- str_replace_all(rdfstring, "^^xsd:date\"", "\"^^xsd:date")
    write_file(rdfstring, filename)
}
