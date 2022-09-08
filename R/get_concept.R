#' Get a concept in an ontology
#'
#' @param x [`character(1)`][character]\cr a table containing all columns of the
#'   ontology that shall be filter by the values in those columns.
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression.
#' @param regex [`logical(1)`][logical]\cr whether or not the value in
#'   \code{...} shall be matched in full, or whether any partial match should be
#'   returned.
#' @param tree [`logical(1)`][logical]\cr whether or not to output the whole
#'   ontology tree starting from the given search terms.
#' @param na.rm [`logical(1)`][logical]\cr whether or not missing concepts are
#'   omitted or not.
#' @param mappings [`character(1)`][character]\cr the type of mappings within
#'   which to search for the concepts, possible values are \code{"none"},
#'   \code{"all"}, \code{"close"}, \code{"broader"}, \code{"narrower"},
#'   \code{"exact"} or any combination thereof.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # exact matches from a loaded ontology ...
#' get_concept(x = data.frame(label = "FODDER CROPS"), ontology = onto)
#'
#' # ... or one stored on the harddisc
#' get_concept(x = data.frame(label = "FODDER CROPS"), ontology = ontoDir)
#'
#' # use regular expressions ...
#' get_concept(x = data.frame(label = "/*crops"), regex = TRUE, ontology = onto)
#'
#' get_concept(x = data.frame(label = "/*crops"), has_broader = ".05", regex = TRUE, ontology = onto)
#'
#' # get all concepts that are nested into another concept
#' get_concept(x = data.frame(label = "FODDER CROPS"), tree = TRUE, ontology = onto)
#' @return A table of a subset of the ontology according to the values in
#'   \code{...}
#' @importFrom checkmate assertFileExists assertLogical testChoice
#' @importFrom tibble as_tibble
#' @importFrom readr read_rds
#' @importFrom tidyselect everything
#' @importFrom tidyr separate_rows separate pivot_longer pivot_wider
#' @importFrom rlang quos eval_tidy := sym as_name
#' @importFrom dplyr filter pull select rename inner_join
#' @importFrom purrr map map_dfc
#' @importFrom stringr str_which str_sub
#' @importFrom magrittr set_names
#' @importFrom utils head
#' @export

get_concept <- function(x = NULL, ..., regex = FALSE, tree = FALSE,
                        na.rm = FALSE, mappings = "none", ontology = NULL){

  assertDataFrame(x = x, null.ok = FALSE)
  assertLogical(x = regex, len = 1, any.missing = FALSE)
  assertLogical(x = tree, len = 1, any.missing = FALSE)
  assertLogical(x = na.rm, len = 1, any.missing = FALSE)
  assertSubset(x = mappings, choices = c("all", "none", "close", "broader", "narrower", "exact"))

  if("none" %in% mappings){
    mappings <- NULL
  } else {
    if("all" %in% mappings){
      mappings <- c("close", "broader", "narrower", "exact")
    }
    mappings <- paste0("has_", mappings, "_match")
  }

  if(!inherits(x = ontology, what = "onto")){
    assertFileExists(x = ontology, access = "r", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  theConcepts <- ontology@concepts

  attrib <- quos(..., .named = TRUE)
  # return(attrib)

  # identify attributes that are not in the ontology
  if(!all(names(attrib) %in% colnames(theConcepts$harmonised))){
    sbst <- names(attrib) %in% colnames(theConcepts$harmonised)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  if(regex){

    if(!is.null(x)){
      toOut <- ontology@concepts$harmonised %>%
        filter(str_detect(label, x$label))
    } else {
      toOut <- ontology@concepts$harmonised
    }

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(as_name(attrib[[i]]), collapse = "|")))

    }

  } else {

    assertNames(x = names(x), subset.of = c("id", "has_broader", "source_id", "class", "label", "source_label", "external_label"))

    # get the already harmonised concepts
    toOut <- x %>%
      left_join(theConcepts$harmonised, by = colnames(x))

    if(!is.null(mappings)){

      # and replace external IDs with the labels
      toOut <- toOut%>%
        pivot_longer(cols = c(has_close_match, has_broader_match, has_narrower_match, has_exact_match), names_to = "match", values_to = "extid") %>%
        separate_rows(extid, sep = " \\| ") %>%
        separate(col = extid, into = c("extid", "certainty"), sep = "[.]") %>%
        left_join(theConcepts$external %>% select(extid = id, external_label = label), by = "extid") %>%
        group_by(label, class, id, description, has_broader, match) %>%
        summarise(external_label = paste0(external_label, collapse = " | ")) %>%
        ungroup() %>%
        mutate(external_label = na_if(external_label, "NA")) %>%
        pivot_wider(id_cols = c(label, class, id, description, has_broader), names_from = match, values_from = external_label)

      toOutCompl <- toOut %>%
        filter(!is.na(id))

      matchCols <- colnames(x)[colnames(x) %in% colnames(theConcepts$external)]
      toMatch <- x %>%
        left_join(theConcepts$external, matchCols) %>%
        filter(!is.na(id)) %>%
        filter(!label %in% toOutCompl$label)

      if(dim(toMatch)[1] != 0){
        temp <- theConcepts$harmonised %>%
          pivot_longer(cols = mappings, names_to = "match", values_to = "extid") %>%
          filter(!is.na(extid)) %>%
          separate_rows(extid, sep = " \\| ") %>%
          separate(col = extid, into = c("extid", "certainty"), sep = "[.]") %>%
          left_join(toMatch %>% select(extid = id, external_label = label), by = "extid") %>%
          filter(!is.na(external_label))

        toOut <- toOut %>%
          filter(!label %in% temp$external_label)

        temp <- temp %>%
          group_by(id, label, class, description, has_broader, match) %>%
          summarise(external_label = paste0(external_label, collapse = " | ")) %>%
          ungroup() %>%
          pivot_wider(id_cols = c(id, label, class, description, has_broader), names_from = match, values_from = external_label)

        toOut <- toOut %>%
          bind_rows(temp) %>%
          arrange(id)
      }

    } else {
      toOut <- toOut %>%
        select(id, label, class, description, has_broader) %>%
        arrange(id)
    }

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(eval_tidy(attrib[[i]]), collapse = "|")))

    }

  }

  # if(missing){

    # out <- toOut %>%
      # filter(is.na(id))

  # } else {

    if(tree){

      topID <- toOut %>%
        pull(id) %>%
        unique()

      out <- make_tree(theConcepts$harmonised, topID)

    } else {

      if(na.rm){
        out <- toOut %>%
          filter(!is.na(id))
      } else {
        out <- toOut
      }
    }

  out <- out %>%
    select(label, class, id, has_broader, description, everything())

  # }

  return(out)

}
