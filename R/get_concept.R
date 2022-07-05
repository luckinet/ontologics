#' Get a concept in an ontology
#'
#' @param x [`character(1)`][character]\cr
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression.
#' @param regex [`logical(1)`][logical]\cr whether or not the value in
#'   \code{...} shall be matched in full, or whether any partial match should be
#'   returned.
#' @param tree [`logical(1)`][logical]\cr whether or not to output the whole
#'   ontology tree starting from the given search terms.
#' @param missing [`logical(1)`][logical]\cr whether or not to give only those
#'   values that are currently missing from the ontology.
#' @param mappings [`logical(1)`][logical]\cr whether or not to give the
#'   concepts including mappings to external concepts.
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
#' @importFrom tidyr separate_rows
#' @importFrom rlang quos eval_tidy := sym as_name
#' @importFrom dplyr filter pull select rename inner_join
#' @importFrom purrr map map_dfc
#' @importFrom stringr str_which str_sub
#' @importFrom magrittr set_names
#' @importFrom utils head
#' @export

get_concept <- function(x = NULL, ..., regex = FALSE, tree = FALSE,
                        missing = FALSE, mappings = FALSE, ontology = NULL){

  # documentation for x is missing

  assertDataFrame(x = x, null.ok = FALSE)
  assertLogical(x = regex, len = 1, any.missing = FALSE)
  assertLogical(x = tree, len = 1, any.missing = FALSE)
  assertLogical(x = missing, len = 1, any.missing = FALSE)
  assertLogical(x = mappings, len = 1, any.missing = FALSE)
  if(regex & missing){
    stop("you can only search for missing items with 'regex = FALSE'.")
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

    assertNames(x = names(x), subset.of = c("id", "has_broader", "source_id", "class", "label", "source_label", "external_id"))

    toOut <- x %>%
      left_join(theConcepts$harmonised, by = colnames(x))

    toMatch <- x %>%
      left_join(theConcepts$external, by = colnames(x)) %>%
      filter(!is.na(id))

    if(dim(toMatch)[1] != 0){
      toOut <- theConcepts$harmonised %>%
        filter(str_detect(has_close_match, paste0(toMatch$id, collapse = "|"))) %>%
        separate_rows(has_close_match, sep = ", ") %>%
        rename(extid = has_close_match) %>%
        left_join(toMatch %>% select(extid = id, external_id = label), by = "extid") %>%
        filter(!is.na(external_id)) %>%
        select(-extid) %>%
        bind_rows(toOut, .)
    }

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(eval_tidy(attrib[[i]]), collapse = "|")))

    }

  }

  if(missing){

    temp <- toOut %>%
      filter(is.na(id))

  } else {

    if(tree){

      topID <- toOut %>%
        pull(id) %>%
        unique()

      temp <- make_tree(theConcepts$harmonised, topID)

    } else {
      temp <- toOut %>%
        filter(!is.na(id))
    }

  }

  if(mappings){
    out <- temp
  } else {
    out <- temp %>%
      select(id, has_broader, label, class)
  }

  return(out)

}
