#' Get a concept in an ontology
#'
#' @param terms [`character(1)`][character]\cr
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression.
#' @param regex [`logical(1)`][logical]\cr whether or not the value in
#'   \code{...} shall be matched in full, or whether any partial match should be
#'   returned.
#' @param tree [`logical(1)`][logical]\cr whether or not to output the whole
#'   ontology tree starting from the given search terms.
#' @param missing [`logical(1)`][logical]\cr whether or not to give only those
#'   values that are currently missing from the ontology.
#' @param path [`character(1)`][character]\cr the path where the ontology in
#'   which to search is stored. It can be omitted in case the option "onto_path"
#'   has been define (see \code{getOption("onto_path")}).
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#'
#' # exact matches
#' get_concept(terms = "FODDER CROPS", path = ontoDir)
#'
#' # use regular expressions ...
#' get_concept(terms = "/*crops", regex = TRUE, path = ontoDir)
#' get_concept(terms = "/*crops", broader = "_05", regex = TRUE, path = ontoDir)
#'
#' # get all concepts that are nested into another concept
#' get_concept(terms = "FODDER CROPS", tree = TRUE, path = ontoDir)
#' @return A table of a subset of the ontology according to the values in
#'   \code{...}
#' @importFrom checkmate assertFileExists assertLogical testChoice
#' @importFrom tibble as_tibble
#' @importFrom readr read_rds
#' @importFrom tidyselect everything
#' @importFrom tidyr separate_rows
#' @importFrom rlang quos eval_tidy := sym
#' @importFrom dplyr filter pull select rename
#' @importFrom purrr map map_dfc
#' @importFrom stringr str_which str_sub
#' @importFrom magrittr set_names
#' @export

get_concept <- function(terms = NULL, ..., regex = FALSE, tree = FALSE, missing = FALSE,
                        path = NULL){

  if(!is.null(path)){
    assertFileExists(x = path, access = "rw", extension = "rds")
  } else {
    path <- getOption("onto_path")
  }

  assertCharacter(x = terms, null.ok = TRUE)
  assertLogical(x = regex, len = 1, any.missing = FALSE)
  assertLogical(x = tree, len = 1, any.missing = FALSE)
  assertLogical(x = missing, len = 1, any.missing = FALSE)
  if(regex & missing){
    stop("you can only search for missing items with 'regex = FALSE'.")
  }

  ontology <- read_rds(file = path)
  onto <- left_join(ontology$attributes,
                    ontology$mappings %>% select(-label_en, -class), by = "code") %>%
    select(code, label_en, class, everything())

  attrib <- quos(..., .named = TRUE)
  # return(attrib)
#
#   if(length(attrib) == 0){
#     return(onto)
#   }

  # identify attributes that are not in the ontology
  if(!all(names(attrib) %in% colnames(onto))){
    sbst <- names(attrib) %in% colnames(onto)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  if(regex){

    if(!is.null(terms)){
      toOut <- onto %>%
        filter(str_detect(label_en, terms))
    } else {
      toOut <- onto
    }

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(attrib[[i]], collapse = "|")))

    }

  } else {

    extConcp <- tibble(label_en = terms, external = terms)

    tempOut <- onto %>%
      filter(label_en %in% terms)

    toOut <- tempOut %>%
      filter(source %in% c("harmonised", "imported")) %>%
      select(-external) %>%
      left_join(extConcp, by = "label_en")
    toMatch <- tempOut %>%
      filter(!source %in% c("harmonised", "imported"))

    if(dim(toMatch)[1] != 0){
      toOut <- onto %>%
        filter(str_detect(external, paste0(toMatch$code, collapse = "|"))) %>%
        separate_rows(external, sep = ", ") %>%
        rename(extCode = external) %>%
        left_join(toMatch %>% select(extCode = code, external = label_en), by = "extCode") %>%
        filter(!is.na(external)) %>%
        select(-extCode) %>%
        bind_rows(toOut, .)
    }

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(attrib[[i]], collapse = "|")))

    }

    toOut <- extConcp %>%
      select(external) %>%
      left_join(toOut, by = "external")

  }

  if(missing){

    temp <- toOut %>%
      filter(is.na(code))

  } else {

    if(tree){

      topID <- toOut %>%
        filter(source %in% c("harmonised", "imported")) %>%
        pull(code) %>%
        unique()

      temp <- make_tree(onto, topID)

    } else {
      temp <- toOut %>%
        filter(!is.na(code))
    }

  }

  out <- temp %>%
    select(code, broader, label_en, class, external, source)

  return(out)

}
