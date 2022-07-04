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
#' @param ontology [`ontology(1)`][list]\cr either a path where the
#'   ontology is stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # exact matches from a loaded ontology ...
#' get_concept(x = data.frame(label_en = "FODDER CROPS"), ontology = onto)
#'
#' # ... or one stored on the harddisc
#' get_concept(x = data.frame(label_en = "FODDER CROPS"), ontology = ontoDir)
#'
#' # use regular expressions ...
#' get_concept(x = data.frame(label_en = "/*crops"), regex = TRUE, ontology = onto)
#'
#' get_concept(x = data.frame(label_en = "/*crops"), broader = ".05", regex = TRUE, ontology = onto)
#'
#' # get all concepts that are nested into another concept
#' get_concept(x = data.frame(label_en = "FODDER CROPS"), tree = TRUE, ontology = onto)
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
                        missing = FALSE, ontology = NULL){

  # documentation for x is missing

  assertDataFrame(x = x, null.ok = FALSE)
  assertLogical(x = regex, len = 1, any.missing = FALSE)
  assertLogical(x = tree, len = 1, any.missing = FALSE)
  assertLogical(x = missing, len = 1, any.missing = FALSE)
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

  onto <- ontology@concepts %>%
    left_join(ontology@labels, by = "id") %>%
    left_join(ontology@sources %>% select(source_id, source_label), by = "source_id") %>%
    left_join(ontology@mappings, by = "id")

  attrib <- quos(..., .named = TRUE)
  # return(attrib)

  # identify attributes that are not in the ontology
  if(!all(names(attrib) %in% colnames(onto))){
    sbst <- names(attrib) %in% colnames(onto)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  if(regex){

    if(!is.null(x)){
      toOut <- onto %>%
        filter(str_detect(label_en, x$label_en))
    } else {
      toOut <- onto
    }

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(as_name(attrib[[i]]), collapse = "|")))

    }

  } else {

    assertNames(x = names(x), subset.of = c("id", "has_broader", "source_id", "class", "label_en", "source_label", "external_id"))

    # extConcp <- tibble(label_en = x, external = x)
    # extConcp <- tibble(label_en = x)

    # tempOut <- onto %>%
    #   filter(label_en %in% x)

    tempOut <- x %>%
      left_join(onto, by = colnames(x))

    toOut <- tempOut %>%
      filter(!is.na(class))# %>%
      # select(-external) %>%
      # left_join(extConcp, by = "label_en") %>%
      # distinct()
    toMatch <- tempOut %>%
      filter(is.na(class)) %>%
      filter(!label_en %in% toOut$label_en)

    if(dim(toMatch)[1] != 0){
      toOut <- onto %>%
        filter(str_detect(external_id, paste0(toMatch$id, collapse = "|"))) %>%
        separate_rows(external_id, sep = ", ") %>%
        rename(extid = external_id) %>%
        left_join(toMatch %>% select(extid = id, external_id = label_en), by = "extid") %>%
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
        filter(source_label %in% c("harmonised", "imported")) %>%
        pull(id) %>%
        unique()

      temp <- make_tree(onto, topID)

    } else {
      temp <- toOut %>%
        filter(!is.na(id))
    }

  }

  out <- temp %>%
    select(id, has_broader, label_en, class, external_id, source_label)

  return(out)

}
