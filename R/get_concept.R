#' Get a concept in an ontology
#'
#' @param ... combination of column name and value to filter that column by.
#' @param external [`logical(1)`][logical]\cr whether or not to return merely
#'   the table of external concepts.
#' @param matches [`logical(1)`][logical]\cr whether or not to include external
#'   concepts as label instead of id in the match columns of the harmonised
#'   concepts; this allows querying the external concepts in the harmonised
#'   concepts (only if \code{external = FALSE}).
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # exact matches from a loaded ontology ...
#' get_concept(label = "FODDER CROPS", ontology = onto)
#'
#' # ... or a path
#' get_concept(label = c("FODDER CROPS", "CEREALS"), ontology = ontoDir)
#'
#' # ignore querries that would not be valid in filter()
#' get_concept(label != 'Bioenergy woody' & has_broader == '.01', ontology = onto)
#'
#' # extract concepts based on regular expressions
#' library(stringr)
#' get_concept(str_detect(label, "crop") & str_detect(id, ".03$"), ontology = ontoDir)
#'
#' @return A table of a subset of the ontology according to the values in
#'   \code{...}
#' @importFrom checkmate assertLogical
#' @importFrom tidyselect everything contains
#' @importFrom tidyr separate_rows separate pivot_longer pivot_wider
#'   separate_longer_delim separate_wider_delim
#' @importFrom rlang quos eval_tidy := sym as_name parse_expr
#' @importFrom dplyr filter pull select rename inner_join
#' @importFrom utils head
#' @export

get_concept <- function(..., external = FALSE, matches = FALSE, ontology = NULL){

  assertLogical(x = external, len = 1, any.missing = FALSE)
  assertLogical(x = matches, len = 1, any.missing = FALSE)

  if(!inherits(x = ontology, what = "onto")){
    assertFileExists(x = ontology, access = "r", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  attrib <- quos(...)
  # return(attrib)

  if(external){
    toOut <- ontology@concepts$external
    outCols <- c("id", "label", "description")
  } else {
    toOut <- ontology@concepts$harmonised
    outCols <- c("id", "label", "description", "class", "has_broader")
  }

  # identify attributes that are not in the ontology
  if(!all(names(attrib) %in% colnames(toOut)) & all(names(attrib) != "")){
    sbst <- names(attrib) %in% colnames(toOut)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  # identify attributes that have no name. they will be evaluated last because they may have complex expressions to evaluate
  if(any(names(attrib) == "")){
    namewith <- which(names(attrib) != "")
    nameless <- which(names(attrib) == "")

    attrib <- c(attrib[namewith], attrib[nameless])
  }

  matched <- FALSE
  for(k in seq_along(attrib)){

    theName <- names(attrib)[k]

    if(theName == ""){

      if(matches & !matched){
        externalConcepts <- ontology@concepts$external %>%
          separate_wider_delim(cols = id, names = c("dataseries", "nr"), delim = "_", cols_remove = FALSE) %>%
          unite(col = label, label, dataseries, sep = "><") %>%
          select(extID = id, extLabel = label)

        toOut <- toOut %>%
          pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                       names_to = "match", values_to = "external") %>%
          separate_longer_delim(cols = external, delim = " \\| ") %>%
          separate_wider_delim(cols = external, names = "extID", delim = ".", too_many = "drop") %>%
          left_join(externalConcepts, by = "extID") %>%
          pivot_wider(id_cols = c("id", "label", "class", "has_broader", "description"), names_from = match,
                      values_from = extLabel, values_fn = ~paste0(na.omit(.x), collapse = " | ")) %>%
          mutate(across(where(is.character), ~na_if(x = ., y = "")))
        matched <- TRUE
      }

      toOut <- toOut %>%
        filter(eval_tidy(attrib[[k]], data = toOut))

    } else {

      toOut <- toOut %>%
        filter(toOut[[theName]] %in% eval_tidy(attrib[[k]]))

    }

  }

  out <- toOut %>%
    select(all_of(outCols), everything())

  return(out)

}
