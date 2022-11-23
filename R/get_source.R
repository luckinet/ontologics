#' Get source(e) in an ontology
#'
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression, if \code{regex =
#'   TRUE}.
#' @param regex [`logical(1)`][logical]\cr whether or not the value in
#'   \code{...} shall be matched in full, or whether any partial match should be
#'   returned.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # exact sources from a loaded ontology ...
#' get_source(label = "harmonised", ontology = onto)
#'
#' # ... or one stored on the harddisc
#' get_source(version = "0.0.1", ontology = ontoDir)
#'
#' # get all sources
#' get_source(ontology = onto)
#' @return A table of the source(s) in the ontology according to the values in
#'   \code{...}
#' @importFrom checkmate assertLogical
#' @export

get_source <- function(..., regex = FALSE, ontology = NULL){

  assertLogical(x = regex, len = 1, any.missing = FALSE)

  if(!inherits(x = ontology, what = "onto")){
    assertFileExists(x = ontology, access = "r", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  toOut <- ontology@sources

  attrib <- quos(..., .named = TRUE)

  # identify attributes that are not in the ontology
  if(!all(names(attrib) %in% colnames(toOut))){
    sbst <- names(attrib) %in% colnames(toOut)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  if(regex){

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(as_name(attrib[[i]]), collapse = "|")))

    }

  } else {

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(toOut[[names(attrib)[i]]] %in% eval_tidy(attrib[[i]]))

    }

  }

  out <- toOut

  return(out)

}
