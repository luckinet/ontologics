#' Get class(es) in an ontology
#'
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression, if \code{regex =
#'   TRUE}.
#' @param regex [`logical(1)`][logical]\cr whether or not the value in
#'   \code{...} shall be matched in full, or whether any partial match should be
#'   returned.
#' @param external [`logical(1)`][logical]\cr whether or not the external
#'   classes (TRUE), or the harmonized classes should be returned (FALSE,
#'   default).
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # exact classes from a loaded ontology ...
#' get_class(label = "class", ontology = onto)
#'
#' # ... or one stored on the harddisc
#' get_class(id = ".xx.xx", ontology = ontoDir)
#'
#' # use regular expressions ...
#' get_class(label = "ro", regex = TRUE, ontology = onto)
#'
#' # get all sources
#' get_class(ontology = onto)
#' @return A table of the class(es) in the ontology according to the values in
#'   \code{...}
#' @importFrom checkmate assertLogical
#' @export

get_class <- function(..., regex = FALSE, external = FALSE, ontology = NULL){

  assertLogical(x = regex, len = 1, any.missing = FALSE)
  assertLogical(x = external, len = 1, any.missing = FALSE)

  if(!inherits(x = ontology, what = "onto")){
    assertFileExists(x = ontology, access = "r", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  if(external){
    toOut <- ontology@classes$external
    outCols <- c("label", "id", "description")
  } else {
    toOut <- ontology@classes$harmonised
    outCols <- c("label", "id", "has_broader", "description")
  }

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
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(eval_tidy(attrib[[i]]), collapse = "|")))

    }

  } else {

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(toOut[[names(attrib)[i]]] %in% eval_tidy(attrib[[i]]))

    }

  }

  out <- toOut %>%
    select(all_of(outCols), everything())

  return(out)

}
