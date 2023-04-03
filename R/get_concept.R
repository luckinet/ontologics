#' Get a concept in an ontology
#'
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression, if \code{regex =
#'   TRUE}.
#' @param external [`logical(1)`][logical]\cr whether or not to return merely
#'   the table of external concepts.
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
#' @importFrom rlang quos eval_tidy := sym as_name parse_expr
#' @importFrom dplyr filter pull select rename inner_join
#' @importFrom utils head
#' @export

get_concept <- function(..., external = FALSE, ontology = NULL){

  assertLogical(x = external, len = 1, any.missing = FALSE)

  if(!inherits(x = ontology, what = "onto")){
    assertFileExists(x = ontology, access = "r", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  if(external){
    toOut <- ontology@concepts$external
    outCols <- c("id", "label", "description")
  } else {
    toOut <- ontology@concepts$harmonised
    outCols <- c("id", "label", "description", "class", "has_broader")
  }

  attrib <- quos(...)
  # return(attrib)

  # identify attributes that are not in the ontology
  if(!all(names(attrib) %in% colnames(toOut)) & all(names(attrib) != "")){
    sbst <- names(attrib) %in% colnames(toOut)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  for(k in seq_along(attrib)){

    theName <- names(attrib)[k]

    if(theName == ""){

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
