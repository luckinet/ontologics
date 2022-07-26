#' Add a new valid source to an ontology
#'
#' @param name [`character(1)`][character]\cr the name of the new source (should
#'   not contain empty spaces).
#' @param description [`character(1)`][character]\cr a verbatim description of
#'   the new source.
#' @param homepage [`character(1)`][character]\cr the homepage of the new source
#'   (this could be the basis of the concept URIs).
#' @param license [`character(1)`][character]\cr the licenses under which the
#'   new source is published.
#' @param notes [`character(1)`][character]\cr any notes on the new source that
#'   don't fit into any of the other meta-data fields here.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' onto <- new_source(name = "externalDataset",
#'                    description = "a vocabulary",
#'                    homepage = "https://www.something.net",
#'                    license = "CC-BY-0",
#'                    ontology = onto)
#' @importFrom checkmate assertCharacter assertClass
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows distinct
#' @importFrom methods new
#' @export

new_source <- function(name = NULL, description = NULL, homepage = NULL,
                       license = NULL, notes = NULL, ontology = NULL){

  assertCharacter(x = name, len = 1)
  assertCharacter(x = description, len = 1, null.ok = TRUE)
  assertCharacter(x = homepage, len = 1, null.ok = TRUE)
  assertCharacter(x = license, len = 1, null.ok = TRUE)
  assertCharacter(x = notes, len = 1, null.ok = TRUE)

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  if(name %in% ontology@sources$label){
    warning("the source '", name, "' has already been registered.", call. = FALSE)
    return(NULL)
  }

  if(length(ontology@sources$id) == 0){
    newID <- 1
  } else {
    newID <- max(as.numeric(ontology@sources$id)) + 1
  }
  newID <- as.character(newID)

  if(str_detect(name, "_")){
    stop("please provide a name that doesn't contain '_' symbols.")
  } else {
    theName <- name
  }

  newSource <- tibble(id = newID,
                      label = name,
                      description = description,
                      homepage = homepage,
                      license = license,
                      notes = notes)

  theSources <- bind_rows(ontology@sources, newSource)

  out <- new(Class = "onto",
             sources = theSources,
             classes = ontology@classes,
             concepts = ontology@concepts)

  if(!is.null(ontoPath)){
    write_rds(x = out, file = ontoPath)
  }

  return(out)
}
