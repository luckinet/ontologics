#' Add a new valid source to an ontology
#'
#' @param name [`character(1)`][character]\cr
#' @param description [`character(1)`][character]\cr
#' @param homepage [`character(1)`][character]\cr
#' @param license [`character(1)`][character]\cr
#' @param notes [`character(1)`][character]\cr
#' @param ontology [`ontology(1)`][list]\cr either a path where the
#'   ontology is stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(name = "crops", path = ontoDir)
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

    ontology <- load_ontology(name = theName, path = ontoPath)
  }

  if(name %in% ontology@sources$sourceName){
    warning("the source '", name, "' has already been registered.")
  }

  if(length(ontology@sources$sourceID) == 0){
    newID <- 1
  } else {
    newID <- max(ontology@sources$sourceID) + 1
  }

  if(str_detect(name, "_")){
    stop("please provide a name that doesn't contain '_' symbols.")
  } else {
    theName <- name
  }

  newSource <- tibble(sourceID = newID,
                      sourceName = name,
                      description = description,
                      homepage = homepage,
                      license = license,
                      notes = notes)

  theSources <- bind_rows(ontology@sources, newSource)

  out <- new(Class = "onto",
             name = ontology@name,
             classes = ontology@classes,
             sources = theSources,
             concepts = ontology@concepts,
             labels = ontology@labels,
             mappings = ontology@mappings)

  if(!is.null(ontoPath)){
    write_rds(x = out, file = ontoPath)
  }

  return(out)
}
