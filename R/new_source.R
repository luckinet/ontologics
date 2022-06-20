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
#' @importFrom dplyr bind_rows
#' @importFrom methods new
#' @export

new_source <- function(name = NULL, description = NULL, homepage = NULL, license = NULL, notes = NULL, ontology = NULL){

  assertCharacter(x = name, len = 1)
  assertCharacter(x = description, len = 1)
  assertCharacter(x = homepage, len = 1)
  assertCharacter(x = license, len = 1)
  assertCharacter(x = notes, len = 1, null.ok = TRUE)
  assertClass(x = ontology, classes = "onto")

  if(length(ontology@sources$sourceID) == 0){
    newID <- 1
  } else {
    newID <- ontology@sources$sourceID + 1
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
             name = theName,
             classes = ontology@classes,
             sources = theSources,
             concepts = ontology@concepts,
             labels = ontology@labels,
             mappings = ontology@mappings)

  return(out)
}
