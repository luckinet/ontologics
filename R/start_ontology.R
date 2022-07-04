#' Start an ontology
#'
#' @param name [`character(1)`][character]\cr the path of the ontology.
#' @param path [`character(1)`][character]\cr the path where the ontology shall
#'   be stored.
#' @param description [`character(1)`][character]\cr
#' @param homepage [`character(1)`][character]\cr
#' @param license [`character(1)`][character]\cr
#' @param notes [`character(1)`][character]\cr
#' @param code [`double(1)`][double]\cr format of a single code snippet that is
#'   concatenated for nested levels.
#' @examples
#' start_ontology(name = "crops", path = tempdir())
#' @return it returns the new, emtpy ontology and also stores that within the
#'   directory specified in \code{path}.
#' @importFrom checkmate assertCharacter assertDirectoryExists
#' @importFrom methods new
#' @importFrom readr write_rds
#' @export

start_ontology <- function(name = NULL, path = NULL, code = ".xx", description = NULL,
                           homepage = NULL, license = NULL, notes = NULL){

  assertDirectoryExists(x = path, access = "rw")

  if(is.null(description)) description <- ""
  if(is.null(homepage)) homepage <- ""
  if(is.null(license)) license <- ""
  if(is.null(notes)) notes <- ""

  theClasses <- tibble(id = code,
                       class_label = NA_character_,
                       has_broader = NA_character_,
                       description = "dummy class that contains the code definition.")
  theSources <- tibble(source_id = 1,
                       source_label = "harmonised",
                       description = description,
                       homepage = homepage,
                       license = license,
                       notes = notes)
  theConcepts <- tibble(id = character(),
                        has_broader = character(),
                        source_id = double())
  theLabels <- tibble(id = character(),
                      class = character(),
                      label_en = character(),
                      description = character())
  theMappings <- tibble(id = character(),
                        external_id = character())

  out <- new(Class = "onto",
             classes = theClasses,
             sources = theSources,
             concepts = theConcepts,
             labels = theLabels,
             mappings = theMappings)

  write_rds(out, paste0(path, "/", name, ".rds"))
  return(out)
}
