#' Start an ontology
#'
#' @param name [`character(1)`][character]\cr the path of the ontology.
#' @param path [`character(1)`][character]\cr the path where the ontology shall
#'   be stored.
#' @param description [`character(1)`][character]\cr
#' @param homepage [`character(1)`][character]\cr
#' @param license [`character(1)`][character]\cr
#' @param notes [`character(1)`][character]\cr
#' @param digits [`double(1)`][double]\cr the number of digits the code shall
#'   have.
#' @examples
#' start_ontology(name = "crops", path = tempdir())
#' @return it returns the new, emtpy ontology and also stores that within the
#'   directory specified in \code{path}.
#' @importFrom checkmate assertCharacter assertDirectoryExists
#' @importFrom methods new
#' @importFrom readr write_rds
#' @export

start_ontology <- function(name = NULL, path = NULL, description = NULL,
                           homepage = NULL, license = NULL, notes = NULL,
                           digits = 2){

  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertDirectoryExists(x = path, access = "rw")

  if(is.null(description)) description <- ""
  if(is.null(homepage)) homepage <- ""
  if(is.null(license)) license <- ""
  if(is.null(notes)) notes <- ""

  theName <- name
  theClasses <- tibble(level = double(),
                       class = character())
  theSources <- tibble(sourceID = 1,
                       sourceName = name,
                       description = description,
                       homepage = homepage,
                       license = license,
                       notes = NA_character_)
  theConcepts <- tibble(code = formatC(1, flag = "0", width = digits),
                        broader = NA_character_,
                        sourceID = 1)
  theLabels <- tibble(code = character(),
                      class = character(),
                      label_en = character())
  theMappings <- tibble(code = character(),
                        external = character())

  out <- new(Class = "onto",
             name = theName,
             classes = theClasses,
             sources = theSources,
             concepts = theConcepts,
             labels = theLabels,
             mappings = theMappings)

  write_rds(out, paste0(path, "/", theName, ".rds"))
  return(out)
}
