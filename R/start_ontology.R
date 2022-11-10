#' Start an ontology
#'
#' @param name [`character(1)`][character]\cr the path of the ontology.
#' @param version [`character(1)`][character]\cr the version of the ontology.
#' @param path [`character(1)`][character]\cr the path where the ontology shall
#'   be stored.
#' @param description [`character(1)`][character]\cr a brief description of the
#'   new ontology.
#' @param homepage [`character(1)`][character]\cr the URL to the homepage of the
#'   new ontology.
#' @param uri_prefix [`character(1)`][character]\cr the basic URL to construct
#'   URIs for all concepts.
#' @param license [`character(1)`][character]\cr any string describing the
#'   license under which this ontology can be (re)used.
#' @param notes [`character(1)`][character]\cr any notes that might apply to
#'   this ontology.
#' @param code [`double(1)`][double]\cr format of a single code snippet that is
#'   concatenated for nested levels.
#' @examples
#' start_ontology(name = "crops", path = tempdir())
#' @return it returns the new, empty ontology and also stores that within the
#'   directory specified in \code{path}.
#' @importFrom checkmate assertCharacter assertDirectoryExists
#' @importFrom httr http_error
#' @importFrom methods new
#' @importFrom readr write_rds
#' @export

start_ontology <- function(name = NULL, version = NULL, path = NULL, code = ".xx",
                           description = NULL, homepage = NULL, uri_prefix = NULL,
                           license = NULL, notes = NULL){

  assertDirectoryExists(x = path, access = "rw")

  if(is.null(description)) description <- ""
  if(is.null(homepage)) homepage <- ""
  if(!is.null(uri_prefix)) http_error(x = uri_prefix)
  if(is.null(license)) license <- ""
  if(is.null(notes)) notes <- ""

  theSources <- tibble(id = as.character(1),
                       label = "harmonised",
                       version = version,
                       date = Sys.Date(),
                       description = description,
                       homepage = homepage,
                       uri_prefix = uri_prefix,
                       license = license,
                       notes = notes)

  theClasses <- list(
    harmonised = tibble(id = code,
                        label = NA_character_,
                        description = "dummy class that contains the code definition.",
                        has_broader = NA_character_,
                        has_close_match = NA_character_,
                        has_narrower_match = NA_character_,
                        has_broader_match = NA_character_,
                        has_exact_match = NA_character_),
    external = tibble(id = character(),
                      label = character(),
                      description = character(),
                      has_source = character())
  )

  theConcepts <- list(
    harmonised = tibble(id = character(),
                        label = character(),
                        description = character(),
                        class = character(),
                        has_broader = character(),
                        has_close_match = character(),
                        has_narrower_match = character(),
                        has_broader_match = character(),
                        has_exact_match = character()),
    external = tibble(id = character(),
                      label = character(),
                      has_broader = character(),
                      description = character(),
                      has_source = character())
  )

  out <- new(Class = "onto",
             sources = theSources,
             classes = theClasses,
             concepts = theConcepts)

  write_rds(out, paste0(path, "/", name, ".rds"))
  return(out)
}
