#' Add a new valid source to an ontology
#'
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology into which the new source should be
#'   included.
#' @param name [`character(1)`][character]\cr the name of the new source (must
#'   not contain empty spaces).
#' @param version [`character(1)`][character]\cr an optional version of the new
#'   source (any value is allowed, but should be a value that follows
#'   \href{https://semver.org/}{semantic versioning}). Either version or date
#'   need to be given.
#' @param date [`character(1)`][character]\cr an optional date at which that
#'   version of an external vocabulary has been created. Should be a value of
#'   the form YYYY-MM-DD. Either version or date need to be given.
#' @param description [`character(1)`][character]\cr a verbatim description of
#'   the new source.
#' @param homepage [`character(1)`][character]\cr the homepage of the new
#'   source, typically the place where additional information or meta-data could
#'   be retrieved in a non-formalised way.
#' @param uri_prefix [`character(1)`][character]\cr the basic uniform resource
#'   locator (URL) all concepts of a new source have in common and which is thus
#'   the basis to construct the concept specific URI.
#' @param license [`character(1)`][character]\cr the licenses under which the
#'   new source is published.
#' @param notes [`character(1)`][character]\cr any notes on the new source that
#'   don't fit into any of the other meta-data fields here.
#' @details Fundamentally, there are two types of sources that can be defined
#'   with this function. \itemize{\item \emph{attribute collections}: where a
#'   collection of terms or concepts are associated as a descriptive attribute
#'   to the harmonised concepts, and \item \emph{linked open data}: where the
#'   concepts that occur in another vocabulary or ontology and which are
#'   themselves part of linked datasets (and hence have a valid URI) are
#'   associated as related concepts to the harmonised concepts.} In the latter
#'   case, each mapped concept should be provided by its ID and the source needs
#'   to have a URL that allows in combination with the concept IDs to construct
#'   the URI under which the mapped concepts are stored in the semantic web.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' onto <- new_source(name = "externalDataset",
#'                    version = "0.0.1",
#'                    description = "a vocabulary",
#'                    homepage = "https://www.something.net",
#'                    license = "CC-BY-0",
#'                    ontology = onto)
#' @return the updated ontology that contains the new source defined here.
#' @importFrom checkmate assert assertCharacter assertClass testCharacter
#'   testDate
#' @importFrom httr http_error
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows distinct
#' @importFrom methods new
#' @export

new_source <- function(ontology = NULL, name = NULL, version = NULL, date = NULL,
                       description = NULL, homepage = NULL, uri_prefix = NULL,
                       license = NULL, notes = NULL){

  assertCharacter(x = name, len = 1)
  isVersion <- testCharacter(version, len = 1)
  isDate <- testDate(date, len = 1)
  assert(isDate, isVersion)
  assertCharacter(x = description, len = 1, null.ok = TRUE)
  assertCharacter(x = homepage, len = 1, null.ok = TRUE)
  assertCharacter(x = uri_prefix, len = 1, null.ok = TRUE)
  assertCharacter(x = license, len = 1, null.ok = TRUE)
  assertCharacter(x = notes, len = 1, null.ok = TRUE)

  if(!is.null(uri_prefix)) http_error(x = uri_prefix)

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
    return(ontology)
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
                      version = version,
                      date = date,
                      description = description,
                      homepage = homepage,
                      uri_prefix = uri_prefix,
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
