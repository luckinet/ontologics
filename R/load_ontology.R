#' Load an ontology
#'
#' @param path [`character(1)`][character]\cr the path where the ontology to
#'   load is stored.
#' @examples
#' # load an already existing ontology
#' load_ontology(path = system.file("extdata", "crops.rds", package = "ontologics"))
#'
#' @return A table of the full ontology (i.e., where attribute and mapping
#'   tables are joined).
#' @importFrom checkmate assertFileExists assertCharacter
#' @importFrom dplyr left_join filter select everything rowwise group_by
#'   distinct ungroup
#' @importFrom readr read_csv read_rds
#' @export

load_ontology <- function(path = NULL){

  assertFileExists(x = path, access = "r", extension = "rds")

  temp <- read_rds(path)

  if(inherits(x = temp, what = "onto")) {

    out <- temp

  } else {

    # # ask user for the columns in 'temp' that are required to load the ontology
    # if(is.data.frame(x = temp)){
    #
    # } else if(is.list(temp)){
    #
    # }

    # the default (for now) is a list with two tables ('attributes' and 'mappings')
    theClasses <- temp$attributes %>%
      rowwise() %>%
      mutate(level = if_else(nchar(code) == 3, 1, 2)) %>%
      group_by(class) %>%
      distinct(level) %>%
      ungroup() %>%
      select(level, class)

    theSources <- temp$attributes %>%
      mutate(sourceID = seq_along(unique(source)),
             sourceName = source,
             description = NA_character_,
             homepage = NA_character_,
             license = NA_character_,
             notes = "imported manually") %>%
      distinct(sourceID, sourceName, description, homepage, license, notes)

    theConcepts <- temp$mappings %>%
      mutate(sourceID = 1) %>%
      select(code, broader, sourceID)

    theLabels <- temp$mappings %>%
      select(code, class, label_en)

    theMappings <- temp$mappings %>%
      mutate(external = NA_character_) %>%
      select(code, external)

    out <- new(Class = "onto",
               classes = theClasses,
               sources = theSources,
               concepts = theConcepts,
               labels = theLabels,
               mappings = theMappings)
  }

  return(out)

}
