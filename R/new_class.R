#' Add a new valid class to an ontology
#'
#' @param class [`character(1)`][character]\cr the new class label.
#' @param broader [`character(1)`][character]\cr the broader class into which
#'   the new class is nested.
#' @param description [`character(1)`][character]\cr a verbatim description of
#'   the new class.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' onto <- new_class(class = "use type", broader = "class",
#'                   description = "something", ontology = onto)
#'
#' @importFrom checkmate assertCharacter assertClass assertTRUE
#' @importFrom methods new
#' @export

new_class <- function(class, broader, description, ontology = NULL){

  assertCharacter(x = class, len = 1, any.missing = FALSE)
  assertCharacter(x = broader, len = 1, unique = FALSE)
  assertCharacter(x = description, len = 1, any.missing = FALSE)
  assertTRUE(length(class) == length(broader))
  assertTRUE(length(class) == length(description))

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  if(is.na(broader)){

    theClass <- tibble(id = ontology@classes$id[1], class_label = class, has_broader = NA_character_, description = description) %>%
      bind_rows(ontology@classes, .) %>%
      distinct()

  } else {
    assertSubset(x = broader, choices = ontology@classes$class_label)
    newLvl <- ontology@classes %>%
      filter(class == !!broader) %>%
      pull(id)
    newLvl <- paste0(newLvl, ontology@classes$id[1])

    theClass <- tibble(id = newLvl, class_label = class, has_broader = broader, description = description) %>%
      bind_rows(ontology@classes, .) %>%
      distinct()
  }

  theClass <- theClass %>%
    filter(!is.na(class_label))

  out <- new(Class = "onto",
             classes = theClass,
             sources = ontology@sources,
             concepts = ontology@concepts,
             labels = ontology@labels,
             mappings = ontology@mappings)


  if(!is.null(ontoPath)){
    write_rds(x = out, file = ontoPath)
  }

  return(out)
}
