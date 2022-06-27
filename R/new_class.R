#' Add a new valid class to an ontology
#'
#' @param class [`character(1)`][character]\cr
#' @param parent [`character(1)`][character]\cr
#' @param ontology [`ontology(1)`][list]\cr either a path where the
#'   ontology is stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(name = "crops", path = ontoDir)
#'
#' onto <- new_class(class = "group", parent = NA, ontology = onto) %>%
#'         new_class(class = "class", parent = "group", ontology = .) %>%
#'         new_class(class = "crop", parent = "class", ontology = .)
#'
#' @importFrom checkmate assertCharacter assertClass assertTRUE
#' @importFrom methods new
#' @export

new_class <- function(class, parent, ontology = NULL){

  assertCharacter(x = class, len = 1, any.missing = FALSE)
  assertCharacter(x = parent, len = 1, unique = FALSE)
  assertTRUE(length(class) == length(parent))

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  newClass <- tibble(class = class, parent = parent)

  if(is.na(parent)){

    theClass <- tibble(level = ontology@classes$level[1], class = class)

  } else {
    assertSubset(x = parent, choices = ontology@classes$class)
    newLvl <- ontology@classes %>%
      filter(class == parent) %>%
      pull(level)
    newLvl <- paste0(newLvl, ontology@classes$level[1])

    theClass <- tibble(level = newLvl, class = class) %>%
      bind_rows(ontology@classes, .) %>%
      distinct()
  }

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
