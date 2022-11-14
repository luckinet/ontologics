#' Add a new valid class to an ontology
#'
#' @param new [`character(1)`][character]\cr the new class label.
#' @param target [`character(1)`][character]\cr the class to which the new class
#'   shall be related.
#' @param description [`character(1)`][character]\cr a verbatim description of
#'   the new class.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' onto <- new_class(new = "use type", target = "class", description = "something",
#'                   ontology = onto)
#'
#' @return the updated ontology that contains the new class(es) defined here.
#' @importFrom checkmate assertCharacter assertClass assertTRUE
#' @importFrom methods new
#' @export

new_class <- function(new, target, description = NULL, ontology = NULL){

  assertCharacter(x = new, len = 1, any.missing = FALSE)

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  theClasses <- ontology@classes

  assertCharacter(x = target, len = 1, unique = FALSE)
  assertCharacter(x = description, len = 1, any.missing = FALSE)
  assertTRUE(length(new) == length(target))
  assertTRUE(length(new) == length(description))

  if(is.na(target)){

    temp <- tibble(id = theClasses$harmonised$id[1],
                   label = new,
                   description = description,
                   has_broader = NA_character_,
                   has_close_match = NA_character_,
                   has_narrower_match = NA_character_,
                   has_broader_match = NA_character_,
                   has_exact_match = NA_character_) %>%
      bind_rows(theClasses$harmonised) %>%
      filter(description != "dummy class that contains the code definition.") %>%
      distinct()

    theClasses$harmonised <- temp
  } else {

    assertSubset(x = target, choices = theClasses$harmonised$label)
    newLvl <- theClasses$harmonised %>%
      filter(label == !!target) %>%
      pull(id)
    newLvl <- paste0(newLvl, theClasses$harmonised$id[1])

    temp <- tibble(id = newLvl,
                   label = new,
                   description = description,
                   has_broader = target,
                   has_close_match = NA_character_,
                   has_narrower_match = NA_character_,
                   has_broader_match = NA_character_,
                   has_exact_match = NA_character_) %>%
      bind_rows(theClasses$harmonised, .) %>%
      distinct()

    theClasses$harmonised <- temp
  }

  out <- new(Class = "onto",
             sources = ontology@sources,
             classes = theClasses,
             concepts = ontology@concepts)


  if(!is.null(ontoPath)){
    write_rds(x = out, file = ontoPath)
  }

  return(out)
}
