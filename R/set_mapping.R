#' Add a new mapping to an ontology
#'
#' Extend an ontology by creating mappings to external vocabularies.
#' @param concept [`character(.)`][character]\cr the english label(s) of already
#'   harmonised concepts to which the external concepts shall be mapped.
#' @param external [`character(.)`][character]\cr the english label(s) of new
#'   external concepts that shall be mapped to concepts that do already exist in
#'   the ontology.
#' @param match [`character(1)`][character]\cr the
#'   \href{https://www.w3.org/TR/skos-reference/#mapping}{skos mapping property}
#'   used to describe the link, possible values are \code{"close"},
#'   \code{"exact"}, \code{"broad"}, \code{"narrow"} and \code{"related"}.
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concept.
#' @param mappings [`tibble()`][tibble]\cr not yet implemented.
#' @param certainty [`integerish(1)`][integer]\cr the certainty of the match.
#'   Possible values are between 1 and 4, with meaning \itemize{\item 1 =
#'   probably unreliable \item 2 = unclear, assigned according to a given
#'   definition \item 3 = clear, assigned according to a given definition \item
#'   4 = original, harmonised term (can't be assigned by a user)}.
#' @param path [`character(1)`][character]\cr the path where the ontology in
#'   which to search is stored. It can be omitted in case the option "onto_path"
#'   has been define (see \code{getOption("onto_path")}).
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#'
#' set_mapping(concept = c("BIOENERGY CROPS", "Bioenergy woody", "Other bioenergy crops"),
#'             external = c("bioenergy plants", "Wood plantation for fuel", "Algae for bioenergy"),
#'             match = c("close", "broad", "broad"),
#'             source = "external_dataset",
#'             certainty = 3,
#'             path = ontoDir)
#'
#' load_ontology(path = ontoDir)
#' @return No return value, called for the side effect of adding new mappings to
#'   an ontology.
#' @importFrom checkmate testIntegerish testCharacter assert assertCharacter
#'   assertChoice assertIntegerish assertFileExists assertNames
#' @importFrom tibble tibble
#' @importFrom dplyr left_join filter pull mutate bind_rows arrange if_else
#' @importFrom tidyr unite
#' @importFrom stringr str_detect str_split
#' @importFrom readr read_rds write_rds
#' @export

set_mapping <- function(concept, external = NULL, match = "close", source = NULL,
                        mappings = NULL, certainty = NULL, path = NULL){

  isInt <- testIntegerish(x = concept)
  isChar <- testCharacter(x = concept)
  assert(isInt, isChar)
  assertCharacter(x = external)
  assertNames(x = match, subset.of = c("close", "exact", "broad", "narrow", "related"))
  assertIntegerish(x = certainty, lower = 1, upper = 3)

  if(!is.null(path)){
    assertFileExists(x = path, access = "rw", extension = "rds")
  } else {
    path <- getOption("onto_path")
  }

  ontology <- read_rds(file = path)

  if(length(match) != length(concept)){
    if(length(match) == 1){
      match <- rep(x = match, length.out = length(concept))
    } else {
      stop("the number of elements in 'match' is neither the same as in 'concept' nor 1.")
    }
  }

  if(length(certainty) != length(concept)){
    if(length(certainty) == 1){
      certainty <- rep(x = certainty, length.out = length(concept))
    } else {
      stop("the number of elements in 'certainty' is neither the same as in 'concept' nor 1.")
    }
  }

  prevID <- str_detect(string = ontology$attributes$code, pattern = source)
  if(!any(prevID)){
    prevID <- 0
  } else {
    prevID <- as.numeric(str_split(ontology$attributes$code[prevID], pattern = "_", simplify = TRUE)[,3])
    prevID <- max(prevID, na.rm = TRUE)
    if(is.na(prevID)) prevID <- 0
  }

  # newIDs <- NULL
  tempMappings <- ontology$mappings %>%
    mutate(new_code = NA_character_)
  iter <- 1
  for(i in seq_along(concept)){

    thisConcept <- concept[i]

    if(!is.na(thisConcept)){

      # is the external concept new?
      srcRows <- str_detect(ontology$attributes$code, source)
      srcIncluded <- ontology$attributes %>%
        filter(srcRows) %>%
        pull(label_en)
      srcNew <- !external[i] %in% srcIncluded

      if(!srcNew){
        next
      }

      num <- suppressWarnings(as.numeric(thisConcept))
      if(is.na(num)){
        thisConcept <- tibble(label_en = thisConcept) %>%
          left_join(ontology$mappings, by = "label_en") %>%
          pull(code)
        assertCharacter(x = thisConcept, len = 1, any.missing = FALSE)
      }

      thisMatch <- match[i]
      thisCertainty <- certainty[i]

      prop <- toupper(substr(thisMatch, 1, 1))
      if(prop == "E") thisCertainty <- ""

      newID <- paste0(source, "_", prop, thisCertainty, "_", prevID + iter)

      tempMappings <- tempMappings %>%
        mutate(new_code = if_else(code %in% thisConcept,
                                  if_else(!is.na(new_code), paste0(new_code, ", ", newID), newID),
                                  if_else(!is.na(new_code), new_code, NA_character_)))

      iter <- iter + 1

      newConcept <- tibble(code = newID, source = source, label_en = external[i])
      ontology$attributes <- ontology$attributes %>%
        bind_rows(newConcept)

    }

  }
  ontology$attributes <- ontology$attributes %>%
    arrange(code, source)

  ontology$mappings <- tempMappings %>%
    unite(col = external, external, new_code, sep = ", ", na.rm = TRUE) %>%
    mutate(external = if_else(external == "", NA_character_, external))

  write_rds(x = ontology, file = path)

}
