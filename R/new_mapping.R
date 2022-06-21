#' Add a new mapping to an ontology
#'
#' Extend an ontology by creating mappings to external vocabularies.
#' @param new [`character(.)`][character]\cr the english label(s) of new
#'   external concepts that shall be mapped to concepts that do already exist in
#'   the ontology.
#' @param concept [`data.frame(.)`][data.frame]\cr the english label(s) of already
#'   harmonised concepts to which the external concepts shall be mapped.
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
#' @param ontology [`ontology(1)`][list]\cr either a path where the
#'   ontology is stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(name = "crops", path = ontoDir)
#'
#' mapping <- data.frame(old = c("BIOENERGY CROPS", "Bioenergy woody",
#'                               "Other bioenergy crops"),
#'                       new = c("bioenergy plants", "Wood plantation for fuel",
#'                               "Algae for bioenergy"),
#'                       type = c("close", "broad", "broad"))
#'
#' onto <- new_source(name = "externalDataset",
#'                    description = "a vocabulary",
#'                    homepage = "https://www.something.net",
#'                    license = "CC-BY-0",
#'                    ontology = onto)
#'
#' onto <- get_concept(terms = mapping$old, ontology = onto) %>%
#'   new_mapping(new = mapping$new,
#'               concept = .,
#'               match = mapping$type,
#'               source = "externalDataset",
#'               certainty = 3,
#'               ontology = onto)
#'
#' @return No return value, called for the side effect of adding new mappings to
#'   an ontology.
#' @importFrom checkmate testIntegerish testCharacter assert assertCharacter
#'   assertChoice assertIntegerish assertFileExists assertNames
#' @importFrom tibble tibble
#' @importFrom dplyr left_join filter pull mutate bind_rows arrange if_else
#' @importFrom tidyr unite
#' @importFrom stringr str_detect str_split
#' @importFrom readr read_rds write_rds
#' @importFrom methods new
#' @export

new_mapping <- function(new = NULL, concept, match = "close", source = NULL,
                        mappings = NULL, certainty = NULL, ontology = NULL){

  assertDataFrame(x = concept)
  assertNames(x = names(concept), must.include = c("code", "label_en", "class"))
  assertCharacter(x = new)
  assertNames(x = match, subset.of = c("close", "exact", "broad", "narrow", "related"))
  assertIntegerish(x = certainty, lower = 1, upper = 3)

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(name = theName, path = ontoPath)
  }

  onto <- ontology@concepts %>%
    left_join(ontology@labels, by = "code") %>%
    left_join(ontology@sources %>% select(sourceID, sourceName), by = "sourceID") %>%
    left_join(ontology@mappings, by = "code")

  testConcept <- concept %>%
    select(code, label_en, class) %>%
    left_join(onto, by = c("code", "label_en", "class"))

  if(any(is.na(testConcept$sourceID))){
    missingConcepts <- testConcept %>%
      filter(is.na(sourceID)) %>%
      pull(label_en)
    stop("the concepts '", paste0(missingConcepts, collapse = ", "), "' don't exist yet as harmonised concepts, please first define them (see function new_concept).")
  }


  if(length(match) != length(concept$code)){
    if(length(match) == 1){
      match <- rep(x = match, length.out = length(concept$code))
    } else {
      stop("the number of elements in 'match' is neither the same as in 'concept' nor 1.")
    }
  }

  if(length(certainty) != length(concept$code)){
    if(length(certainty) == 1){
      certainty <- rep(x = certainty, length.out = length(concept$code))
    } else {
      stop("the number of elements in 'certainty' is neither the same as in 'concept' nor 1.")
    }
  }

  srcID <- ontology@sources %>%
    filter(sourceName %in% source) %>%
    pull(sourceID)

  if(length(srcID) == 0){
    stop("please first define the source '", source, "' (see function new_source).")
  }

  prevID <- str_detect(string = onto$sourceName, pattern = source)
  if(!any(prevID)){
    prevID <- 0
  } else {
    prevID <- str_split(onto$code[prevID], pattern = "[.]", simplify = TRUE)
    prevID <- as.numeric(prevID[, dim(prevID)[2]])
    prevID <- max(prevID, na.rm = TRUE)
    if(is.na(prevID)) prevID <- 0
  }

  newConcept <- ontology@concepts
  newLabels <- ontology@labels
  newMappings <- ontology@mappings %>%
    mutate(new_code = NA_character_)
  iter <- 1
  for(i in seq_along(concept$code)){

    thisConcept <- concept[i,]

    # is the external concept new?
    codeIncl <- ontology@concepts %>%
      filter(sourceID %in% srcID)
    srcIncl <- ontology@labels %>%
      filter(code %in% codeIncl)
    srcNew <- !new[i] %in% srcIncl

    if(!srcNew){
      next
    }

    thisMatch <- match[i]
    thisCertainty <- certainty[i]

    prop <- toupper(substr(thisMatch, 1, 1))
    if(prop == "E") thisCertainty <- ""

    newID <- paste0(source, ".", prop, thisCertainty, ".", prevID + iter)

    newMappings <- newMappings %>%
      mutate(new_code = if_else(code %in% thisConcept$code,
                                if_else(!is.na(new_code), paste0(new_code, ", ", newID), newID),
                                if_else(!is.na(new_code), new_code, NA_character_)))

    iter <- iter + 1

    newConcept <- tibble(code = newID, sourceID = srcID) %>%
      bind_rows(newConcept)
    newLabels <- tibble(code = newID, class = NA_character_, label_en = new[i]) %>%
      bind_rows(newLabels)

  }

  newConcept <- newConcept %>%
    select(code, broader, sourceID) %>%
    arrange(code)
  newLabels <- newLabels %>%
    arrange(code)
  newMappings <- newMappings %>%
    unite(col = new, external, new_code, sep = ", ", na.rm = TRUE) %>%
    mutate(external = if_else(new == "", NA_character_, new)) %>%
    select(-new) %>%
    arrange(code)

  out <- new(Class = "onto",
             name = ontology@name,
             classes = ontology@classes,
             sources = ontology@sources,
             concepts = newConcept,
             labels = newLabels,
             mappings = newMappings)

  if(!is.null(ontoPath)){
    write_rds(x = out, file = ontoPath)
  }

  return(out)

}
