#' Add a new concept to an ontology
#'
#' This adds a new concept to an existing ontology to semantically integrate and
#' thus harmonise it with the already existing ontology.
#' @param new [`character(.)`][character]\cr the english label(s) of new
#'   concepts that shall be included in the ontology.
#' @param broader [`data.frame(.)`][data.frame]\cr the english label(s) of already
#'   harmonised concepts to which the new concept shall be semantically linked
#'   via a
#'   \href{https://www.w3.org/TR/skos-reference/#semantic-relations}{skos:broader}
#'    relation, see Details.
#' @param class [`character(.)`][character]\cr the class(es) of the new labels.
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concept (for example
#'   \emph{Author+Year}).
#' @param attributes [`tibble()`][tibble]\cr not yet implemented.
#' @param ontology [`ontology(1)`][list]\cr either a path where the
#'   ontology is stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(name = "crops", path = ontoDir)
#'
#' concepts <- data.frame(old = c("Bioenergy woody", "Bioenergy herbaceous"),
#'                        new = c("acacia", "miscanthus"))
#'
#' onto <- new_source(name = "externalDataset",
#'                    description = "a vocabulary",
#'                    homepage = "https://www.something.net",
#'                    license = "CC-BY-0",
#'                    ontology = onto)
#'
#' onto <- get_concept(terms = concepts$old, ontology = onto) %>%
#'   new_concept(new = concepts$new,
#'               broader = .,
#'               class = "crop",
#'               source = "externalDataset",
#'               ontology = onto)
#' @return returns invisibly a table of the new harmonised concepts that were
#'   added to the ontology, or a message that nothing new was added.
#' @importFrom checkmate testCharacter testIntegerish assert assertFileExists
#'   assertSubset assertDataFrame
#' @importFrom tibble tibble
#' @importFrom dplyr filter pull bind_rows arrange
#' @importFrom stringr str_detect str_split str_sub
#' @importFrom readr read_rds write_rds
#' @importFrom utils tail
#' @importFrom methods new
#' @export

new_concept <- function(new, broader = NULL, class = NULL, source, #overwrite = FALSE,
                        attributes = NULL, ontology = NULL){

  if(is.null(new)){
    return("no new concepts to harmonise.")
  }
  newChar <- testCharacter(x = new, ignore.case = FALSE)
  if(!newChar){
    new <- as.character(new)
  }
  assertDataFrame(x = broader, null.ok = TRUE)
  assertCharacter(x = class, any.missing = FALSE)
  assertCharacter(x = source, any.missing = FALSE)

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

  if(!is.null(broader)){

    assertNames(x = names(broader), must.include = c("code", "label_en", "class"))

    testConcept <- broader %>%
      select(code, label_en, class) %>%
      left_join(onto, by = c("code", "label_en", "class"))

    if(any(is.na(testConcept$sourceID))){
      missingConcepts <- testConcept %>%
        filter(is.na(sourceID)) %>%
        pull(label_en)
      stop("the concepts '", paste0(missingConcepts, collapse = ", "), "' don't exist yet as harmonised concepts, please first define them (see function new_concept).")
    }
  }

  if(length(class) != length(new)){
    if(length(class) == 1){
      class <- rep(x = class, length.out = length(new))
    } else {
      stop("the number of elements in 'class' is neither the same as in 'new' nor 1.")
    }
  }

  # needs an error message, in case the classes are not yet defined in the ontlogy

  srcID <- ontology@sources %>%
    filter(sourceName %in% source) %>%
    pull(sourceID)

  if(length(srcID) == 0){
    stop("please first define the source '", source, "' (see function new_source).")
  }

  if(!all(is.na(ontology@concepts$code)) & length(ontology@labels$code) == 0){
    digits <- nchar(ontology@concepts$code[1])
    newConcept <- tibble(code = character(), broader = character(), sourceID = double())
  } else {
    digits <- tail(str_split(ontology@concepts$code[1], "[.]")[[1]], 1)
    digits <- nchar(digits)
    newConcept <- ontology@concepts
  }

  newLabels <- ontology@labels
  newMappings <- ontology@mappings
  iter <- 0
  for(i in seq_along(new)){
    newLabel <- new[i]
    newClass <- class[i]

    if(!is.null(broader)){

      broaderID <- onto %>%
        # filter(sourceName == source)
        filter(sourceName == ontology@sources$sourceName[1]) %>%
        filter(label_en %in% !!broader$label_en[i]) %>%
        pull(code)

      nestedID <- make_tree(input = newConcept, top = broaderID) %>%
        filter(broader == broaderID) %>%
        pull(code)

    } else {
      nestedID <- iter
      broaderID <- NA_character_
    }

    if(length(nestedID) == 0){
      nextID <- paste0(c(broaderID, formatC(1, flag = "0", width = digits)), collapse = ".")
    } else {
      nextID <- tail(nestedID, 1)
      tempID <- str_split(nextID, "[.]")[[1]]
      tempID[length(tempID)] <- formatC(as.numeric(tempID[length(tempID)]) + 1, flag = "0", width = digits) #
      nextID <- paste0(tempID, collapse = ".")
      if(str_sub(nextID, 1, 1) != "."){
        nextID <- paste0(".", nextID)
      }
    }

    iter <- iter + 1

    newConcept <- tibble(code = nextID, sourceID = srcID, broader = broaderID) %>%
      bind_rows(newConcept)
    newLabels <- tibble(code = nextID, class = newClass, label_en = newLabel) %>%
      bind_rows(newLabels)
    newMappings <- tibble(code = nextID, external = NA_character_) %>%
      bind_rows(newMappings)

  }

  newConcept <- newConcept %>%
    select(code, broader, sourceID) %>%
    arrange(code)
  newLabels <- newLabels %>%
    arrange(code)
  newMappings <- newMappings %>%
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
