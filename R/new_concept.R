#' Add a new concept to an ontology
#'
#' This adds a new concept to an existing ontology to semantically integrate and
#' thus harmonise it with the already existing ontology.
#' @param new [`character(.)`][character]\cr the english label(s) of new
#'   concepts that shall be included in the ontology.
#' @param broader [`data.frame(.)`][data.frame]\cr the english label(s) of
#'   already harmonised concepts to which the new concept shall be semantically
#'   linked via a
#'   \href{https://www.w3.org/TR/skos-reference/#semantic-relations}{skos:broader}
#'    relation, see Details.
#' @param description [`character(.)`][character]\cr a verbatim description of
#'   the new concept(s).
#' @param class [`character(.)`][character]\cr the class(es) of the new labels.
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concept (for example
#'   \emph{Author+Year}).
#' @param attributes [`tibble()`][tibble]\cr not yet implemented.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # add fully known concepts
#' concepts <- data.frame(old = c("Bioenergy woody", "Bioenergy herbaceous"),
#'                        new = c("acacia", "miscanthus"))
#'
#' onto <- new_source(name = "externalDataset",
#'                    description = "a vocabulary",
#'                    homepage = "https://www.something.net",
#'                    license = "CC-BY-0",
#'                    ontology = onto)
#'
#' onto <- get_concept(x = data.frame(label_en = concepts$old), ontology = onto) %>%
#'   new_concept(new = concepts$new,
#'               broader = .,
#'               class = "crop",
#'               source = "externalDataset",
#'               ontology = onto)
#'
#' # add concepts where the nesting is clear, but not the new class
#' concepts <- data.frame(old = c("Barley", "Barley"),
#'                        new = c("food", "bio-energy"))
#'
#' onto <- get_concept(x = data.frame(label_en = concepts$old), ontology = onto) %>%
#'   new_concept(new = concepts$new,
#'               broader = .,
#'               source = "externalDataset",
#'               ontology = onto)
#'
#' # define that class ...
#' onto <- new_class(class = "use type", broader = "class",
#'                   description = "the way a crop is used", ontology = onto)
#'
#' # ... and set the concepts again
#' onto <- get_concept(x = data.frame(label_en = concepts$old), ontology = onto) %>%
#'   new_concept(new = concepts$new,
#'               broader = .,
#'               class = "use type",
#'               source = "externalDataset",
#'               ontology = onto)
#'
#' @return returns invisibly a table of the new harmonised concepts that were
#'   added to the ontology, or a message that nothing new was added.
#' @importFrom checkmate testCharacter testIntegerish assert assertFileExists
#'   assertSubset assertDataFrame
#' @importFrom tibble tibble
#' @importFrom dplyr filter pull bind_rows arrange n summarise row_number
#' @importFrom stringr str_detect str_split str_sub str_replace_all
#' @importFrom readr read_rds write_rds
#' @importFrom utils tail
#' @importFrom methods new
#' @export

new_concept <- function(new, broader = NULL, description = NULL, class = NULL,
                        source = NULL, attributes = NULL, ontology = NULL){

  if(is.null(new)){
    return("no new concepts to harmonise.")
  }
  newChar <- testCharacter(x = new, ignore.case = FALSE)
  if(!newChar){
    new <- as.character(new)
  }
  assertDataFrame(x = broader, null.ok = TRUE)
  assertCharacter(x = description, null.ok = TRUE)
  assertCharacter(x = class, null.ok = TRUE)
  assertCharacter(x = source, any.missing = FALSE)

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  onto <- ontology@concepts %>%
    left_join(ontology@labels, by = "id") %>%
    left_join(ontology@sources %>% select(source_id, source_label), by = "source_id") %>%
    left_join(ontology@mappings, by = "id")

  if(!is.null(class)){

    if(any(is.na(class))){

      missing <- new[is.na(class)]
      class[is.na(class)] <- "undefined"

      if(length(missing) > 4){
        missString <- paste0(c(missing[1:3], "...", tail(missing, 1)), collapse = ", ")
      } else {
        missString <- paste0(missing, collapse = ", ")
      }

      warning("some new concepts (", missString, ") don't have a class; please define this with 'new_class()' and re-run 'new_concept()' with these concepts and the new class.", call. = FALSE)

    } else {

      if(!any(ontology@classes$class_label %in% class)){
        missingClasses <- unique(class[!class %in% ontology@classes$class_labelclass_label])
        stop("the class(es) '", paste0(missingClasses, collapse = ", "), "' don't exist yet, please first define them with 'new_class()'.")
      }

    }

    if(length(class) != length(new)){
      if(length(class) == 1){
        class <- rep(x = class, length.out = length(new))
      } else {
        stop("the number of elements in 'class' is neither the same as in 'new' nor 1.")
      }
    }

  } else {
    warning("all new concepts don't have a class; please define this with 'new_class()' and re-run 'new_concept()' with these concepts and the new class.", call. = FALSE)
    class <- rep("undefined", length(new))
  }

  if(!is.null(broader)){

    assertNames(x = names(broader), must.include = c("id", "label_en", "class"))

    testConcept <- broader %>%
      select(id, label_en, class) %>%
      left_join(onto, by = c("id", "label_en", "class"))

    if(any(is.na(testConcept$source_id))){
      missingConcepts <- testConcept %>%
        filter(is.na(source_id)) %>%
        pull(label_en)
      stop("the concepts '", paste0(missingConcepts, collapse = ", "), "' don't exist yet as harmonised concepts, please first define them with 'new_concept()'.")
    }
  } else {
    broader <- tibble(id = rep(NA_character_, length(new)), label_en = rep(NA_character_, length(new)), class = rep(NA_character_, length(new)))
  }

  if(!is.null(description)){

    if(length(description) != length(new)){
      if(length(description) == 1){
        description <- rep(x = description, length.out = length(new))
      } else {
        stop("the number of elements in 'description' is neither the same as in 'new' nor 1.")
      }
    }
  } else {
    description <- NA_character_
  }

  # get the source ID
  srcID <- ontology@sources %>%
    filter(source_label %in% source) %>%
    pull(source_id)

  if(length(srcID) == 0){
    stop("please first define the source '", source, "' with 'new_source()'.")
  }

  # determine how many digits each new code should have
  if(!all(is.na(ontology@concepts$id)) & length(ontology@labels$id) == 0){
    digits <- nchar(ontology@concepts$id[1])
  } else {
    digits <- tail(str_split(ontology@classes$id, "[.]")[[1]], 1)
    digits <- nchar(digits)
  }

  # and by which symbol the levels are separated
  seperator <- str_replace_all(string = ontology@classes$id[1], pattern = "x", replacement = "")

  # get concepts that are already defined for the broader concepts
  nestedIDs <- onto %>%
    filter(has_broader %in% !!broader$id) %>%
    select(id = has_broader, nestedID = id, top2D = has_broader, class)

  # get the broader concepts
  broaderIDs <- onto %>%
    filter(id %in% !!broader$id) %>%
    group_by(id, class) %>%
    summarise(topID = suppressWarnings(max(id))) %>%
    ungroup() %>%
    arrange(id)

  # assign nested and broader IDs into the temporary object
  temp <- bind_cols(broader, tibble(new = new, newClass = class, description = description)) %>%
    left_join(nestedIDs, by = c("id", "class")) %>%
    left_join(broaderIDs, by = c("id", "class")) %>%
    unite(col = topID, topID, top2D, sep = "", na.rm = TRUE)

  # build the new ID
  temp <- temp %>%
    group_by(id) %>%
    mutate(nextID = if_else(!is.na(nestedID),
                            paste0(topID, seperator, formatC(as.numeric(tail(str_split(nestedID, if_else(seperator == ".", "[.]", seperator))[[1]], 1)) + row_number(), flag = "0", width = digits)),
                            paste0(topID, seperator, formatC(row_number(), flag = "0", width = digits))),
           source_id = srcID,
           external_id = NA_character_) %>%
    ungroup()

  newLabels <- temp %>%
    select(id = nextID, class = newClass, label_en = new) %>%
    bind_rows(ontology@labels) %>%
    arrange(id)

  # filter concepts that are duplicated because they were previously undefined
  extConcepts <- newLabels %>%
    filter(is.na(class)) %>%
    mutate(undef = FALSE)

  undefined <- newLabels %>%
    filter(!is.na(class)) %>%
    group_by(label_en) %>%
    mutate(undef = if_else(n() > 1 & class == "undefined", TRUE, FALSE)) %>%
    ungroup() %>%
    bind_rows(extConcepts) %>%
    pull(undef)

  newLabels <- newLabels %>%
    filter(!undefined)

  newConcept <- temp %>%
    select(id = nextID, has_broader = id, source_id) %>%
    bind_rows(ontology@concepts) %>%
    arrange(id) %>%
    filter(!undefined)
  newMappings <- temp %>%
    select(id = nextID, external_id) %>%
    bind_rows(ontology@mappings) %>%
    arrange(id) %>%
    filter(!undefined)


  out <- new(Class = "onto",
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
