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
#' onto <- get_concept(terms = concepts$old, ontology = onto) %>%
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
#' onto <- get_concept(terms = concepts$old, ontology = onto) %>%
#'   new_concept(new = concepts$new,
#'               broader = .,
#'               source = "externalDataset",
#'               ontology = onto)
#'
#' # define that class ...
#' onto <- new_class(class = "use type", broader = "class",
#'                   definition = "the way a crop is used", ontology = onto)
#'
#' # ... and set the concepts again
#' onto <- get_concept(terms = concepts$old, ontology = onto) %>%
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

new_concept <- function(new, broader = NULL, class = NULL, source,
                        attributes = NULL, ontology = NULL){

  if(is.null(new)){
    return("no new concepts to harmonise.")
  }
  newChar <- testCharacter(x = new, ignore.case = FALSE)
  if(!newChar){
    new <- as.character(new)
  }
  assertDataFrame(x = broader, null.ok = TRUE)
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
    left_join(ontology@labels, by = "code") %>%
    left_join(ontology@sources %>% select(sourceID, sourceName), by = "sourceID") %>%
    left_join(ontology@mappings, by = "code")

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

      if(!any(ontology@classes$class %in% class)){
        missingClasses <- unique(class[!class %in% ontology@classes$class])
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

    assertNames(x = names(broader), must.include = c("code", "label_en", "class"))

    testConcept <- broader %>%
      select(code, label_en, class) %>%
      left_join(onto, by = c("code", "label_en", "class"))

    if(any(is.na(testConcept$sourceID))){
      missingConcepts <- testConcept %>%
        filter(is.na(sourceID)) %>%
        pull(label_en)
      stop("the concepts '", paste0(missingConcepts, collapse = ", "), "' don't exist yet as harmonised concepts, please first define them with 'new_concept()'.")
    }
  } else {
    broader <- tibble(code = rep(NA_character_, length(new)), label_en = rep(NA_character_, length(new)), class = rep(NA_character_, length(new)))
  }

  # get the source ID
  srcID <- ontology@sources %>%
    filter(sourceName %in% source) %>%
    pull(sourceID)

  if(length(srcID) == 0){
    stop("please first define the source '", source, "' with 'new_source()'.")
  }

  # determine how many digits each new code should have
  if(!all(is.na(ontology@concepts$code)) & length(ontology@labels$code) == 0){
    digits <- nchar(ontology@concepts$code[1])
  } else {
    digits <- tail(str_split(ontology@classes$level, "[.]")[[1]], 1)
    digits <- nchar(digits)
  }

  # and by which symbol the levels are separated
  seperator <- str_replace_all(string = ontology@classes$level[1], pattern = "x", replacement = "")

  # get concepts that are already defined for the broader concepts
  nestedIDs <- onto %>%
    filter(broader %in% !!broader$code) %>%
    select(code = broader, nestedID = code, top2D = broader, class)

  # get the broader concepts
  broaderIDs <- onto %>%
    filter(code %in% !!broader$code) %>%
    group_by(code, class) %>%
    summarise(topID = suppressWarnings(max(code))) %>%
    ungroup() %>%
    arrange(code)

  # assign nested and broader IDs into the temporary object
  temp <- bind_cols(broader, tibble(new = new, newClass = class)) %>%
    left_join(nestedIDs, by = c("code", "class")) %>%
    left_join(broaderIDs, by = c("code", "class")) %>%
    unite(col = topID, topID, top2D, sep = "", na.rm = TRUE)

  # build the new ID
  temp <- temp %>%
    group_by(code) %>%
    mutate(nextID = if_else(!is.na(nestedID),
                            paste0(topID, seperator, formatC(as.numeric(tail(str_split(nestedID, if_else(seperator == ".", "[.]", seperator))[[1]], 1)) + row_number(), flag = "0", width = digits)),
                            paste0(topID, seperator, formatC(row_number(), flag = "0", width = digits))),
           sourceID = srcID,
           external = NA_character_) %>%
    ungroup()

  newLabels <- temp %>%
    select(code = nextID, class = newClass, label_en = new) %>%
    bind_rows(ontology@labels) %>%
    arrange(code)

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
    select(code = nextID, broader = code, sourceID) %>%
    bind_rows(ontology@concepts) %>%
    arrange(code) %>%
    filter(!undefined)
  newMappings <- temp %>%
    select(code = nextID, external) %>%
    bind_rows(ontology@mappings) %>%
    arrange(code) %>%
    filter(!undefined)



  ###################### remove ####################

  # newLabels <- ontology@labels
  # newMappings <- ontology@mappings
  # iter <- 0
  # for(i in seq_along(new)){
  #   newLabel <- new[i]
  #   newClass <- class[i]
  #
  #   if(!is.null(broader)){
  #
  #     broaderID <- onto %>%
  #       # filter(sourceName == source) %>%
  #       filter(label_en %in% !!broader$label_en[i]) %>%
  #       pull(code)
  #
  #     nestedID <- make_tree(input = newConcept, top = broaderID) %>%
  #       filter(broader == broaderID) %>%
  #       pull(code)
  #
  #   } else {
  #     nestedID <- iter
  #     broaderID <- NA_character_
  #   }
  #
  #   if(length(nestedID) == 0){
  #     nextID <- paste0(c(broaderID, formatC(1, flag = "0", width = digits)), collapse = ".")
  #   } else {
  #
  #     nestedConcepts <- ontology@labels[ontology@labels$code %in% nestedID,]
  #
  #     if(newLabel %in% nestedConcepts$label_en){
  #       nextID <- nestedConcepts$code[nestedConcepts$label_en %in% newLabel]
  #     } else {
  #       nextID <- tail(str_split(nextID, "[.]")[[1]], 1)
  #       tempID <- str_split(nextID, "[.]")[[1]]
  #       tempID[length(tempID)] <- formatC(as.numeric(tempID[length(tempID)]) + 1, flag = "0", width = digits)
  #       nextID <- paste0(tempID, collapse = ".")
  #       if(str_sub(nextID, 1, 1) != "."){
  #         nextID <- paste0(".", nextID)
  #       }
  #     }
  #
  #   }
  #
  #   iter <- iter + 1
  #
  #   newConcept <- tibble(code = nextID, sourceID = srcID, broader = broaderID) %>%
  #     bind_rows(newConcept)
  #   newLabels <- tibble(code = nextID, class = newClass, label_en = newLabel) %>%
  #     bind_rows(newLabels)
  #   newMappings <- tibble(code = nextID, external = NA_character_) %>%
  #     bind_rows(newMappings)
  #
  # }

  # newConcept <- newConcept %>%
  #   filter(!undefined) %>%
  #   arrange(code) %>%
  #   select(code, broader, sourceID)
  # newLabels <- newLabels %>%
  #   filter(!undefined) %>%
  #   arrange(code) %>%
  #   select(code, class, label_en)
  # newMappings <- newMappings %>%
  #   filter(!undefined) %>%
  #   arrange(code) %>%
  #   select(code, external)


  ###################### remove ####################

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
