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
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # add fully known concepts
#' concepts <- data.frame(
#'   old = c("Bioenergy woody", "Bioenergy herbaceous"),
#'   new = c("acacia", "miscanthus")
#' )
#'
#' onto <- new_source(
#'   version = "0.0.1",
#'   name = "externalDataset",
#'   description = "a vocabulary",
#'   homepage = "https://www.something.net",
#'   license = "CC-BY-0",
#'   ontology = onto
#' )
#'
#' onto <- new_concept(
#'   new = concepts$new,
#'   broader = get_concept(label = concepts$old, ontology = onto),
#'   class = "crop",
#'   ontology = onto
#' )
#'
#' # add concepts where the nesting is clear, but not the new class
#' concepts <- data.frame(
#'   old = c("Barley", "Barley"),
#'   new = c("food", "bio-energy")
#' )
#'
#' onto <- new_concept(
#'   new = concepts$new,
#'   broader = get_concept(label = concepts$old, ontology = onto),
#'   ontology = onto
#' )
#'
#' # define that class ...
#' onto <- new_class(
#'   new = "use type", target = "class",
#'   description = "the way a crop is used", ontology = onto
#' )
#'
#' # ... and set the concepts again
#' onto <- new_concept(
#'   new = concepts$new,
#'   broader = get_concept(label = concepts$old, ontology = onto),
#'   class = "use type",
#'   ontology = onto
#' )
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
                        ontology = NULL){

  assertCharacter(x = new, any.missing = FALSE)
  assertDataFrame(x = broader, null.ok = TRUE)
  assertCharacter(x = description, null.ok = TRUE)
  assertCharacter(x = class, null.ok = TRUE)

  if (inherits(x = ontology, what = "onto")) {
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  theConcepts <- ontology@concepts

  if (!is.null(class)) {
    if (any(is.na(class))) {
      missing <- new[is.na(class)]
      class[is.na(class)] <- "undefined"

      if (length(missing) > 4) {
        missString <- paste0(c(missing[1:3], "...", tail(missing, 1)), collapse = ", ")
      } else {
        missString <- paste0(missing, collapse = ", ")
      }

      warning("some new concepts (", missString, ") don't have a class; please define this with 'new_class()' and re-run 'new_concept()' with these concepts and the new class.", call. = FALSE)
    } else {
      if (!any(ontology@classes$harmonised$label %in% class)) {
        missingClasses <- unique(class[!class %in% ontology@classes$harmonised$label])
        stop("the class(es) '", paste0(missingClasses, collapse = ", "), "' don't exist yet, please first define them with 'new_class()'.")
      }
    }

    if (length(class) != length(new)) {
      if (length(class) == 1) {
        class <- rep(x = class, length.out = length(new))
      } else {
        stop("the number of elements in 'class' is neither the same as in 'new' nor 1.")
      }
    }
  } else {
    warning("all new concepts don't have a class; please define this with 'new_class()' and re-run 'new_concept()' with these concepts and the new class.", call. = FALSE)
    class <- rep("undefined", length(new))
  }

  if (!is.null(broader)) {
    assertNames(x = names(broader), must.include = c("id", "label", "class"))

    testConcept <- broader %>%
      select(id, label, class) %>%
      mutate(avail = TRUE) %>%
      left_join(theConcepts$harmonised, by = c("id", "label", "class"))

    if (any(!testConcept$avail)) {
      missingConcepts <- testConcept %>%
        filter(!avail) %>%
        pull(label)
      stop("the concepts '", paste0(missingConcepts, collapse = ", "), "' don't exist yet as harmonised concepts, please first define them with 'new_concept()'.")
    }
  } else {
    broader <- tibble(id = rep(NA_character_, length(new)), label = rep(NA_character_, length(new)), class = rep(NA_character_, length(new)))
  }

  if (!is.null(description)) {
    if (length(description) != length(new)) {
      if (length(description) == 1) {
        description <- rep(x = description, length.out = length(new))
      } else {
        stop("the number of elements in 'description' is neither the same as in 'new' nor 1.")
      }
    }
  } else {
    description <- NA_character_
  }

  # determine how many digits each new code should have
  digits <- tail(str_split(ontology@classes$harmonised$id, "[.]")[[1]], 1)
  digits <- nchar(digits)

  # and by which symbol the levels are separated
  seperator <- str_replace_all(string = ontology@classes$harmonised$id[1], pattern = "x", replacement = "")

  # get concepts that are already defined for the broader concepts
  nestedIDs <- theConcepts$harmonised %>%
    filter(has_broader %in% !!broader$id) %>%
    select(id = has_broader, nestedID = id, top2D = has_broader, class)

  # get the broader concepts
  broaderIDs <- theConcepts$harmonised %>%
    filter(id %in% !!broader$id) %>%
    group_by(id, class) %>%
    summarise(topID = suppressWarnings(max(id))) %>%
    ungroup() %>%
    arrange(id)

  # assign nested and broader IDs into the temporary object
  temp <- broader %>%
    select(id, label, class) %>%
    bind_cols(tibble(new = new, newClass = class, description = description)) %>%
    left_join(nestedIDs, by = c("id", "class")) %>%
    left_join(broaderIDs, by = c("id", "class")) %>%
    unite(col = topID, topID, top2D, sep = "", na.rm = TRUE)

  # get the maximum child ID that may have been defined already
  oldChildConcepts <- theConcepts$harmonised %>%
    group_by(has_broader) %>%
    summarise(children = n())

  # build the new ID
  temp <- temp %>%
    left_join(oldChildConcepts, by = c("topID" = "has_broader")) %>%
    mutate(children = if_else(is.na(children), 0L, children)) %>%
    group_by(id) %>%
    mutate(nextID = if_else(!is.na(nestedID),
                            paste0(topID, seperator, formatC(as.numeric(tail(str_split(nestedID, if_else(seperator == ".", "[.]", seperator))[[1]], 1)) + row_number() + children, flag = "0", width = digits)),
                            paste0(topID, seperator, formatC(row_number() + children, flag = "0", width = digits))),
           nextID = as.character(nextID),
           has_close_match = NA_character_,
           has_narrower_match = NA_character_,
           has_broader_match = NA_character_,
           has_exact_match = NA_character_) %>%
    ungroup()

  temp <- temp %>%
    select(
      id = nextID,
      label = new,
      description,
      class = newClass,
      has_broader = id,
      has_close_match, has_narrower_match, has_broader_match, has_exact_match
    ) %>%
    bind_rows(theConcepts$harmonised) %>%
    arrange(id)

  # filter concepts that are duplicated because they were previously undefined
  extConcepts <- temp %>%
    filter(is.na(class)) %>%
    mutate(undef = FALSE)

  undefined <- temp %>%
    filter(!is.na(class)) %>%
    group_by(label) %>%
    mutate(undef = if_else(n() > 1 & class == "undefined", TRUE, FALSE)) %>%
    ungroup() %>%
    bind_rows(extConcepts) %>%
    pull(undef)

  theConcepts$harmonised <- temp %>%
    filter(!undefined) %>%
    arrange(id)

  out <- new(
    Class = "onto",
    sources = ontology@sources,
    classes = ontology@classes,
    concepts = theConcepts
  )

  if (!is.null(ontoPath)) {
    write_rds(x = out, file = ontoPath)
  }

  return(out)
}
