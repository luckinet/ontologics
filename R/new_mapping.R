#' Add a new mapping to an ontology
#'
#' Extend an ontology by creating mappings between classes and concepts of
#' external vocabularies and the harmonised classes and concepts.
#' @param new [`character(.)`][character]\cr the english external label(s) that
#'   shall be mapped to labels that do already exist in the ontology.
#' @param target [`data.frame(.)`][data.frame]\cr the already harmonised english
#'   label(s) to which the external labels shall be mapped.
#' @param match [`character(1)`][character]\cr the
#'   \href{https://www.w3.org/TR/skos-reference/#mapping}{skos mapping property}
#'   used to describe the link, possible values are \code{"close"},
#'   \code{"exact"}, \code{"broad"} and \code{"narrow"}.
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new label.
#' @param description [`character(.)`][character]\cr a verbatim description of
#'   the new label.
#' @param certainty [`integerish(1)`][integer]\cr the certainty of the match.
#'   Possible values are between 1 and 4, with meaning \itemize{\item 1 =
#'   probably unreliable \item 2 = unclear, assigned according to a given
#'   definition \item 3 = clear, assigned according to a given definition \item
#'   4 = original, harmonised term (can't be assigned by a user)}.
#' @param type [`character(1)`][character]\cr whether the new labels are mapped
#'   to a \code{"concept"}, or to a \code{"class"}.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' mapping <- data.frame(old = c("BIOENERGY CROPS", "Bioenergy woody",
#'                               "Other bioenergy crops"),
#'                       new = c("bioenergy plants", "Wood plantation for fuel",
#'                               "Algae for bioenergy"),
#'                       type = c("close", "broader", "broader"))
#'
#' onto <- new_source(name = "externalDataset",
#'                    description = "a vocabulary",
#'                    homepage = "https://www.something.net",
#'                    license = "CC-BY-0",
#'                    ontology = onto)
#'
#' onto <- get_concept(x = data.frame(label = mapping$old), ontology = onto) %>%
#'   new_mapping(new = mapping$new,
#'               target = .,
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
#'   bind_cols full_join na_if
#' @importFrom tidyr unite pivot_wider
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect str_split
#' @importFrom readr read_rds write_rds
#' @importFrom methods new
#' @export

new_mapping <- function(new = NULL, target, source = NULL, description = NULL,
                        match = "close", certainty = NULL, type = "concept",
                        ontology = NULL){

  assertDataFrame(x = target)
  assertCharacter(x = new)
  assertNames(x = match, subset.of = c("close", "exact", "broader", "narrower"))
  assertIntegerish(x = certainty, lower = 1, upper = 3)
  assertChoice(x = type, choices = c("concept", "class"))

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  if(type == "concept"){
    typeNames <- "concepts"
    typeName <- "concept"
    targetCols <- c("id", "label", "class")
    theTable <- ontology@concepts
  } else if(type == "class"){
    typeNames <- "classes"
    typeName <- "class"
    targetCols <- c("id", "label")
    theTable <- ontology@classes
    target <- suppressWarnings(target %>%
      left_join(theTable$harmonised) %>%
      select(id, label))
  }
  assertNames(x = names(target), must.include = targetCols)

  testTable <- target %>%
    select(all_of(targetCols)) %>%
    mutate(avail = TRUE) %>%
    left_join(theTable$harmonised, by = targetCols)

  if(any(!testTable$avail)){
    missingItems <- testTable %>%
      filter(!avail) %>%
      pull(label)
    stop("the ", typeNames," '", paste0(missingItems, collapse = ", "), "' don't exist yet as harmonised ", typeNames,", please first define them with 'new_", typeName,"()'.")
  }

  if(length(match) != length(target$label)){
    if(length(match) == 1){
      match <- rep(x = match, length.out = length(target$label))
    } else {
      stop("the number of elements in 'match' is neither the same as in 'target' nor 1.")
    }
  }

  if(length(certainty) != length(target$label)){
    if(length(certainty) == 1){
      certainty <- rep(x = certainty, length.out = length(target$label))
    } else {
      stop("the number of elements in 'certainty' is neither the same as in 'target' nor 1.")
    }
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

  srcID <- ontology@sources %>%
    filter(label %in% source) %>%
    pull(id)

  if(length(srcID) == 0){
    stop("please first define the source '", source, "' (see function new_source).")
  }

  prevID <- str_detect(string = theTable$external$id, pattern = source)
  if(!any(prevID)){
    prevID <- 0
  } else {
    prevID <- str_split(theTable$external$id[prevID], pattern = "_", simplify = TRUE)
    prevID <- as.numeric(prevID[, dim(prevID)[2]])
    prevID <- max(prevID, na.rm = TRUE)
    if(is.na(prevID)) prevID <- 0
  }

  temp <- target %>%
    select(all_of(targetCols)) %>%
    bind_cols(tibble(new = new, match = match, certainty = certainty,
                     description = description, has_source = srcID)) %>%
    separate_rows(new, sep = " \\| ")

  extMps <- temp %>%
    distinct(new, description, has_source) %>%
    filter(new != "") %>%
    mutate(newid = paste0(source, "_", row_number() + prevID)) %>%
    select(id = newid, label = new, description, has_source) %>%
    bind_rows(ontology@concepts$external) %>%
    filter(!(label %in% theTable$external$label & has_source %in% theTable$external$has_source))
  theTable$external <- extMps %>%
    bind_rows(theTable$external, .)

  if(dim(extMps)[1] != 0){

    toOut <- temp %>%
      left_join(theTable$external %>% select(new = label, newid = id), by = "new") %>%
      mutate(newid = if_else(!is.na(newid), paste0(newid, ".", certainty), NA_character_),
             match = paste0("new_", match, "_match")) %>%
      group_by(id, label, match) %>%
      summarise(newid = paste0(newid, collapse = " | ")) %>%
      ungroup() %>%
      mutate(newid = na_if(newid, "NA")) %>%
      pivot_wider(id_cols = c(id, label), names_from = match, values_from = newid) %>%
      full_join(theTable$harmonised, by = c("id", "label"))

    if("new_close_match" %in% colnames(toOut)){
      toOut <- toOut %>%
        unite(col = "has_close_match", new_close_match, has_close_match, sep = " | ", na.rm = TRUE)
    }
    if("new_broader_match" %in% colnames(toOut)){
      toOut <- toOut %>%
        unite(col = "has_broader_match", new_broader_match, has_broader_match, sep = " | ", na.rm = TRUE)
    }
    if("new_narrower_match" %in% colnames(toOut)){
      toOut <- toOut %>%
        unite(col = "has_narrower_match", new_narrower_match, has_narrower_match, sep = " | ", na.rm = TRUE)
    }
    if("new_exact_match" %in% colnames(toOut)){
      toOut <- toOut %>%
        unite(col = "has_exact_match", new_exact_match, has_exact_match, sep = " | ", na.rm = TRUE)
    }

    toOut <- toOut %>%
      na_if(y = "") %>%
      select(all_of(targetCols), description, has_broader, has_close_match, has_broader_match, has_narrower_match, has_exact_match) %>%
      arrange(id)

    theTable$harmonised <- toOut

  }

  if(type == "concept"){
    out <- new(Class = "onto",
               sources = ontology@sources,
               classes = ontology@classes,
               concepts = theTable)
  } else if(type == "class"){
    out <- new(Class = "onto",
               sources = ontology@sources,
               classes = theTable,
               concepts = ontology@concepts)
  }


  if(!is.null(ontoPath)){
    write_rds(x = out, file = ontoPath)
  }

  return(out)

}
