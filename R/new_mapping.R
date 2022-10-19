#' Add a new mapping to an ontology
#'
#' Extend an ontology by creating mappings between classes and concepts of
#' external vocabularies and the harmonised classes and concepts.
#' @param new [`character(.)`][character]\cr the english external label(s) that
#'   shall be mapped to labels that do already exist in the ontology.
#' @param target [`data.frame(.)`][data.frame]\cr the already harmonised English
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
#' @param matchDir [`character(1)`][character]\cr the directory where to store
#'   source-specific matching tables.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
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
#'                    version = "0.0.1",
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
#'   bind_cols full_join na_if across
#' @importFrom tidyr unite pivot_wider
#' @importFrom tidyselect all_of
#' @importFrom stringr str_detect str_split str_replace
#' @importFrom readr read_rds write_rds
#' @importFrom methods new
#' @importFrom stats na.omit
#' @export

new_mapping <- function(new = NULL, target, source = NULL, description = NULL,
                        match = NULL, certainty = NULL, type = "concept",
                        ontology = NULL, matchDir = NULL, verbose = FALSE){

  assertCharacter(x = new, all.missing = FALSE)
  assertDataFrame(x = target, nrows = length(new))
  assertCharacter(x = description, null.ok = TRUE)
  assertIntegerish(x = certainty, lower = 1, upper = 4)
  assertChoice(x = type, choices = c("concept", "class"))
  assertLogical(x = verbose, len = 1)

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  if(!is.null(description)){

    if(length(description) != length(new)){
      if(length(description) == 1){
        description <- bind_cols(desc = description, label = new)
      } else {
        stop("the number of elements in 'description' is neither the same as in 'new' nor 1.")
      }
    } else if(all(description == "")) {
      description <- bind_cols(desc = NA_character_, label = new)
    } else {
      description <- bind_cols(desc = description, label = new)
    }
  } else {
    description <- bind_cols(desc = NA_character_, label = new)
  }

  if(!is.null(match)){
    if(length(match) != length(new)){
      if(length(match) == 1){
        match <- rep(x = match, length.out = length(new))
      } else {
        stop("the number of elements in 'match' is neither the same as in 'target' nor 1.")
      }
    }
  }

  # prepare some variables
  if(type == "concept"){
    typeNames <- "concepts"
    typeName <- "concept"
    targetCols <- c("id", "label", "class", "has_broader")
    theTable <- ontology@concepts
  } else if(type == "class"){
    typeNames <- "classes"
    typeName <- "class"
    targetCols <- c("id", "label", "has_broader")
    theTable <- ontology@classes
    target <- suppressWarnings(target %>%
                                 left_join(theTable$harmonised, by = colnames(target)) %>%
                                 select(id, label, has_broader))
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

  # in case target doesn't contain labels, call the function edit_matches, to
  # assign the new terms to the already existing labels in the ontology
  if(!"label" %in% colnames(target)){
    assertNames(x = colnames(target), must.include = "class")

    related <- edit_matches(concepts = tibble(label = new), attributes = target, source = source,
                            ontology = ontology, matchDir = matchDir, verbose = verbose)

    temp <- related %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "new") %>%
      filter(!is.na(new)) %>%
      mutate(certainty = certainty,
             has_source = srcID,
             match = str_replace(string = match, pattern = "has_", replacement = ""),
             match = str_replace(string = match, pattern = "_match", replacement = "")) %>%
      separate_rows(new, sep = " \\| ")

  } else {
    assertNames(x = names(target), must.include = targetCols)
    assertSubset(x = match, choices = c("close", "exact", "broader", "narrower"))

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

    temp <- target %>%
      select(all_of(targetCols)) %>%
      bind_cols(tibble(new = new, match = match, certainty = certainty,
                       description = description$desc, has_source = srcID)) %>%
      separate_rows(new, sep = " \\| ")

  }

  extMps <- temp %>%
    distinct(new, description, has_source) %>%
    filter(new != "") %>%
    filter(!(new %in% theTable$external$label & has_source %in% theTable$external$has_source)) %>%
    mutate(newid = paste0(source, "_", row_number() + prevID)) %>%
    select(id = newid, label = new, description, has_source)

  if(!all(is.na(description$desc)) & !dim(extMps)[1] == 0){
    extMps <- left_join(extMps, description, by = "label") %>%
      mutate(description = desc) %>%
      select(-desc)
  }

  theTable$external <- extMps %>%
    bind_rows(theTable$external, .)

  if(dim(extMps)[1] != 0){

    toOut <- temp %>%
      left_join(theTable$external %>% filter(has_source == srcID) %>% select(new = label, newid = id), by = "new") %>%
      filter(!is.na(newid)) %>%
      mutate(newid = if_else(!is.na(newid), paste0(newid, ".", certainty), NA_character_),
             match = paste0("has_", match, "_match")) %>%
      select(-certainty, -has_source, -new)

    toOut <- theTable$harmonised %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "newid") %>%
      separate_rows(newid, sep = " \\| ") %>%
      full_join(toOut, by = c(all_of(targetCols), "description", "match", "newid")) %>%
      distinct()

    toOut <- toOut %>%
      group_by(across(all_of(targetCols)), description, has_broader, match) %>%
      summarise(newid = paste0(na.omit(newid), collapse = " | ")) %>%
      ungroup() %>%
      mutate(newid = na_if(newid, "")) %>%
      pivot_wider(id_cols = c(all_of(targetCols), description, has_broader), names_from = match, values_from = newid)

    toOut <- toOut %>%
      na_if(y = "") %>%
      select(all_of(targetCols), description, has_close_match, has_broader_match, has_narrower_match, has_exact_match) %>%
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
