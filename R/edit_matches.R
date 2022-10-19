#' Edit matches manually in a csv-table
#'
#' Allows the user to match concepts with an already existing ontology, without
#' actually writing into the ontology, but instead storing the resulting
#' matching table as csv. This function is used in the function
#' \code{\link{new_mapping}} and is not primarily intended for use on its own.
#' @param concepts [`data.frame(.)`][data.frame]\cr the new concepts that shall
#'   be manually matched.
#' @param attributes [`data.frame(.)`][data.frame]\cr the attributes of new
#'   concepts that help to match new and target concepts manually (must contain
#'   at least the column 'class').
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concepts.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @param matchDir [`character(1)`][character]\cr the directory where to store
#'   source-specific matching tables.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
#' @details In order to match new concepts into an already existing ontology, it
#'   may become necessary to carry out manual matches of the new concepts with
#'   already harmonised concepts, for example, when the new concepts are
#'   described with terms that are not yet in the ontology. This function puts
#'   together a table, in which the user would edit matches by hand. Whith the
#'   argument \code{verbose = TRUE}, detailed information about the edit process
#'   are shown to the user. After defining matches, and even if not all
#'   necessary matches are finished, the function stores a specific "matching
#'   table" with the name \emph{match_SOURCE.csv} in the respective directory
#'   (\code{matchDir}), from where work can be picked up and continued at
#'   another time.
#' @return A table that contains all new matches, or if none of the new concepts
#'   weren't already in the ontology, a table of the already sucessful matches.
#' @importFrom checkmate assertDataFrame assertNames assertCharacter
#'   assertFileExists testFileExists
#' @importFrom utils tail head
#' @importFrom stringr str_split str_detect
#' @importFrom readr read_csv write_csv cols
#' @importFrom dplyr filter rename full_join mutate if_else select left_join
#'   bind_rows distinct arrange
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer pivot_wider
#' @export

edit_matches <- function(concepts, attributes = NULL, source = NULL,
                         ontology = NULL, matchDir = NULL, verbose = TRUE){

  assertDataFrame(x = concepts, min.cols = 1)
  assertNames(x = names(concepts), must.include = c("label"))
  assertDataFrame(x = attributes, nrows = nrow(concepts))
  assertCharacter(x = source, len = 1, any.missing = FALSE)

  intPaths <- getOption(x = "adb_path")
  sourceFile <- paste0("match_", source, ".csv")

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  # ... and store the newly defined matches as a source specific matching table
  if(testFileExists(paste0(matchDir, sourceFile))){
    prevMatches <- read_csv(paste0(matchDir, sourceFile), col_types = cols(.default = "c"))
  } else {
    prevMatches <- NULL
  }

  filterClasses <- ontology@classes$harmonised %>%
    filter(label %in% attributes$class)
  while(!any(is.na(filterClasses$has_broader))){
    filterClasses <- ontology@classes$harmonised %>%
      filter(label %in% filterClasses$label | label %in% filterClasses$has_broader)
  }
  filterClasses <- filterClasses %>%
    pull(label) %>%
    unique()
  attributes <- attributes %>%
    select(-class)

  allAttribs <- concepts %>%
    bind_cols(attributes)
  selectedCols <- which(colnames(allAttribs) %in% c("id", "has_broader", "source_id", "class", "label", "source_label", "external_label"))

  temp <- get_concept(x = allAttribs[,selectedCols], na.rm = FALSE, ontology = ontology, mappings = "all") %>%
    left_join(allAttribs)

  # determine those concepts, that are not yet defined in the ontology
  if(!is.null(prevMatches)){

    temp <- prevMatches %>%
      filter(class %in% filterClasses) %>%
      rename(harmLab = label) %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "label") %>%
      separate_rows(label, sep = " \\| ") %>%
      full_join(temp, by = c("label", "class", "id", "has_broader", "description")) %>%
      filter(!(is.na(match) & !is.na(id))) %>%
      mutate(harmLab = if_else(is.na(match), label, harmLab),
             match = if_else(!is.na(has_close_match), "has_close_match", if_else(is.na(match), "sort_in", match)),
             label = if_else(is.na(match), NA_character_, label)) %>%
      pivot_wider(id_cols = c(harmLab, class, id, has_broader, description), names_from = match,
                  values_from = label, values_fn = ~paste0(.x, collapse = " | ")) %>%
      na_if(y = "NA") %>%
      rename(label = harmLab)

    if("sort_in" %in% colnames(temp)){
      temp <- temp %>%
        select(-sort_in)
    }

  }

  missingConcepts <- temp %>%
    filter(is.na(id))
  inclConcepts <- temp %>%
    filter(!is.na(id))

  # build a table of external concepts and of harmonised concepts these should be overwritten with
  if(dim(missingConcepts)[1] != 0){

    relate <- ontology@concepts$harmonised %>%
      select(id, label, class, has_broader) %>%
      left_join(inclConcepts, by = c("id", "label", "class", "has_broader")) %>%
      filter(!is.na(class)) %>%
      filter(class %in% filterClasses)

    sortIn <- missingConcepts %>%
      select(-colnames(attributes)[which(colnames(attributes) %in% colnames(missingConcepts))]) %>%
      left_join(concepts %>% bind_cols(attributes), by = "label") %>%
      mutate(sort_in = label,
        label = NA_character_,
        class = NA_character_) %>%
      select(sort_in, names(attributes), id, has_broader, label, class, everything())

    # put together the object that shall be edited by the user ...
    sortIn %>%
      bind_rows(relate) %>%
      write_csv(file = paste0(matchDir, "/matching.csv"), quote = "all", na = "")

    # ... and make them aware of their duty
    message("please edit the file '", paste0(matchDir, "/matching.csv"), "' \n")
    if(verbose){
      message("--- column description ---\n")
      message("sort_in             cut out these values and sort them either into 'has_broader_match', \n                    'has_exact_match', has_narrower_match or 'has_close_match'")
      message("id                  filter by this column to jump to the subset you need to edit")
      message("label               concepts to which the new terms should be related")
      message("class               the class of harmonised concepts")
      message("has_close_match     in case a new concept is a close match to the harmonised concept, paste \n                    it here, delimit several concepts with a '|'")
      message("has_broader_match   in case a new concept is a broader match than the harmonised concept, \n                    paste it here, delimit several concepts with a '|'")
      message("has_narrower_match  in case a new concept is a narrower match than the harmonised concept, \n                    paste it here, delimit several concepts with a '|'")
      message("has_exact_match     in case a new concept is an exact match to the harmonised concept \n                    (which is only the case when it's from the same ontology), paste it \n                    here, delimit several concepts with a '|'")
      message("\n--- some useful tips ---")
      message("\n-> values that were already successfully matched by previous translations are listed here, \n   however, altering them here doesn't change the ontology. \n\n-> any row that doesn't contain a value in the column 'code' will be discarded. Hence, \n   if you want a value to be ignored, simply don't paste it anywhere. \n\n-> do not change the values in the columns 'code', 'harmonised' and 'class', as they \n   are important to insert the new matches into the ontology. \n\n-> if a term shall be nested into a position that doesn't have a class, (for example, \n   because that class occurrs the first time with this term) first create that nested \n   class with 'new_class()'.\n")
    }
    done <- readline(" -> press any key when done: ")

    related <- read_csv(paste0(matchDir, "/matching.csv"), col_types = cols(.default = "c")) %>%
      filter(!is.na(id)) %>%
      select(-sort_in)

    if(dim(related)[1] == 0){
      related <- NULL
    }

  } else {
    related <- inclConcepts
  }

  out <- prevMatches %>%
    bind_rows(related) %>%
    pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                 names_to = "match", values_to = "new_label") %>%
    separate_rows(new_label, sep = " \\| ") %>%
    distinct() %>%
    group_by(id, has_broader, label, class, description, match) %>%
    summarise(new_label = paste0(new_label, collapse = " | "), .groups = "keep") %>%
    ungroup() %>%
    mutate(new_label = na_if(x = new_label, y = "NA")) %>%
    pivot_wider(id_cols = c(label, class, id, has_broader, description), names_from = match, values_from = new_label) %>%
    distinct() %>%
    filter(!is.na(has_broader_match) | !is.na(has_close_match) | !is.na(has_narrower_match) | !is.na(has_exact_match)) %>%
    arrange(id)

  write_csv(x = out, file = paste0(matchDir, sourceFile), append = FALSE, na = "")

  return(related)
}
