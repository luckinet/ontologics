#' Edit matches manually in a csv-table
#'
#' @param concepts [`data.frame(.)`][data.frame]\cr the new concepts that shall
#'   be manually matched.
#' @param attributes [`character(1)`][character]\cr the attributes of new
#'   concepts that help to match new and target concepts concepts manually (must
#'   contain at least the column 'class').
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concepts.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @param matchDir [`character(1)`][character]\cr the directory where to store
#'   source-specific matching tables.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
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
                         ontology = NULL, matchDir = NULL, verbose = FALSE){

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

  temp <- get_concept(x = concepts %>% bind_cols(attributes), na.rm = FALSE, ontology = ontology, mappings = "all") %>%
    unite(col = "has_close_match", label, has_close_match, sep = " | ", na.rm = TRUE, remove = FALSE) %>%
    separate_rows(has_close_match, sep = " \\| ") %>%
    distinct() %>%
    group_by(label, class, id, has_broader, description, has_broader_match, has_exact_match, has_narrower_match) %>%
    summarise(has_close_match = paste0(has_close_match, collapse = " | ")) %>%
    ungroup() %>%
    mutate(has_close_match = if_else(is.na(id), NA_character_, has_close_match))

  # determine those concepts, that are not yet defined in the ontology
  if(!is.null(prevMatches)){

    temp <- temp %>%
      rename(prev_broader_match = has_broader_match,
             prev_close_match = has_close_match,
             prev_exact_match = has_exact_match,
             prev_narrower_match = has_narrower_match) %>%
      full_join(prevMatches, by = c("label", "class", "id", "has_broader", "description")) %>%
      unite(col = "has_broader_match", prev_broader_match, has_broader_match, sep = " | ", na.rm = TRUE) %>%
      unite(col = "has_close_match", prev_close_match, has_close_match, sep = " | ", na.rm = TRUE) %>%
      unite(col = "has_exact_match", prev_exact_match, has_exact_match, sep = " | ", na.rm = TRUE) %>%
      unite(col = "has_narrower_match", prev_narrower_match, has_narrower_match, sep = " | ", na.rm = TRUE) %>%
      na_if(y = "")

    # temp <- prevMatches %>%
    #   filter(class %in% filterClasses) %>%
    #   rename(harmLab = label) %>%
    #   pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
    #                names_to = "match", values_to = "label") %>%
    #   separate_rows(label, sep = " \\| ") %>%
    #   full_join(concepts, by = "label") %>%
    #   mutate(harmLab = if_else(is.na(match), label, harmLab),
    #          label = if_else(is.na(match), NA_character_, label),
    #          match = if_else(is.na(match), "sort_in", match)) %>%
    #   pivot_wider(id_cols = c(harmLab, class, id, has_broader, description), names_from = match,
    #               values_from = label, values_fn = ~paste0(.x, collapse = " | ")) %>%
    #   na_if(y = "NA") %>%
    #   rename(label = harmLab)
    #
    # if("sort_in" %in% colnames(temp)){
    #   temp <- temp %>%
    #     select(-sort_in)
    # }

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
    message("please edit the file '", paste0(matchDir, "/matching.csv"), "'")
    if(verbose){
      message("column description and tips \n\ncode       : filter by this column to jump to the subset you need to edit\nharmonised : concepts to which the new terms should be related \nclass      : the class of harmonised concepts \nclose      : in case a new concept is a close match to the harmonised concept, paste \n             it next to that concept, delimit several concepts with a '|' \nnarrower     : in case a new concept is not the same as any harmonised concept, paste \n             it next to that concept into which it is nested, delimit \n             several concepts with a '|' \nsort_in    : cut out these concepts and sort them either into 'close' or into 'narrower' \n\n-> values that were already successfully matched by previous translations are listed here, \n   however, altering them here doesn't change the ontology. \n\n-> any row that doesn't contain a value in the column 'code' will be discarded. Hence, \n   if you want a value to be ignored, simply don't paste it anywhere. \n\n-> do not change the values in the columns 'code', 'harmonised' and 'class', as they \n   are important to insert the new matches into the ontology. \n\n-> if a term shall be nested into a position that doesn't have a class, (for example, \n   because that class occurrs the first time with this term) first create that nested \n   class with 'new_class()'.")
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
