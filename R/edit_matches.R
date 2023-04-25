#' Edit matches manually in a csv-table
#'
#' Allows the user to match concepts with an already existing ontology, without
#' actually writing into the ontology, but instead storing the resulting
#' matching table as csv.
#' @param new [`data.frame(.)`][data.frame]\cr the new concepts that shall be
#'   manually matched.
#' @param target [`data.frame(.)`][data.frame]\cr the attributes, in terms of
#'   columns in the ontology, of new concepts that help to match new and target
#'   concepts manually.
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concepts.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @param matchDir [`character(1)`][character]\cr the directory where to store
#'   source-specific matching tables.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
#' @param beep [`integerish(1)`][integer]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
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
#'
#'   Fuzzy matching is carried out and matches with 0, 1 or 2 differing
#'   charcters are presented in a respective column.
#' @return A table that contains all new matches, or if none of the new concepts
#'   weren't already in the ontology, a table of the already sucessful matches.
#' @importFrom checkmate assertDataFrame assertNames assertCharacter
#'   assertFileExists testFileExists
#' @importFrom utils tail head
#' @importFrom stringr str_split
#' @importFrom readr read_csv write_csv cols
#' @importFrom dplyr filter rename full_join mutate if_else select left_join
#'   bind_rows distinct arrange any_of
#' @importFrom tidyselect everything starts_with
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble add_column
#' @importFrom fuzzyjoin stringdist_left_join
#' @export

edit_matches <- function(new, target = NULL, source = NULL,
                         ontology = NULL, matchDir = NULL, verbose = TRUE,
                         beep = NULL){

  assertCharacter(x = new)
  assertNames(x = names(target), must.include = c("class", "has_broader"))
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

  # get the classes within which to search
  filterClasses <- ontology@classes$harmonised %>%
    filter(label %in% target$class)
  filterClassLevel <- length(str_split(string = filterClasses$id, pattern = "[.]")[[1]])
  if(dim(filterClasses)[1] == 0){
    stop("no classes are matched in the ontology.")
  }
  while(!any(is.na(filterClasses$has_broader))){
    filterClasses <- ontology@classes$harmonised %>%
      filter(label %in% filterClasses$label | label %in% filterClasses$has_broader)
  }
  filterClasses <- filterClasses %>%
    pull(label) %>%
    unique()

  #
  target <- target %>%
    mutate(lvl = length(str_split(has_broader, "[.]")[[1]]))

  if(all(target$lvl < filterClassLevel-1)){
    parentFilter <- unique(target$has_broader)
    withBroader <- NULL
  } else {
    parentFilter <- NULL
    withBroader <- "has_broader"
  }

  temp <- get_concept(label = new, class = target$class, has_broader = target$has_broader, ontology = ontology) %>%
    left_join(tibble(label = new, has_broader = target$has_broader), ., by = c("label", "has_broader"))

  # determine previous matches
  if(testFileExists(paste0(matchDir, sourceFile))){
    prevAvail <- TRUE
    prevMatches <- read_csv(paste0(matchDir, sourceFile), col_types = cols(.default = "c"))

    prevMatchLabels <- prevMatches %>%
      filter(class %in% filterClasses) %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match), values_to = "labels") %>%
      filter(!is.na(labels)) %>%
      distinct(labels) %>%
      separate_rows(labels, sep = " \\| ") %>%
      pull(labels)
  } else {
    prevAvail <- FALSE
    # if no previous matches are present, match the new concepts with the already
    # harmonised concepts in assumption that a match on the term is also a match on
    # the underlying concept
    tempMatch <- temp %>%
      mutate(has_close_match = label) %>%
      select(label, has_close_match, has_broader)
    prevMatches <- ontology@concepts$harmonised %>%
      filter(class %in% filterClasses) %>%
      select(-has_close_match) %>%
      left_join(tempMatch, ., by = c("label", "has_broader")) %>%
      filter(!is.na(id)) %>%
      mutate(has_broader_match = NA_character_,
             has_exact_match = NA_character_,
             has_narrower_match = NA_character_)

    prevMatchLabels <- prevMatches %>%
      distinct(label) %>%
      separate_rows(label, sep = " \\| ") %>%
      pull(label)

    if(dim(prevMatches)[1] == 0){
      prevMatches[1,] <- "ignore"
      prevMatches$class <- filterClasses[1]
    }
  }

  # gather all concepts for the focal data-series (previous matches from
  # matching table and matches that may already be in the ontology) ...
  dsConcepts <- prevMatches %>%
    filter(class %in% filterClasses) %>%
    rename(harmLab = label) %>%
    pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                 names_to = "match", values_to = "label") %>%
    separate_rows(label, sep = " \\| ") %>%
    full_join(temp, by = c("label", "class", "id", "has_broader", "description")) %>%
    mutate(harmLab = if_else(is.na(harmLab), label, harmLab),
           label = if_else(is.na(match), if_else(!is.na(id), label, NA_character_), label),
           match = if_else(is.na(match), if_else(!is.na(id), "has_close_match", "sort_in"), match)) %>%
    pivot_wider(id_cols = c(harmLab, class, id, has_broader, description), names_from = match,
                values_from = label, values_fn = ~paste0(na.omit(.x), collapse = " | ")) %>%
    mutate(across(where(is.character), ~na_if(x = ., y = ""))) %>%
    filter(harmLab != "ignore") %>%
    rename(label = harmLab)

  if("sort_in" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      select(-sort_in)
  }
  if(!"has_close_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_close_match = NA_character_, .after = "description")
  }
  if(!"has_broader_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_broader_match = NA_character_, .after = "has_close_match")
  }
  if(!"has_exact_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_exact_match = NA_character_, .after = "has_broader_match")
  }
  if(!"has_narrower_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_narrower_match = NA_character_, .after = "has_exact_match")
  }

  # ... and determine which are already included concepts and which are still missing
  inclConcepts <- dsConcepts %>%
    filter(!is.na(id))
  missingConcepts <- dsConcepts %>%
    filter(is.na(id) & !label %in% prevMatchLabels)

  if(dim(missingConcepts)[1] != 0){

    if(!is.null(parentFilter)){
      if(!all(is.na(parentFilter))){
        toRelate <- make_tree(id = parentFilter, ontology = ontology)
      } else {
        toRelate <- ontology@concepts$harmonised
      }
    } else {
      toRelate <- ontology@concepts$harmonised
    }
    relate <- toRelate %>%
      select(id, label, class, has_broader) %>%
      filter(class %in% filterClasses) %>%
      left_join(inclConcepts, by = c("id", "label", "class", "has_broader")) %>%
      filter(!is.na(class))

    toJoin <- relate %>%
      rename(label_harm = label) %>%
      mutate(label = tolower(label_harm)) %>%
      # filter(!is.na(label_harm) & class == tail(filterClasses, 1)) %>%
      filter(!is.na(label_harm)) %>%
      select(-has_broader, -has_broader_match, -has_close_match, -has_exact_match, -has_narrower_match)

    joined <- missingConcepts %>%
      select(label_new = label, has_broader) %>%
      mutate(label = tolower(label_new)) %>%
      stringdist_left_join(toJoin, by = "label", distance_col = "dist", max_dist = 2)

    if(!all(is.na(joined$dist))){
      joined <- joined %>%
        select(-label.x, -label.y) %>%
        arrange(dist) %>%
        mutate(dist = paste0("dist_", dist)) %>%
        pivot_wider(names_from = dist, values_from = label_harm)
      if(!"dist_0" %in% colnames(joined)){
        joined <- joined %>%
          add_column(dist_0 = NA_character_, .after = "description")
      }
      if(!"dist_1" %in% colnames(joined)){
        joined <- joined %>%
          add_column(dist_1 = NA_character_, .after = "dist_0")
      }
      if(!"dist_2" %in% colnames(joined)){
        joined <- joined %>%
          add_column(dist_2 = NA_character_, .after = "dist_1")
      }

      joined <- joined %>%
        group_by(label_new, id, class, has_broader) %>%
        summarise(across(starts_with("dist_"), ~ paste0(na.omit(unique(.x)), collapse = " | "))) %>%
        mutate(across(where(is.character), function(x) na_if(x, ""))) %>%
        ungroup() %>%
        select(label = label_new, id, class, has_broader, has_0_differences = dist_0, has_1_difference = dist_1, has_2_differences = dist_2)

      hits <- joined %>%
        filter(!is.na(has_0_differences)) %>%
        mutate(#class = tail(filterClasses, 1),
               has_new_close_match = label,
               label = has_0_differences) %>%
        select(label, id, all_of(withBroader), class, has_new_close_match)

      numbers <- relate %>%
        group_by(label) %>%
        summarise(n = n())

      relate <- relate %>%
        left_join(hits, by = c("id", "label", "class", withBroader)) %>%
        left_join(numbers, by = "label") %>%
        mutate(has_new_close_match = if_else(n > 1, NA_character_, has_new_close_match)) %>%
        mutate(has_new_close_match = if_else(str_detect(has_close_match, has_new_close_match), NA_character_, has_new_close_match)) %>%
        unite(col = "has_close_match", has_close_match, has_new_close_match, sep = " | ", na.rm = TRUE) %>%
        mutate(across(where(is.character), function(x) na_if(x, ""))) %>%
        select(-n)

      missingJoined <- joined %>%
        filter(is.na(has_0_differences))
      stillMissing <- joined %>%
        select(-has_broader, -has_0_differences, -id, -class) %>%
        group_by(label) %>%
        summarise(across(starts_with("has_"), ~ paste0(na.omit(unique(.x)), collapse = " | "))) %>%
        mutate(across(where(is.character), function(x) na_if(x, ""))) %>%
        ungroup()

      missingConcepts <- missingConcepts %>%
        filter(label %in% missingJoined$label) %>%
        left_join(stillMissing, by = "label")

    }

    sortIn <- missingConcepts %>%
      left_join(tibble(label = new), by = "label") %>%
      mutate(sort_in = label,
             label = NA_character_,
             class = NA_character_) %>%
      select(sort_in, names(attributes), id, has_broader, label, class, everything())

    # put together the object that shall be edited by the user ...
    if(dim(sortIn)[1] != 0){

      sortIn %>%
        bind_rows(relate) %>%
        write_csv(file = paste0(matchDir, "/matching.csv"), quote = "all", na = "")

      if(!is.null(beep)){
        beep(sound = beep)
      }

      # ... and make them aware of their duty
      if(prevAvail){
        message("\nprevious matches found for this dataseries, only previously not matched terms are presented")
      } else {
        message("\nno previous matches found for this dataseries, close match with other potentially available terms is presented")
      }

      message("-> please edit the file '", paste0(matchDir, "/matching.csv"), "' \n")
      if(verbose){
        message("--- column description ---\n")
        message("sort_in             cut out these values and sort them either into 'has_broader_match', \n                    'has_exact_match', has_narrower_match or 'has_close_match'")
        message("has_broader         the broader concept id of each of the already harmonised concepts")
        message("id                  filter by this column to jump to the subset you need to edit")
        message("label               concepts to which the new terms should be related")
        message("class               the class of harmonised concepts")
        message("description         the description of each concept")
        message("has_close_match     in case a new concept is a close match to the harmonised concept, paste \n                    it here, delimit several concepts with a '|'")
        message("has_broader_match   in case a new concept is a broader match than the harmonised concept, \n                    paste it here, delimit several concepts with a '|'")
        message("has_narrower_match  in case a new concept is a narrower match than the harmonised concept, \n                    paste it here, delimit several concepts with a '|'")
        message("has_exact_match     in case a new concept is an exact match to the harmonised concept \n                    (which is only the case when it's from the same ontology), paste it \n                    here, delimit several concepts with a '|'")
        message("has_x_differences   in case a new concepts matches via fuzzy matching with any of the already \n                    existing concepts, those concepts are shown in the columns with the \n                    respective number of character differences")
        message("\n--- some useful tips ---")
        message("\n-> values that were already successfully matched by previous translations are listed, \n   however, altering already matched concepts doesn't change the ontology. \n\n-> any row that doesn't contain a value in the column 'id' will be discarded. Hence, \n   if you want a value to be ignored, simply don't paste it anywhere. \n\n-> do not change the values in the columns 'id', 'label' and 'class', as they are \n   important to insert the new matches into the ontology. \n\n-> if a term shall be nested into a position that doesn't have a class, (for example, \n   because that class occurrs the first time with this term) first create that nested \n   class with 'new_class()'.\n")
      }
      done <- readline(" -> press any key when done: ")

      related <- read_csv(paste0(matchDir, "/matching.csv"), col_types = cols(.default = "c"))
      assertNames(x = names(related), must.include = c("sort_in", "has_broader", "id", "label", "class", "description", "has_broader_match", "has_close_match", "has_exact_match", "has_narrower_match"))
      related <- related %>%
        select(-sort_in) %>%
        filter(!is.na(id)) %>%
        select(-any_of(c("has_0_differences", "has_1_difference", "has_2_differences")))

      # if("dist" %in% names(joined)){
      #   if(!all(is.na(joined$dist))){
      #     related <- related %>%
      #       select(-any_of("has_0_differences", "has_1_difference", "has_2_differences"))
      #   }
      # }

      if(dim(related)[1] == 0){
        related <- NULL
      }

    } else {
      related <- relate
    }

  } else {
    related <- inclConcepts
  }

  if(!is.null(related)){

    out <- prevMatches %>%
      filter(!id == "ignore") %>%
      bind_rows(related) %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "new_label") %>%
      separate_rows(new_label, sep = " \\| ") %>%
      distinct() %>%
      group_by(id, has_broader, label, class, description, match) %>%
      summarise(new_label = paste0(na.omit(new_label), collapse = " | "), .groups = "keep") %>%
      ungroup() %>%
      mutate(new_label = na_if(x = new_label, y = "")) %>%
      pivot_wider(id_cols = c(label, class, id, has_broader, description), names_from = match, values_from = new_label) %>%
      distinct() %>%
      filter(!is.na(has_broader_match) | !is.na(has_close_match) | !is.na(has_narrower_match) | !is.na(has_exact_match)) %>%
      filter(!is.na(id)) %>%
      arrange(id)

  }

  write_csv(x = out, file = paste0(matchDir, sourceFile), append = FALSE, na = "")

  return(related)
}
