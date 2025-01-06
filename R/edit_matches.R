
#' Edit matches manually in a csv-table
#'
#' Allows the user to match concepts with an already existing ontology, without
#' actually writing into the ontology, but instead storing the resulting
#' matching table as csv.
#' @param new [`data.frame(.)`][data.frame]\cr the new concepts that shall be
#'   manually matched, includes "label", "class" and "has_broader" columns.
#' @param topLevel [`logical(1)`][logical]\cr whether or not the new concepts
#'   are at the highest level only, i.e., have to be matched without context, or
#'   whether they are contain columns that must be matched within parent
#'   columns.
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concepts.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @param matchDir [`character(1)`][character]\cr the directory where to store
#'   source-specific matching tables.
#' @param stringdist [`logical(1)`][logical]\cr whether or not to use string
#'   distance to find matches (should not be used for large datasets/when a
#'   memory error is shown).
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
#'   assertFileExists testFileExists assertDirectoryExists
#' @importFrom utils tail head
#' @importFrom stringr str_split str_extract
#' @importFrom readr read_csv write_csv cols
#' @importFrom dplyr filter rename full_join mutate if_else select left_join
#'   bind_rows distinct arrange any_of if_any
#' @importFrom tidyselect everything starts_with
#' @importFrom tidyr pivot_longer pivot_wider separate_wider_delim
#'   separate_wider_regex
#' @importFrom tibble add_column
#' @importFrom fuzzyjoin stringdist_left_join
#' @export

edit_matches <- function(new, topLevel, source = NULL, ontology = NULL,
                         matchDir = NULL, stringdist = TRUE, verbose = TRUE,
                         beep = NULL){

  assertDataFrame(x = new)
  assertNames(x = names(new), must.include = c("label", "class", "has_broader"))
  assertLogical(x = topLevel, any.missing = FALSE, len = 1)
  assertCharacter(x = source, len = 1, any.missing = FALSE)
  assertDirectoryExists(x = matchDir, access = "rw")

  sourceFile <- paste0("match_", source, ".rds")
  sourceID <- get_source(label = source, ontology = ontology) %>%
    pull(id)

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
    filter(label %in% new$class)

  if(topLevel){
    joinCols <- c("label", "class")
  } else {
    joinCols <- c("label", "class", "has_broader")
  }

  if(dim(filterClasses)[1] != 0){

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

    # identify level of the target concepts
    new <- new %>%
      mutate(lvl = length(str_split(has_broader, "[.]")[[1]]))

    if(any(ontology@classes$harmonised$label %in% getOption("gazetteer_top"))){
      topLength <- length(str_split(string = ontology@classes$harmonised |> filter(label %in% getOption("gazetteer_top")) |> pull(id), pattern = "[.]")[[1]])
      new <- new %>%
        mutate(top = str_extract(string = has_broader, pattern = paste0("(.[[:digit:]]+){", topLength-1, "}")))

      parentFilter <- unique(new$top)
      withBroader <- NULL
    } else {

      # set a filter in case target concepts have broader concepts
      if(all(new$lvl <= filterClassLevel-1)){
        parentFilter <- unique(new$has_broader)
        withBroader <- NULL
      } else {
        parentFilter <- NA
        withBroader <- "has_broader"
      }

    }

    ignoreClass <- tail(filterClasses, 1)
  } else {
    filterClasses <- get_class(ontology = ontology) %>%
      pull(label)
    parentFilter <- NA
    withBroader <- NULL
    ignoreClass <- head(filterClasses, 1)
  }

  # ontoMatches <- get_concept(label = new$label, class = new$class, has_broader = new$has_broader, ontology = ontology) %>%
  #   rename(harmLab = label) %>%
  #   pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
  #                names_to = "match", values_to = "label")

  # determine previous matches from matching table (and make them long)
  if(testFileExists(paste0(matchDir, sourceFile))){
    dsMatches <- readRDS(file = paste0(matchDir, sourceFile))
  } else {
    dsMatches <- tibble(id = character(), label = character(), class = character(), has_broader = character(), description = character(),
                        has_broader_match = character(), has_close_match = character(), has_exact_match = character(), has_narrower_match = character())
  }

  dsMatchesLong <- dsMatches %>%
    filter(class %in% filterClasses) %>%
    rename(harmLab = label) %>%
    pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                 names_to = "match", values_to = "label") %>%
    separate_longer_delim(cols = label, delim = " | ") |>
    filter(!is.na(label))

  # ignore concepts that were previously tagged 'ignore'
  tempIgnore <- dsMatchesLong |>
    filter(harmLab == "ignore") |>
    select(label, class, harmLab) |>
    distinct()

  new <- new |>
    left_join(tempIgnore, by = c("label", "class")) |>
    filter(!harmLab %in% "ignore") |>
    filter(label != "") |>
    distinct() |>
    select(-harmLab)

  # ... and return an empty object if everything is to ignore
  if(dim(new)[1] == 0){
    out <- tibble(id = character(), has_broader = character(), label = character(), class = character(),
                  description = character(), match = character(), external = character())
    return(out)
  }

  # gather all concepts for the focal data-series (previous matches from
  # matching table and matches that may already be in the ontology) and join
  # with new concepts
  dsConcepts <- dsMatchesLong %>%
    full_join(new |> select(all_of(joinCols)), by = joinCols) %>%
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
  if(!"has_broader_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_broader_match = NA_character_, .after = "description")
  }
  if(!"has_close_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_close_match = NA_character_, .after = "has_broader_match")
  }
  if(!"has_exact_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_exact_match = NA_character_, .after = "has_close_match")
  }
  if(!"has_narrower_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_narrower_match = NA_character_, .after = "has_exact_match")
  }
  dsConcepts <- dsConcepts |>
    select(label, class, id, has_broader, description, has_broader_match, has_close_match, has_exact_match, has_narrower_match)

  # ... and determine which are already included concepts and which are still missing
  inclConcepts <- dsConcepts %>%
    filter(!is.na(id))
  missingConcepts <- dsConcepts %>%
    filter(is.na(id))

  if(dim(missingConcepts)[1] != 0){

    toRelate <- ontology@concepts$harmonised
    if(!any(is.na(parentFilter))){
      toRelate <- make_tree(id = parentFilter, ontology = ontology)
    }

    extConcepts <- ontology@concepts$external %>%
      separate_wider_delim(cols = id, names = c("dataseries", "nr"), delim = "_", cols_remove = FALSE) %>%
      rowwise() %>%
      mutate(label = paste0(label, " [", dataseries, "]")) %>%
      select(external = label, temp = id)

    relate <- toRelate %>%
      filter(class %in% filterClasses) %>%
      filter(!label == "ignore") %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "external") %>%
      separate_rows(external, sep = " \\| ") %>%
      separate_wider_delim(cols = external, names = c("temp"), delim = ".", too_many = "drop") %>%
      left_join(extConcepts, by = "temp") %>%
      group_by(across(all_of(c("id", "label", "class", "has_broader", "description", "match")))) %>%
      summarise(external = paste0(na.omit(external), collapse = " | ")) %>%
      ungroup() %>%
      mutate(external = na_if(external, "")) %>%
      pivot_wider(id_cols = c("id", "label", "class", "has_broader", "description"), names_from = match, values_from = external) %>%
      rowwise() %>%
      mutate(description = paste0(na.omit(c(description,
                                            if_else(!is.na(has_close_match), paste0("close: ", has_close_match), NA),
                                            if_else(!is.na(has_broader_match), paste0("broader: ", has_broader_match), NA),
                                            if_else(!is.na(has_narrower_match), paste0("narrower: ", has_narrower_match), NA))),
                                  collapse = " -- ")) %>%
      mutate(description = na_if(description, "")) %>%
      select(id, label, class, has_broader, description) %>%
      left_join(inclConcepts %>% select(-description), by = c("id", "label", "class", "has_broader")) %>%
      distinct(label, class, id, has_broader, .keep_all = TRUE) |>
      filter(!is.na(class))

    toJoin <- relate %>%
      rename(label_harm = label) %>%
      mutate(label = tolower(label_harm)) %>%
      filter(!is.na(label_harm)) %>%
      filter(class == tail(filterClasses, 1)) |>
      # filter(has_broader %in% parentFilter) |>
      select(-has_broader, -has_broader_match, -has_close_match, -has_exact_match, -has_narrower_match)

    tempJoin <- missingConcepts %>%
      select(label_new = label, has_broader) %>%
      mutate(label = tolower(label_new))

    if(stringdist){
      joined <- stringdist_left_join(x = tempJoin, y = toJoin, by = "label", distance_col = "dist", max_dist = 2) |>
        separate_wider_regex(id, c(id_new = ".*", "[.]", rest = ".*"), cols_remove = FALSE) |>
        filter(has_broader == id_new) |>
        select(-id_new, -rest)
    } else {
      joined <- left_join(x = tempJoin, y = toJoin, by = "label") |>
        separate_wider_regex(id, c(id_new = ".*", "[.]", rest = ".*"), cols_remove = FALSE) |>
        filter(has_broader == id_new) |>
        select(-label, -id_new, -rest) |>
        mutate(label.x = NA, label.y = NA, dist = 0)
    }

    if(!all(is.na(joined$dist))){
      joined <- joined %>%
        select(-label.x, -label.y) %>%
        distinct() %>%
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
        mutate(has_new_close_match = label,
               label = has_0_differences) %>%
        select(label, id, all_of(withBroader), class, has_new_close_match)

      relate <- relate %>%
        left_join(hits, by = c("id", "label", "class", withBroader)) %>%
        rowwise() %>%
        mutate(has_new_close_match = if_else(has_close_match %in% has_new_close_match, NA_character_, has_new_close_match)) %>%
        unite(col = "has_close_match", has_close_match, has_new_close_match, sep = " | ", na.rm = TRUE) %>%
        mutate(across(where(is.character), function(x) na_if(x, "")))

      stillMissing <- joined %>%
        filter(is.na(has_0_differences)) %>%
        select(-has_broader, -has_0_differences, -id, -class) %>%
        group_by(label) %>%
        summarise(across(starts_with("has_"), ~ paste0(na.omit(unique(.x)), collapse = " | "))) %>%
        mutate(across(where(is.character), function(x) na_if(x, ""))) %>%
        ungroup()

      stillMissing <- missingConcepts %>%
        filter(!label %in% hits$has_new_close_match) %>%
        left_join(stillMissing, by = "label")

    } else {
      stillMissing <- missingConcepts
    }

    sortIn <- stillMissing %>%
      # could also include the class of the stillMissing concepts to clarify
      left_join(tibble(label = unique(new$label)), by = "label") %>%
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

      message("-> please edit the file '", paste0(matchDir, "/matching.csv"), "' (at class ", tail(filterClasses, 1), ") \n")
      if(verbose){
        message("--- column description ---\n")
        message("sort_in             cut out these values and sort them either into 'has_broader_match', \n                    'has_exact_match', has_narrower_match or 'has_close_match'")
        message("has_broader         the id of the broader concept of the already harmonised concepts")
        message("id                  the id of concepts to which the new terms should be related")
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
      toIgnore <- related %>%
        filter(id == "ignore") %>%
        mutate(label = "ignore",
               class = ignoreClass,
               has_close_match = sort_in,
               id = str_replace_all(ontology@classes$harmonised$id[ontology@classes$harmonised$label == ignoreClass], pattern = "x", replacement = "0")) %>%
        group_by(id, label, class, has_broader_match, has_exact_match, has_narrower_match) %>%
        summarise(has_close_match = paste0(na.omit(has_close_match), collapse = " | "), .groups = "keep") %>%
        ungroup()

      related <- related %>%
        filter(!is.na(id) & id != "ignore") %>%
        select(-sort_in) %>%
        mutate(description = NA_character_) %>%
        select(-any_of(c("has_0_differences", "has_1_difference", "has_2_differences"))) %>%
        bind_rows(toIgnore)

      if(dim(related)[1] == 0){
        related <- NULL
      }

    } else {
      related <- relate %>%
        mutate(description = NA_character_)
    }

  } else {
    related <- inclConcepts
  }

  if(!is.null(related)){

    matchingTable <- dsMatches %>%
      filter(!id == "ignore") %>%
      bind_rows(related) %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "new_label") %>%
      separate_longer_delim(cols = new_label, delim = " | ") %>%
      distinct() %>%
      pivot_wider(id_cols = c("id", "label", "class", "has_broader", "description"), names_from = match,
                  values_from = new_label, values_fn = ~paste0(na.omit(.x), collapse = " | ")) %>%
      mutate(across(where(is.character), ~na_if(x = ., y = ""))) %>%
      filter(!is.na(has_broader_match) | !is.na(has_close_match) | !is.na(has_narrower_match) | !is.na(has_exact_match)) %>%
      filter(!is.na(id)) %>%
      arrange(id)

    saveRDS(object = matchingTable, file = paste0(matchDir, sourceFile))

    # only return objects that fall within those broader concepts that are in
    # the final matching table at the current class
    new_parentFilter <- matchingTable |>
      filter(class == tail(filterClasses, 1)) |>
      distinct(has_broader) |>
      pull(has_broader)

    if(!all(is.na(new_parentFilter))){
      related <- related %>%
        filter(has_broader %in% new_parentFilter)
    }

    newGrep <- str_replace_all(new$label, c("\\(" = "\\\\(", "\\)" = "\\\\)", "\\*" = "\\\\*",
                                            "\\[" = "\\\\[", "\\]" = "\\\\]"))

    out <- related %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "external") %>%
      separate_longer_delim(cols = external, delim = " | ") %>%
      filter(!is.na(external)) |>
      distinct() |>
      mutate(match = str_replace(string = match, pattern = "has_", replacement = ""),
             match = str_replace(string = match, pattern = "_match", replacement = "")) |>
      filter(external %in% new$label)

  }

  return(out)
}
