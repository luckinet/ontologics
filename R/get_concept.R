#' Get a concept in an ontology
#'
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression, if \code{regex =
#'   TRUE}.
#' @param regex [`logical(1)`][logical]\cr if \code{regex = TRUE}, the columns
#'   defined in \code{table} are filtered by \code{\link[stringr]{str_detect}}
#'   on the column values (if you define several, they are combined with an AND
#'   operator), otherwise a \code{\link[dplyr]{left_join}} on the ontology is
#'   carried out.
#' @param external [`logical(1)`][logical]\cr whether or not to return merely
#'   the table of external concepts.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @param table [`character(1)`][character]\cr a table containing all columns (a
#'   subset of "id", "class", "label", "has_broader" and "has_source") of the
#'   ontology that shall be filter by the values in those columns.
#' @param per_class [`logical(1)`][logical]\cr whether ot not to flatten the
#'   ontology before matching \code{table} with the ontology, whereby
#'   \code{table} would contain columns of the classes in the ontology. This can
#'   be useful when concepts are unique only within their parent concepts, so
#'   that unique identification is only possible when they are matched together.
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#' onto <- load_ontology(path = ontoDir)
#'
#' # exact matches from a loaded ontology ...
#' get_concept(label = "FODDER CROPS", ontology = onto)
#'
#' # extract concepts based on regular expressions
#' get_concept(label = "crop", id = ".03$", regex = TRUE, ontology = ontoDir)
#'
#' @return A table of a subset of the ontology according to the values in
#'   \code{...}
#' @importFrom checkmate assertFileExists assertLogical testChoice
#' @importFrom tibble as_tibble
#' @importFrom readr read_rds
#' @importFrom tidyselect everything contains
#' @importFrom tidyr separate_rows separate pivot_longer pivot_wider
#' @importFrom rlang quos eval_tidy := sym as_name
#' @importFrom dplyr filter pull select rename inner_join
#' @importFrom purrr map map_dfc
#' @importFrom stringr str_which str_sub
#' @importFrom magrittr set_names
#' @importFrom utils head
#' @export

get_concept <- function(..., regex = FALSE, external = FALSE, ontology = NULL){
  # table = NULL, per_class = FALSE){

  assertLogical(x = regex, len = 1, any.missing = FALSE)
  assertLogical(x = external, len = 1, any.missing = FALSE)

  if(!inherits(x = ontology, what = "onto")){
    assertFileExists(x = ontology, access = "r", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  if(external){
    toOut <- ontology@concepts$external
    outCols <- c("id", "label", "description")
  } else {
    toOut <- ontology@concepts$harmonised %>%
      pivot_longer(cols = c(has_close_match, has_broader_match, has_narrower_match, has_exact_match), names_to = "match", values_to = "extid") %>%
      separate_rows(extid, sep = " \\| ") %>%
      separate(col = extid, into = c("extid", "certainty"), sep = "[.]") %>%
      left_join(ontology@concepts$external %>% select(extid = id, external_label = label), by = "extid") %>%
      group_by(id, label, description, class, has_broader, match) %>%
      summarise(external_label = paste0(unique(external_label), collapse = " | ")) %>%
      ungroup() %>%
      mutate(external_label = na_if(external_label, "NA")) %>%
      pivot_wider(id_cols = c(id, label, description, class, has_broader), names_from = match, values_from = external_label)

    outCols <- c("id", "label", "description", "class", "has_broader")
  }

  attrib <- quos(..., .named = TRUE)

  # identify attributes that are not in the ontology
  if(!all(names(attrib) %in% colnames(toOut))){
    sbst <- names(attrib) %in% colnames(toOut)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  if(regex){

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(str_detect(toOut[[names(attrib)[i]]], paste0(as_name(attrib[[i]]), collapse = "|")))

    }

  } else {

    for(i in seq_along(attrib)){

      toOut <- toOut %>%
        filter(toOut[[names(attrib)[i]]] %in% eval_tidy(attrib[[i]]))

    }

  }

  out <- toOut %>%
    select(all_of(outCols), everything())

  return(out)



  # assertDataFrame(x = table, null.ok = TRUE)
  # assertLogical(x = per_class, len = 1, any.missing = FALSE)
  #
  #
  # if(external){
  #
  #   if(is.null(table)){
  #     toOut <- ontology@concepts$external
  #   } else {
  #
  #     assertNames(x = names(table), subset.of = c("id", "label", "description", "has_broader", "has_source"))
  #
  #     if(!regex){
  #
  #       toOut <- table %>%
  #         left_join(ontology@concepts$external, by = colnames(table)) %>%
  #         mutate(external = label,
  #                match = "exact",
  #                has_source = "1") %>%
  #         select(external, match, label, id, has_broader, description, has_source)
  #
  #     } else {
  #
  #       toOut <- ontology@concepts$external
  #
  #       for(j in seq_along(table)){
  #         toOut <- toOut %>%
  #           filter(str_detect(string = toOut[[colnames(table)[j]]],
  #                             pattern = paste0(c(table %>% pull(j)), collapse = "|")))
  #       }
  #
  #     }
  #   }
  #
  #
  # } else {
  #
  #   if(is.null(table)){
  #     toOut <- ontology@concepts$harmonised
  #   } else {
  #
  #     if(per_class){
  #
  #       theClasses <- unique(ontology@concepts$harmonised$class)
  #       assertNames(x = names(table), subset.of = theClasses)
  #
  #       flatOnto <- ontology@concepts$harmonised %>%
  #         filter(class == theClasses[1])
  #
  #       for(i in seq_along(theClasses)){
  #         if(i == length(theClasses)) break
  #
  #         temp <- ontology@concepts$harmonised %>%
  #           filter(class == theClasses[i]) %>%
  #           select(id, !!theClasses[i] := label)
  #
  #         child <- ontology@concepts$harmonised %>%
  #           filter(class == theClasses[i + 1]) %>%
  #           select(!!theClasses[i + 1] := label, id = has_broader, new_id = id)
  #
  #         temp <- temp %>%
  #           left_join(child, by = "id")
  #
  #         if(i == 1){
  #           flatOnto <- temp %>%
  #             mutate(!!paste0(theClasses[i], "_id") := id,
  #                    !!paste0(theClasses[i + 1], "_id") := new_id) %>%
  #             select(theClasses[i], paste0(theClasses[i], "_id"), theClasses[i + 1], paste0(theClasses[i + 1], "_id"), new_id)
  #         } else {
  #           flatOnto <- flatOnto %>%
  #             mutate(id = new_id) %>%
  #             select(-new_id) %>%
  #             left_join(temp %>% select(id, theClasses[i + 1], new_id), by = "id") %>%
  #             mutate(!!paste0(theClasses[i + 1], "_id") := new_id)
  #         }
  #       }
  #
  #       flatOnto <- flatOnto %>%
  #         select(-id, -new_id) %>%
  #         select(contains(colnames(table)))
  #
  #       targetClass <- tail(colnames(table), 1)
  #       parentClass <- tail(colnames(table), 2)[1]
  #
  #       toOut <- table %>%
  #         left_join(flatOnto, by = colnames(table)) %>%
  #         distinct() %>%
  #         select(label = {{ targetClass }}, id = paste0(targetClass, "_id")) %>%
  #         mutate(class = targetClass)
  #
  #       parentOut <- table %>%
  #         left_join(flatOnto %>% distinct(!!sym(parentClass), !!sym(paste0(parentClass, "_id"))), by = all_of(parentClass)) %>%
  #         select(label = all_of(targetClass), has_broader = paste0(parentClass, "_id"))
  #
  #       toOut <- ontology@concepts$harmonised %>%
  #         filter(id %in% toOut$id) %>%
  #         full_join(toOut, by = c("id", "label", "class"))
  #
  #       if(all(is.na(toOut$has_broader))){
  #         toOut <- toOut %>%
  #           select(-has_broader) %>%
  #           left_join(parentOut, by = "label") %>%
  #           select(id, label, class, has_broader, everything())
  #       }
  #
  #     } else {
  #
  #       assertNames(x = names(table), subset.of = c("id", "class", "label", "description", "has_broader", "has_source"))
  #
  #       if(!regex){
  #
  #         toOut <- table %>%
  #           left_join(ontology@concepts$harmonised, by = colnames(table)) %>%
  #           mutate(external = label,
  #                  match = "exact",
  #                  has_source = "1") %>%
  #           select(external, match, label, class, id, has_broader, description, has_source)
  #
  #         if("label" %in% names(table)){
  #
  #           subsNames <- colnames(table)[colnames(table) %in% names(ontology@concepts$external)]
  #
  #           extOut <- table %>%
  #             left_join(ontology@concepts$external, by = subsNames) %>%
  #             select(extid = id, extLabel = label, has_source) %>%
  #             filter(!is.na(extid))
  #
  #           if(dim(extOut)[1] != 0){
  #
  #             extOut <- ontology@concepts$harmonised %>%
  #               pivot_longer(cols = c(has_close_match, has_broader_match, has_narrower_match, has_exact_match), names_to = "match", values_to = "extid") %>%
  #               separate_rows(extid, sep = " \\| ") %>%
  #               separate(col = extid, into = c("extid", "certainty"), sep = "[.]") %>%
  #               filter(extid %in% na.omit(extOut$extid)) %>%
  #               left_join(extOut, ., by = "extid") %>%
  #               mutate(match = str_replace_all(match, "has_", ""),
  #                      match = str_replace_all(match, "_match", "")) %>%
  #               select(external = extLabel, match, label, class, id, has_broader, description, has_source)
  #
  #           } else {
  #             extOut <- extOut %>%
  #               select(external = extLabel, has_source)
  #           }
  #
  #           # rename for join
  #           table <-  table %>%
  #             select(external = label, everything()) %>%
  #             distinct()
  #
  #           toOut <- toOut %>%
  #             bind_rows(extOut) %>%
  #             arrange(match) %>%
  #             left_join(table, ., by = colnames(table))
  #         }
  #
  #       } else {
  #
  #         toOut <- ontology@concepts$harmonised
  #
  #         for(j in seq_along(table)){
  #           toOut <- toOut %>%
  #             filter(str_detect(string = toOut[[colnames(table)[j]]],
  #                               pattern = paste0(c(table %>% pull(j)), collapse = "|")))
  #         }
  #
  #         regexpr <- paste0(paste0(names(table), "~", table), collapse = ",")
  #         toOut <- toOut %>%
  #           mutate(external = regexpr,
  #                  match = "regex",
  #                  has_source = "1") %>%
  #           select(external, match, label, class, id, has_broader, description, has_source)
  #       }
  #
  #     }
  #
  #   }
  # }
#
#   return(toOut)

}
