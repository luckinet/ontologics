#' Get a concept in an ontology
#'
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression.
#' @param regex [`logical(1)`][logical]\cr whether or not the value in
#'   \code{...} shall be matched in full, or whether any partial match should be
#'   returned.
#' @param tree [`logical(1)`][logical]\cr whether or not to output the whole
#'   ontology tree starting from the given search terms.
#' @param missing [`logical(1)`][logical]\cr whether or not to give only those
#'   values that are currently missing from the ontology.
#' @param path [`character(1)`][character]\cr the path where the ontology in
#'   which to search is stored. It can be omitted in case the option "onto_path"
#'   has been define (see \code{getOption("onto_path")}).
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#'
#' # exact matches
#' get_concept(label_en = "FODDER CROPS", path = ontoDir)
#'
#' # use regular expressions ...
#' get_concept(label_en = "/*crops", regex = TRUE, path = ontoDir)
#' get_concept(label_en = "/*crops", broader = "_05", regex = TRUE, path = ontoDir)
#'
#' # get all concepts that are nested into another concept
#' get_concept(label_en = "FODDER CROPS", tree = TRUE, path = ontoDir)
#' @return A table of a subset of the ontology according to the values in
#'   \code{...}
#' @importFrom checkmate assertFileExists assertLogical testChoice
#' @importFrom tibble as_tibble
#' @importFrom readr read_rds
#' @importFrom tidyselect everything
#' @importFrom rlang exprs eval_tidy := sym
#' @importFrom dplyr filter pull select
#' @importFrom purrr map map_dfc
#' @importFrom stringr str_which str_sub
#' @importFrom magrittr set_names
#' @export

get_concept <- function(..., regex = FALSE, tree = FALSE, missing = FALSE,
                        path = NULL){

  if(!is.null(path)){
    assertFileExists(x = path, access = "rw", extension = "rds")
  } else {
    path <- getOption("onto_path")
  }

  assertLogical(x = regex, len = 1, any.missing = FALSE)
  assertLogical(x = tree, len = 1, any.missing = FALSE)
  assertLogical(x = missing, len = 1, any.missing = FALSE)
  if(regex & missing){
    stop("you can only search for missing items with 'regex = FALSE'.")
  }

  ontology <- read_rds(file = path)
  onto <- left_join(ontology$attributes,
                    ontology$mappings %>% select(-label_en, -class), by = "code") %>%
    select(code, label_en, class, everything())

  attrib <- exprs(..., .named = TRUE)
  # return(attrib)

  if(length(attrib) == 0){
    return(onto)
  }

  # identify attributes that are not in the ontology
  if(!all(names(attrib) %in% colnames(onto))){
    sbst <- names(attrib) %in% colnames(onto)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  # temp <- data.frame(matrix(ncol = length(colnames(onto)), nrow = 0, data = NA_character_)) %>%
  #   as_tibble() %>%
  #   set_names(colnames(onto))
  # empty <- data.frame(matrix(ncol = length(colnames(onto)), nrow = 1, data = NA_character_)) %>%
  #   as_tibble() %>%
  #   set_names(colnames(onto))

  codes <- map(.x = seq_along(attrib), .f = function(ix){
    if(regex){

      onto %>%
        filter(str_detect(onto[[names(attrib)[ix]]], paste0(attrib[[ix]], collapse = "|"))) %>%
        pull(code)

    } else{

      onto %>%
        filter(!!sym(names(attrib)[ix]) %in% attrib[[ix]]) %>%
        pull(code)

    }
  })

  codes <- Reduce(intersect, codes)

  temp <- onto %>%
    filter(code %in% codes)

  # if missing = TRUE (regex must be FALSE) and thus we can join the input
  # concepts with the temp output  to find which concepts are missing
  if(missing){

    temp <- map_dfc(.x = seq_along(attrib), .f = function(ix){

      tibble(!!sym(names(attrib)[ix]) := attrib[[ix]])

    }) %>%
      expand.grid() %>%
      as_tibble() %>%
      left_join(temp, by = names(attrib)) %>%
      filter(is.na(code))

  } else {

    if(tree){

      topID <- temp %>%
        pull(code) %>%
        unique()

      temp <- make_tree(onto, topID)

    }

  }

  out <- temp %>%
    select(code, broader, label_en, class, external, source)

  # for(i in seq_along(attrib)){
  #   toSearch <- as.character(eval_tidy(attrib[[i]]))
  #   outMiss <- NULL
  #
  #   for(j in seq_along(toSearch)){
  #
  #     pos <- str_which(string = onto[[names(attrib)[i]]], pattern = toSearch[j])
  #     posExt <- str_which(string = extConcepts[[names(attrib)[i]]], pattern = toSearch[j])
  #
  #     if(regex){
  #       if(is.na(toSearch[j]) | toSearch[j] == ""){
  #         newTab <- empty
  #       } else {
  #         if(length(pos) == 0 & length(posExt) == 0){
  #           if(missing){
  #             outMiss <- c(outMiss, toSearch[j])
  #             next
  #           } else{
  #             stop(paste0("the concept '", toSearch[j], "' (", j, ") is not yet defined."))
  #           }
  #         } else {
  #           if(length(pos) != 0){
  #             newTab <- onto[onto[[names(attrib)[i]]] %in% toSearch[j],]
  #           } else if(length(posExt) != 0){
  #             extTab <- extConcepts$code[posExt]
  #
  #             posExt <- map_lgl(seq_along(onto$external), function(ix){
  #               temp <- str_split(onto$external[ix], ", ")[[1]]
  #               if(any(temp %in% extTab)){
  #                 return(TRUE)
  #               } else {
  #                 return(FALSE)
  #               }
  #             })
  #             newTab <- onto[posExt,]
  #           }
  #         }
  #       }
  #     } else {
  #       newTab <- onto[pos,]
  #     }
  #
  #     if(dim(newTab)[1] != 1 & regex){
  #       warning(paste0("concept ", j, " (", toSearch[j],") has ", dim(newTab)[1], " entries"), call. = FALSE)
  #     }
  #
  #     if(j == 1){
  #       temp <- newTab
  #     } else {
  #       temp <- temp %>%
  #         bind_rows(newTab)
  #     }
  #
  #   }
  #
  # }

  return(out)

}
