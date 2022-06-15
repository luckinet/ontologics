#' Get a concept in an ontology
#'
#' @param ... combination of column name and value to filter that column by. The
#'   value to filter by can be provided as regular expression.
#' @param exact [`logical(1)`][logical]\cr whether or not the value in
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
#' get_concept(label_en = "/*crops", exact = FALSE, path = ontoDir)
#'
#' # get all concepts that are nested into another concept
#' get_concept(label_en = "FODDER CROPS", tree = TRUE, path = ontoDir)
#' @return A table of a subset of the ontology according to the values in
#'   \code{...}
#' @importFrom checkmate assertFileExists assertLogical testChoice
#' @importFrom tibble as_tibble
#' @importFrom readr read_rds
#' @importFrom tidyselect everything
#' @importFrom rlang quos eval_tidy
#' @importFrom dplyr filter pull select
#' @importFrom purrr map_lgl
#' @importFrom stringr str_which str_sub
#' @importFrom magrittr set_names
#' @export

get_concept <- function(..., exact = TRUE, tree = FALSE, missing = FALSE, #labels = FALSE,
                        path = NULL){

  if(!is.null(path)){
    assertFileExists(x = path, access = "rw", extension = "rds")
  } else {
    path <- getOption("onto_path")
  }

  ontology <- read_rds(file = path)
  onto <- left_join(ontology$attributes %>% filter(source %in% c("harmonised", "imported")),
                    ontology$mappings %>% select(-label_en, -class), by = "code") %>%
    select(code, label_en, class, everything())
  extConcepts <- ontology$attributes %>% filter(!source %in% c("harmonised", "imported"))

  assertLogical(x = exact, len = 1, any.missing = FALSE)
  assertLogical(x = tree, len = 1, any.missing = FALSE)

  attrib <- quos(...)
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

  temp <- data.frame(matrix(ncol = length(colnames(onto)), nrow = 0, data = NA_character_)) %>%
    as_tibble() %>%
    set_names(colnames(onto))
  empty <- data.frame(matrix(ncol = length(colnames(onto)), nrow = 1, data = NA_character_)) %>%
    as_tibble() %>%
    set_names(colnames(onto))

  for(i in seq_along(attrib)){
    toSearch <- as.character(eval_tidy(attrib[[i]]))
    missTemp <- NULL

    for(j in seq_along(toSearch)){

      pos <- str_which(string = onto[[names(attrib)[i]]], pattern = toSearch[j])
      posExt <- str_which(string = extConcepts[[names(attrib)[i]]], pattern = toSearch[j])

      if(exact){
        if(is.na(toSearch[j]) | toSearch[j] == ""){
          newTab <- empty
        } else {
          if(length(pos) == 0 & length(posExt) == 0){
            if(missing){
              missTemp <- c(missTemp, toSearch[j])
              next
            } else{
              stop(paste0("the concept '", toSearch[j], "' (", j, ") is not yet defined."))
            }
          } else {
            if(length(pos) != 0){
              newTab <- onto[onto[[names(attrib)[i]]] %in% toSearch[j],]
            } else if(length(posExt) != 0){
              extTab <- extConcepts$code[posExt]

              posExt <- map_lgl(seq_along(onto$external), function(ix){
                temp <- str_split(onto$external[ix], ", ")[[1]]
                if(any(temp %in% extTab)){
                  return(TRUE)
                } else {
                  return(FALSE)
                }
              })
              newTab <- onto[posExt,]
            }
          }
        }
      } else {
        newTab <- onto[pos,]
      }

      if(dim(newTab)[1] != 1 & exact){
        warning(paste0("concept ", j, " (", toSearch[j],") has ", dim(newTab)[1], " entries"), call. = FALSE)
      }

      if(j == 1){
        temp <- newTab
      } else {
        temp <- temp %>%
          bind_rows(newTab)
      }

    }

  }

  if(tree){

    topID <- temp %>%
      pull(code) %>%
      unique()

    temp <- make_tree(onto, topID)

  }

  temp <- temp %>%
    select(code, broader, label_en, class, external)

  if(missing){
    return(missTemp)
  } else {
    return(temp)
  }
}
