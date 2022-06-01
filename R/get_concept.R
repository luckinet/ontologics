#' Get a concept from an ontology
#'
#' @param ... combination of column name and value to filter the column by. The
#'   value to filter by can be provided as regular expression.
#' @param exact [`logical(1)`][logical]\cr
#' @param tree [`logical(1)`][logical]\cr whether or not to output the whole
#'   ontology tree starting from the given search terms.
#' @param missing [`logical(1)`][logical]\cr whether or not to give only those
#'   values that are currently missing from the ontology.
#' @param ontoDir [`character(1)`][character]\cr the path where the ontology in
#'   which to search is stored. In case you are building a point or areal
#'   database and have used the function \code{\link[luckiTools]{start_occurrenceDB}}
#'   or \code{\link[luckiTools]{start_arealDB}}, this path is already stored in
#'   the options (see \code{getOption("onto_path")}).
#' @examples
#' \dontrun{
#' # exact matches
#' get_concept(label_en = "Forest land")
#'
#' # use regular expressions ...
#' get_concept(label_en = "/*orest")
#'
#' # ... and filter more
#' get_concept(label_en = "/*orest", class = "landuse group")
#'
#' # get all concepts that depend on another concept
#' get_concept(label_en = "Forest land", tree = TRUE)
#'
#' # get all labels (and filter with other functions)
#' get_concept(labels = TRUE)
#' }
#'
#' @importFrom checkmate assertFileExists assertLogical testChoice
#' @importFrom tibble as_tibble
#' @importFrom readr read_rds
#' @importFrom tidyselect everything
#' @importFrom rlang quos eval_tidy
#' @importFrom dplyr filter pull select
#' @importFrom stringr str_which str_sub
#' @importFrom magrittr set_names
#' @export

get_concept <- function(..., exact = TRUE, tree = FALSE, missing = FALSE, #labels = FALSE,
                        ontoDir = NULL){

  if(!is.null(ontoDir)){
    assertFileExists(x = ontoDir, access = "rw", extension = "rds")
  } else {
    ontoDir <- getOption("onto_path")
  }

  ontology <- read_rds(file = ontoDir)
  onto <- left_join(ontology$attributes %>% filter(source %in% c("lucki", "external")),
                    ontology$mappings %>% select(-label_en, -class), by = "code") %>%
    select(code, label_en, class, everything())

  assertLogical(x = exact, len = 1, any.missing = FALSE)
  assertLogical(x = tree, len = 1, any.missing = FALSE)

  attrib <- quos(...)

  if(length(attrib) == 0){
    return(onto)
  }

  if(!all(names(attrib) %in% colnames(onto))){
    sbst <- names(attrib) %in% colnames(onto)
    theName <- names(attrib)[!sbst]
    warning(paste0("'", paste0(theName, collapse = ", "), "' is not a column in the ontology and is thus ignored."))
    attrib <- attrib[sbst]
  }

  temp <- data.frame(matrix(ncol = length(colnames(onto)), nrow = 0)) %>%
    as_tibble() %>%
    set_names(colnames(onto))
  empty <- data.frame(matrix(ncol = length(colnames(onto)), nrow = 1, data = NA)) %>%
    as_tibble() %>%
    set_names(colnames(onto))

  for(i in seq_along(attrib)){
    toSearch <- as.character(eval_tidy(attrib[[i]]))
    missTemp <- NULL

    for(j in seq_along(toSearch)){

      pos <- str_which(string = onto[[names(attrib)[i]]], pattern = toSearch[j])

      if(exact){
        if(is.na(toSearch[j]) | toSearch[j] == ""){
          newTab <- empty
        } else {
          if(length(pos) == 0){
            if(missing){
              missTemp <- c(missTemp, toSearch[j])
            } else{
              stop(paste0("the concept '", toSearch[j], "' (", j, ") is not yet defined."))
            }
          } else {
            newTab <- onto[onto[[names(attrib)[i]]] %in% toSearch[j],]
          }
        }
      } else {
        newTab <- onto[pos,]
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

    temp <- get_tree(onto, topID)

  }

  if(missing){
    return(missTemp)
  } else {
    return(temp)
  }
}
