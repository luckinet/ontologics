#' Make a tree of an ontology
#'
#' @param top [`tibble(1)`][tibble]\cr the concepts table that shall be at the
#'   top of the tree.
#' @param reverse [`logical(1)`][logical]\cr whether or not to make a tree that
#'   gives the parents, instead of the children, of target concepts.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @importFrom checkmate assertFileExists
#' @importFrom stringr str_split
#' @importFrom dplyr filter pull arrange
#' @export

make_tree <- function(top, reverse = FALSE, ontology = NULL){

  if(!inherits(x = ontology, what = "onto")){
    assertFileExists(x = ontology, access = "r", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  top <- top %>% pull(id)

  fin <- NULL
  outIDs <- top
  input <- ontology@concepts$harmonised %>%
    arrange(id)

  if(reverse){
    while(is.null(fin)){
      parentID <- input %>%
        filter(id %in% top) %>%
        pull(has_broader) %>%
        unique()
      if(all(is.na(parentID))){
        fin <- TRUE
      } else{
        top <- parentID
        outIDs <- c(outIDs, parentID)
      }
    }

  } else {
    while(is.null(fin)){
      childID <- input %>%
        filter(has_broader %in% top) %>%
        pull(id)
      if(length(childID) != 0){
        top <- childID
        outIDs <- c(outIDs, childID)
      } else {
        fin <- TRUE
      }
    }
  }

  temp <- input %>%
    filter(id %in% outIDs)

  return(temp)
}
