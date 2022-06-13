#' Get the ontology tree
#'
#' @param onto [`tibble(1)`][tibble]\cr the concepts table of an ontology.
#' @param top [`character(1)`][character]\cr the ID from which to derive the
#'   tree.
#' @importFrom dplyr filter pull
#' @export

get_tree <- function(onto, top){

  fin <- NULL
  outIDs <- top
  while(is.null(fin)){
    childID <- onto %>%
      filter(broader %in% top) %>%
      pull(code)
    if(length(childID) != 0){
      top <- childID
      outIDs <- c(outIDs, childID)
    } else {
      fin <- TRUE
    }
  }

  temp <- onto %>%
    filter(code %in% outIDs)

  return(temp)
}
