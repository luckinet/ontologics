#' Get the ontology tree
#'
#' @param concepts [`tibble(1)`][tibble]\cr the concepts table of an ontology.
#' @param topID [][]\cr the ID from which to derive the tree
#' @importFrom dplyr filter pull
#' @export

.get_tree <- function(concepts, topID){

  fin <- NULL
  outIDs <- topID
  while(is.null(fin)){
    childID <- concepts %>%
      filter(broader %in% topID) %>%
      pull(code)
    if(length(childID) != 0){
      topID <- childID
      outIDs <- c(outIDs, childID)
    } else {
      fin <- TRUE
    }
  }

  temp <- concepts %>%
    filter(code %in% outIDs)

  return(temp)
}
