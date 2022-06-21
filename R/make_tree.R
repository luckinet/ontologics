#' Make a tree of an ontology
#'
#' @param input [`tibble(1)`][tibble]\cr the concepts table of an ontology.
#' @param top [`character(1)`][character]\cr the ID from which to derive the
#'   tree.
#' @importFrom dplyr filter pull arrange

make_tree <- function(input, top){

  fin <- NULL
  outIDs <- top
  input <- input %>%
    arrange(code)
  while(is.null(fin)){
    childID <- input %>%
      filter(broader %in% top) %>%
      pull(code)
    if(length(childID) != 0){
      top <- childID
      outIDs <- c(outIDs, childID)
    } else {
      fin <- TRUE
    }
  }

  temp <- input %>%
    filter(code %in% outIDs)

  return(temp)
}
