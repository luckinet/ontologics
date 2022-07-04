#' Load an ontology
#'
#' @param path [`character(1)`][character]\cr the path where the ontology to
#'   load is stored.
#' @examples
#' # load an already existing ontology
#' load_ontology(path = system.file("extdata", "crops.rds", package = "ontologics"))
#'
#' @return A table of the full ontology (i.e., where attribute and mapping
#'   tables are joined).
#' @importFrom checkmate assertFileExists assertCharacter
#' @importFrom dplyr left_join filter select everything rowwise group_by
#'   distinct ungroup
#' @importFrom readr read_csv read_rds
#' @export

load_ontology <- function(path = NULL){

  assertFileExists(x = path, access = "r", extension = "rds")

  temp <- read_rds(path)

  if(inherits(x = temp, what = "onto")) {

    out <- temp

  } else {

    message("currently not supported")

  }

  return(out)

}
