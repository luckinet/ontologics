#' Load an ontology
#'
#' @param path [`character(1)`][character]\cr the path where the ontology to
#'   load is stored. It can be omitted in case the option "onto_path" has been
#'   define (see \code{getOption("onto_path")}).
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#'
#' load_ontology(path = ontoDir)
#' @return A table of the full ontology (i.e., where attribute and mapping
#'   tables are joined).
#' @importFrom checkmate assertFileExists
#' @importFrom dplyr left_join filter select everything
#' @importFrom readr read_csv read_rds
#' @export

load_ontology <- function(path){

  if(!is.null(path)){
    assertFileExists(x = path, access = "rw", extension = "rds")
  } else {
    path <- getOption("onto_path")
  }

  temp <- read_rds(path)

  out <- left_join(temp$attributes %>% filter(source %in% c("harmonised", "imported")),
                   temp$mappings %>% select(-label_en, -class), by = "code") %>%
    select(code, broader, label_en, class, everything())

  return(out)

}
