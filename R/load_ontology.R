#' Load an ontology
#'
#' @param ontoDir [`character(1)`][character]\cr the path where the ontology to
#'   load is stored. It can be omitted in case the option "onto_path" has been
#'   define (see \code{getOption("onto_path")}).
#' @examples
#' load_ontology(system.file("extdata", "territories.rds",
#'                           package = "ontologics"))
#' @importFrom checkmate assertFileExists
#' @importFrom dplyr left_join filter select everything
#' @importFrom readr read_csv read_rds
#' @export

load_ontology <- function(ontoDir){

  if(!is.null(ontoDir)){
    assertFileExists(x = ontoDir, access = "rw", extension = "rds")
  } else {
    ontoDir <- getOption("onto_path")
  }

  temp <- read_rds(ontoDir)

  out <- left_join(temp$attributes %>% filter(source %in% c("harmonised", "imported")),
                   temp$mappings %>% select(-label_en, -class), by = "code") %>%
    select(code, broader, label_en, class, everything())

  return(out)

}
