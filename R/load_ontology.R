#' Load a gazetteer
#'
#' @param ontoDir [`character(1)`][character]\cr the path where the ontology to
#'   load is stored. It can be omitted in case the option "onto_path" has been
#'   define (see \code{getOption("onto_path")}).
#' @details A \href{https://en.wikipedia.org/wiki/Gazetteer}{gezatteer} is a
#'   directory "that contains information concerning the geographical makeup,
#'   social statistics and physical features of a country, region, or
#'   continent", or in the case here, additionally the ontological relations of
#'   the areal data.
#' @importFrom tools file_ext
#' @importFrom readr read_csv read_rds
#' @export

load_ontology <- function(ontoDir){

  if(!is.null(ontoDir)){
    assertFileExists(x = ontoDir, access = "rw", extension = "rds")
  } else {
    ontoDir <- getOption("onto_path")
  }

  if(file_ext(ontoDir) == "csv"){
    temp <- read_csv(ontoDir, col_types = "ccccic")
  } else {
    temp <- read_rds(ontoDir)
  }

  out <- left_join(temp$attributes %>% filter(source %in% c("harmonised", "imported")),
                   temp$mappings %>% select(-label_en, -class), by = "code") %>%
    select(code, broader, label_en, class, everything())

  return(out)

}
