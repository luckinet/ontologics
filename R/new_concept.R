#' Add a new concept to an ontology
#'
#' This adds a new concept to an existing ontology to semantically integrate and
#' thus harmonise it with the already existing ontology.
#' @param new [`character(.)`][character]\cr the english label(s) of new
#'   concepts that shall be included in the ontology.
#' @param broader [`character(.)`][character]\cr the english label(s) of already
#'   harmonised concepts to which the new concept shall be semantically linked
#'   via a
#'   \href{https://www.w3.org/TR/skos-reference/#semantic-relations}{skos:broader}
#'    relation, see Details.
#' @param class [`character(.)`][character]\cr the class(es) of the new labels.
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concept (for example
#'   \emph{Author+Year}).
#' @param overwrite [`logical(1)`][logical] whether or not to overwrite already
#'   existing concepts.
#' @param attributes [`tibble()`][tibble]\cr not yet implemented.
#' @param path [`character(1)`][character]\cr the path where the ontology in
#'   which to search is stored. It can be omitted in case the option "onto_path"
#'   has been define (see \code{getOption("onto_path")}).
#' @examples
#' ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
#'
#' new_concept(new = c("acacia", "miscanthus"),
#'             broader = c("Bioenergy woody", "Bioenergy herbaceous"),
#'             class = "crop",
#'             source = "external_dataset",
#'             path = ontoDir)
#'
#' load_ontology(path = ontoDir)
#' @return returns invisibly a table of the new harmonised concepts that were
#'   added to the ontology, or a message that nothing new was added.
#' @importFrom checkmate testCharacter testIntegerish assert assertFileExists
#'   assertSubset
#' @importFrom tibble tibble
#' @importFrom dplyr filter pull bind_rows arrange
#' @importFrom stringr str_detect str_split
#' @importFrom readr read_rds write_rds
#' @importFrom utils tail
#' @export

new_concept <- function(new, broader, class = NULL, source, overwrite = FALSE,
                        attributes = NULL, path = NULL){

  if(is.null(new)){
    return("no new concepts to harmonise.")
  }
  newChar <- testCharacter(x = new, ignore.case = FALSE)
  if(!newChar){
    new <- as.character(new)
  }
  isInt <- testIntegerish(x = broader)
  isChar <- testCharacter(x = broader)
  assert(isInt, isChar)
  assertCharacter(x = class, any.missing = FALSE)
  assertCharacter(x = source, any.missing = FALSE)
  assertLogical(x = overwrite, any.missing = FALSE, len = 1)

  if(!is.null(path)){
    assertFileExists(x = path, access = "rw", extension = "rds")
  } else {
    path <- getOption("onto_path")
  }

  ontology <- read_rds(file = path)

  if(length(class) != length(new)){
    if(length(class) == 1){
      class <- rep(x = class, length.out = length(new))
    } else {
      stop("the number of elements in 'class' is neither the same as in 'new' nor 1.")
    }
  }

  if(length(broader) != length(new)){
    if(length(broader) == 1){
      broader <- rep(x = broader, length.out = length(new))
    } else {
      stop("the number of elements in 'broader' is neither the same as in 'new' nor 1.")
    }
  }

  prevID <- str_detect(string = ontology$attributes$source, pattern = source)
  if(!any(prevID)){
    prevID <- 0
  } else {
    prevID <- str_split(string = max(ontology$labels$code[prevID], na.rm = TRUE), pattern = "_")[[1]]
    prevID <- as.numeric(tail(prevID, 1))
    if(is.na(prevID)) prevID <- 0
  }

  newOut <- NULL
  for(i in seq_along(new)){
    newLabel <- new[i]
    newClass <- class[i]

    dups <- grep(pattern = newLabel, x = ontology$attributes$label_en)
    if(length(dups) != 0){
      if(overwrite){
        print(ontology$attributes[dups,])
        continue <- "maybe"
        while(!continue %in% c("yes", "no")){
          continue <- readline(prompt = paste0("the concept '", newLabel, "' has already been defined, define anyway? (yes/no): "))
        }
        if(continue != "yes"){
          next
        }
      } else {
        next
      }
    }

    broaderID <- ontology$attributes %>%
      filter(source == "harmonised")
    assertSubset(x = broader[i], choices = broaderID$label_en, .var.name = paste0("broader[i] (", broader[i], ")"))
    broaderID <- broaderID %>%
      filter(label_en == broader[i]) %>%
      pull(code)

    nestedID <- make_tree(ontology$mappings, broaderID) %>%
      filter(broader == broaderID) %>%
      pull(code)

    if(length(nestedID) == 0){
      nextID <- paste0(broaderID, "01")
    } else {
      nextID <- tail(nestedID, 1)
      tempID <- str_split(nextID, "[.]")[[1]]
      tempID[length(tempID)] <- formatC(as.numeric(tempID[length(tempID)]) + 1, flag = "0", width = nchar(tempID[length(tempID)]))
      nextID <- paste0(tempID, collapse = ".")
    }

    newOut <- tibble(code = as.character(nextID), source = c("imported"),
                     label_en = newLabel, class = newClass)
    ontology$attributes <- ontology$attributes %>%
      bind_rows(newOut) %>%
      arrange(code, source)

    newMapping <- tibble(code = as.character(nextID), broader = as.character(broaderID),
                         label_en = newLabel, class = newClass)
    ontology$mappings <- ontology$mappings %>%
      bind_rows(newMapping) %>%
      arrange(code)

  }

  write_rds(x = ontology, file = path)
  if(!is.null(newOut)){
    invisible(newOut)
  } else {
    message("no new harmonised concepts to add.")
  }

}
