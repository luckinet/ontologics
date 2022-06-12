#' Check various aspects of an ontology
#'
#' @param ontology [`ontology`][ontology]\cr the LUCKINet ontology.
#' @details \itemize{ \item ensure there are no duplicates (especially
#'   combinations where the only difference is a luckinetID) \item ensure that
#'   unique term+class combinations have only one unique luckinetID \item ensure
#'   that none of them have a missing luckinetID}
#' @return \code{list} the erroneous aspects are listed.
#' @importFrom checkmate assertDataFrame assertNames
#' @importFrom dplyr select contains group_by summarise n_distinct filter
#'   arrange desc pull n
#' @importFrom tibble tibble
#' @export

# validate_ontology <- function(ontology){

  # assertDataFrame(x = ontology, min.cols = 5)
  # assertNames(x = names(ontology), must.include = c("term_from", "class_from", "luckinetID_from", "relationship", "term_to", "class_to", "luckinetID_to"))
  #
  # issues <- list(duplicated_rows = NULL,
  #                duplicated_ID = list(term_from = NULL, term_to = NULL),
  #                missing_ID = list(term_from = NULL, term_to = NULL))
  # printErrors <- FALSE
  #
  # # ensure there are no duplicates (especially combinations where the only difference is a luckinetID)
  # simpleDup <- ontology %>%
  #   duplicated()
  # if(any(simpleDup)){
  #   printErrors <- TRUE
  #   warning("the ontology contains ", sum(simpleDup), " fully duplicated rows.")
  #   issues$duplicated_rows <- c(which(simpleDup))
  # }
  #
  # # termsDup <- ontology %>%
  # #   select(-contains("luckinetID")) %>%
  # #   duplicated()
  #
  # # ensure that unique term+class combinations have only one unique luckinetID
  # concepts_from <- ontology %>%
  #   select(term_from, class_from, luckinetID_from) %>%
  #   group_by(term_from, class_from) %>%
  #   summarise(unique_IDs = n_distinct("luckinetID_from"))
  # if(any(concepts_from$unique_IDs > 1)){
  #   printErrors <- TRUE
  #   temp <- concepts_from$term_from[which(concepts_from$unique_IDs > 1)]
  #   ontology %>%
  #     filter(term_from %in% temp)
  #   warning(paste0("the following value(s) in 'term_from' have several luckinetIDs: ", paste0(temp, collapse = ", ")))
  #   issues$duplicated_ID$term_from <- temp
  # }
  #
  # concepts_to <- ontology %>%
  #   select(term_to, class_to, luckinetID_to) %>%
  #   group_by(term_to, class_to) %>%
  #   summarise(unique_IDs = n_distinct("luckinetID_to", na.rm = TRUE))
  # if(any(concepts_to$unique_IDs > 1)){
  #   printErrors <- TRUE
  #   temp <- concepts_to$term_to[which(concepts_to$unique_IDs > 1)]
  #   warning(paste0("the following value(s) in 'term_to' have several luckinetIDs: ", paste0(temp, collapse = ", ")))
  #   issues$duplicated_ID$term_to <- temp
  # }
  #
  # # ensure that none of them have a missing luckinetID
  # missing_IDs <- ontology %>%
  #   select(term_from, class_from, luckinetID_from) %>%
  #   filter(is.na(luckinetID_from))
  # if(dim(missing_IDs)[1] != 0){
  #   printErrors <- TRUE
  #   temp <- unique(missing_IDs$term_from)
  #   if(length(temp) > 20){
  #     warning("the ontology contains ", sum(length(temp)), " value(s) in 'term_from' that have no luckinetIDs.")
  #   } else {
  #     warning(paste0("the following value(s) in 'term_from' have no luckinetIDs: ", paste0(temp, collapse = ", ")))
  #   }
  #   issues$missing_ID$term_from <- temp
  # }
  #
  # missing_IDs <- ontology %>%
  #   select(term_to, class_to, luckinetID_to) %>%
  #   filter(class_to != "fao_definition") %>%
  #   filter(class_to != "faoID") %>%
  #   filter(class_to != "esaID") %>%
  #   filter(is.na(luckinetID_to))
  #
  # if(dim(missing_IDs)[1] != 0){
  #   printErrors <- TRUE
  #   tabMis <- table(missing_IDs$term_to)
  #   temp <- tibble(term = names(tabMis), n = tabMis) %>%
  #     arrange(desc(n)) %>%
  #     pull(term)
  #   if(length(temp) > 20){
  #     warning("the ontology contains ", sum(length(temp)), " value(s) in 'term_to' that have no luckinetIDs.")
  #   } else {
  #     warning(paste0("the following value(s) in 'term_to' have no luckinetIDs: ", paste0(temp, collapse = ", ")))
  #   }
  #   issues$missing_ID$term_to <- temp
  # }
  #
  # if(printErrors){
  #   return(issues)
  # } else {
  #   return(ontology)
  # }
# }
