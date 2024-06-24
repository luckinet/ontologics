#' Ontology class (S4) and methods
#'
#' @slot sources [`data.frame(.)`][data.frame]\cr
#' @slot classes [`data.frame(.)`][data.frame]\cr
#' @slot concepts [`data.frame(.)`][data.frame]\cr

onto <- setClass(Class = "onto",
                 slots = c(sources = "data.frame",
                           classes = "list",
                           concepts = "list"
                 )
)

setValidity("onto", function(object){

  errors = character()

  # sources
  if(!.hasSlot(object = object, name = "sources")){
    errors = c(errors, "the ontology does not have a 'sources' slot.")
  } else {
    if(!is.data.frame(object@sources)){
      errors = c(errors, "the slot 'sources' is not a data.frame.")
    }
    if(!all(c("id", "label", "description", "homepage", "license", "notes") %in% names(object@sources))){
      errors = c(errors, "the ontology must have a sources-table with the columns 'id', 'label', 'description', 'homepage', 'license' and 'notes'.")
    }
  }


  # classes
  if(!.hasSlot(object = object, name = "classes")){
    errors = c(errors, "the ontology does not have a 'classes' slot.")
  } else {
    if(!is.list(object@classes)){
      errors = c(errors, "the slot 'classes' is not a list.")
    }
    if(!all(names(object@classes) %in% c("harmonised", "external"))){
      errors = c(errors, "the ontology doesn't have the classes-tables 'harmonised' and/or 'external'.")
    }
    if(!is.data.frame(object@classes$harmonised)){
      errors = c(errors, "the the harmonised classes are not in a table.")
    }
    if(!all(c("id", "label", "description", "has_broader", "has_close_match", "has_narrower_match", "has_broader_match", "has_exact_match") %in% names(object@classes$harmonised))){
      errors = c(errors, "the ontology must have a table of harmonised classes with the columns 'id' and 'label', 'description', 'has_broader' and 'has_close_match', 'has_narrower_match'', 'has_broader_match' and 'has_exact_match'.")
    }

    if(!is.data.frame(object@classes$external)){
      errors = c(errors, "the the external classes are not in a table.")
    }
    if(!all(c("id", "label", "description", "has_source") %in% names(object@classes$external))){
      errors = c(errors, "the ontology must have a table of external classes with the columns 'id' and 'label', 'description', 'has_source'.")
    }

  }


  # concepts
  if(!.hasSlot(object = object, name = "concepts")){
    errors = c(errors, "the ontology does not have a 'concepts' slot.")
  } else {
    if(!is.list(object@concepts)){
      errors = c(errors, "the slot 'concepts' is not a list.")
    }
    if(!all(names(object@concepts) %in% c("harmonised", "external"))){
      errors = c(errors, "the ontology doesn't have the concepts-tables 'harmonised' and/or 'external'.")
    }
    if(!is.data.frame(object@concepts$harmonised)){
      errors = c(errors, "the the harmonised concepts are not in a table.")
    }
    if(!is.data.frame(object@concepts$harmonised)){
      errors = c(errors, "the the harmonised concepts are not in a table.")
    }
    if(!all(c("id", "label", "description", "class", "has_broader", "has_close_match", "has_narrower_match", "has_broader_match", "has_exact_match") %in% names(object@concepts$harmonised))){
      errors = c(errors, "the ontology must have a table of harmonised concepts with the columns 'id' and 'label', 'description',  'class', 'has_broader', 'has_close_match', 'has_narrower_match'', 'has_broader_match' and 'has_exact_match'.")
    }

    if(!is.data.frame(object@concepts$external)){
      errors = c(errors, "the the external concepts are not in a table.")
    }
    if(!is.data.frame(object@concepts$external)){
      errors = c(errors, "the the external concepts are not in a table.")
    }
    if(!all(c("id", "label", "description", "has_source") %in% names(object@concepts$external))){
      errors = c(errors, "the ontology must have a table of external concepts with the columns 'id' and 'label', 'description' and 'has_source'.")
    }

  }


  if(length(errors) == 0){
    return(TRUE)
  } else {
    return(errors)
  }

})


#' Print onto in the console
#'
#' @param object object to \code{show}.
#' @importFrom dplyr select mutate if_else group_by summarise rename bind_rows left_join arrange desc filter
#' @importFrom tidyr separate
#' @importFrom purrr map
#' @importFrom stringr str_count str_c
#' @importFrom tibble tibble

setMethod(f = "show",
          signature = "onto",
          definition = function(object){

            # @importFrom crayon yellow red cyan
            theSources <- object@sources
            theConcepts <- object@concepts
            theClasses <- object@classes

            nrSources <- dim(theSources)[1]
            nrClasses <- dim(theClasses$harmonised)[1]
            usedClasses <- length(unique(theConcepts$harmonised$class))
            nrConcepts <- dim(theConcepts$harmonised)[1]
            usedConcepts <- length(unique(theConcepts$harmonised$label))

            classLevels <- theClasses$harmonised %>%
              mutate(clslvls = str_count(string = id, pattern = "[.]"))
            classLevels <- max(classLevels$clslvls)

            lvlChars <- nchar(theClasses$harmonised$id[1])

            itemsPerClass <- theClasses$harmonised %>%
              mutate(level = if_else(nchar(id) == lvlChars, 1, if_else(
                nchar(id) == 2 * lvlChars, 2, if_else(
                  nchar(id) == 3 * lvlChars, 3, if_else(
                    nchar(id) == 4 * lvlChars, 4, if_else(
                      nchar(id) == 5 * lvlChars, 5, NA_real_)))))) %>%
              separate(col = "id", sep = "[.]", into = paste0("id_", 0:classLevels), fill = "right") %>%
              select(-id_0)

            itemsPerSource <- theConcepts$external %>%
              group_by(has_source) %>%
              summarise(items = n()) %>%
              rename(id = has_source) %>%
              bind_rows(tibble(id = "1", items = as.integer(nrConcepts)), .) %>%
              left_join(theSources, by = "id") %>%
              mutate(temp = paste0("'", label, "'", " (", items, ")")) %>%
              arrange(desc(items))

            if(dim(itemsPerSource)[1] > 5){
              sourceList <- paste0("    -> ", paste0(itemsPerSource$temp[1:5], collapse = ", "), ", ...\n\n")
            } else {
              sourceList <- paste0("    -> ", paste0(itemsPerSource$temp, collapse = ", "), "\n\n")
            }

            theConceptsHarm <- theConcepts$harmonised %>%
              mutate(level = if_else(nchar(id) == lvlChars, 1, if_else(
                nchar(id) == 2 * lvlChars, 2, if_else(
                  nchar(id) == 3 * lvlChars, 3, if_else(
                    nchar(id) == 4 * lvlChars, 4, if_else(
                      nchar(id) == 5 * lvlChars, 5, if_else(
                        nchar(id) == 6 * lvlChars, 6, NA_real_))))))) %>%
              separate(col = "id", sep = "[.]", into = paste0("id_", 0:classLevels), fill = "right") %>%
              select(-id_0)

            if(dim(theConceptsHarm)[1] != 0){

              if(usedClasses > 1){
                lvl1Items <- theConceptsHarm %>%
                  filter(!is.na(id_2))
                lvl1Items <- dim(lvl1Items)[1]
              } else {
                lvl1Items <- dim(theConceptsHarm)[1]
              }

              if(lvl1Items < getOption("width")){
                graphWidth <- lvl1Items
              } else {
                graphWidth <- getOption("width") - 2
              }

              itemsPerClass <- theConceptsHarm %>%
                group_by(class) %>%
                summarise(items = n()) %>%
                left_join(itemsPerClass, ., by = c("label" = "class")) %>%
                mutate(items = if_else(is.na(items), 0L, items))

              indent1 <- 2 + itemsPerClass$level
              className <- paste0(strrep(" ", indent1), "\u221F ", theClasses$harmonised$label)

              indent2 <- max(nchar(className)) - nchar(className)
              indent2 <- 3 + (max(nchar(itemsPerClass$items), na.rm = TRUE) - nchar(itemsPerClass$items)) + indent2

              defInd <- graphWidth - nchar(itemsPerClass$items) - max(indent2) - nchar(className)

              definitions <- tibble(def1 = str_sub(string = theClasses$harmonised$description, start = 1, end = defInd),
                                    def2 = theClasses$harmonised$description,
                                    len = nchar(def2),
                                    defInd = defInd,
                                    out = if_else(len > defInd, str_c(def1, "..."), def1)) %>%
                pull(out)

              classList <- paste0(className, strrep(" ", indent2), itemsPerClass$items, "   ", definitions)
              classList <- paste0(paste0(classList, collapse = "\n"), "\n\n")

              conceptList <- conceptChart <- ""
              conceptClass <- theClasses$harmonised$label[1:3]
              for(i in 1:3){

                itemsPerGroup <- theConceptsHarm %>%
                  group_by(!!sym(paste0("id_", i))) %>%
                  summarise(items = n()) %>%
                  mutate(prop =  round(items / sum(items) * graphWidth, 0))

                temp <- theConceptsHarm %>%
                  filter(level == i) %>%
                  left_join(itemsPerGroup, by = paste0("id_", i)) %>%
                  mutate(temp = paste0("'", label, "'", " (", items, ")")) %>%
                  arrange(desc(items))

                if(dim(temp)[1] > 5){
                  conceptList <- paste0(conceptList, "    -> ", conceptClass[i], ": ", paste0(temp$temp[1:5], collapse = ", "), ", ...\n")
                } else {
                  conceptList <- paste0(conceptList, "    -> ", conceptClass[i], ": ", paste0(temp$temp, collapse = ", "), "\n")
                }

                # ticks <- map(seq_along(temp$prop), function(ix){
                #   paste0(rep("-", temp$prop[ix]-1), collapse = "")
                # })
                # ticks <- paste0(ticks, collapse = "|")

                # conceptChart <- paste0(conceptChart, "   ", paste0("|", ticks, "|"))

              }

              # conceptList <- paste0(conceptList, conceptChart)
              conceptList <- paste0(conceptList)

            } else {

              conceptList <- "\n"
              classList <- "\n"
              if(is.na(theClasses$harmonised$label)) nrClasses <- 0

            }

            cat(paste0("  sources : ", nrSources, "\n"))
            cat(sourceList)
            cat("  classes :", nrClasses, "\n")
            cat(classList)
            cat("  top concepts:", nrConcepts, "\n")
            cat(conceptList)

          }
)
