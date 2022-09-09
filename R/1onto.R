#' Ontology class (S4) and methods
#'
#'
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

setMethod(f = "show",
          signature = "onto",
          definition = function(object){

            # str(object)
            # @importFrom crayon yellow red cyan
            # theType <- object@type
            # thePoints <- getPoints(x = object)
            # theFeatures <- getFeatures(x = object)
            # theGroups <- getGroups(x = object)
            #
            # vertAttribs <- length(thePoints)
            # featureAttribs <- names(theFeatures)[!names(theFeatures) %in% c("fid")]
            # groupAttribs <- names(theGroups)[!names(theGroups) %in% c("gid")]
            #
            # myAttributes <- NULL
            # points <- feats <- groups <- FALSE
            #
            # if(is.na(object@crs)){
            #   myCrs <- "cartesian"
            # } else {
            #   myCrs <- object@crs
            # }
            #
            # if(theType == "grid"){
            #   theFeats <- featureAttribs
            #   theLayer <- theGroups
            #   if(!is.null(theLayer)){
            #     if(!all(names(thePoints) %in% c("gid"))){
            #       myAttributes <- c(myAttributes, paste0(" ", ifelse(length(groupAttribs) == 0,
            #                                                          paste0("--\n"),
            #                                                          ifelse(length(groupAttribs) <= 9,
            #                                                                 paste0(paste0(groupAttribs, collapse = ", "), "\n"),
            #                                                                 paste0(paste0(c(head(groupAttribs, 9), "..."), collapse = ", "), "\n"))
            #       )))
            #     }
            #   }
            #
            #   if(length(unique(groupAttribs)) == 1){
            #     myFeat <- "layer"
            #   } else {
            #     myFeat <- "layers"
            #   }
            #   myUnits <- "cells"
            #   geomGroups <- ""
            #
            # } else {
            #   theGrps <- theGroups$gid
            #   if(length(unique(theGrps)) == 1){
            #     myGrp <- "group"
            #   } else {
            #     myGrp <- "groups"
            #   }
            #   theFeats <- theFeatures$fid
            #   featureAttribs <- featureAttribs[-which(featureAttribs == "gid")]
            #   if(length(unique(theFeats)) == 1){
            #     myFeat <- "feature"
            #   } else {
            #     myFeat <- "features"
            #   }
            #   myUnits <- "points"
            #   geomGroups <- paste0(length(unique(theGrps)), " ", myGrp, " | ")
            #
            #   if(!all(names(thePoints) %in% c("x", "y", "fid"))){
            #     myAttributes <- c(myAttributes, paste0(" (points) ",
            #                                            ifelse(vertAttribs <= 9,
            #                                                   paste0(paste0(names(thePoints)[!names(thePoints) %in% c("x", "y", "fid")], collapse = ", "), "\n"),
            #                                                   paste0(paste0(c(head(names(thePoints)[!names(thePoints) %in% c("x", "y", "fid")], 9), "..."), collapse = ", "), "\n")
            #                                            )))
            #     points <- TRUE
            #   }
            #   if(!all(names(theFeatures) %in% c("fid", "gid"))){
            #     if(points){
            #       featureString <- "           (features) "
            #     } else {
            #       featureString <- " (features) "
            #     }
            #     myAttributes <- c(myAttributes, paste0(featureString,
            #                                            ifelse(length(featureAttribs) <= 9,
            #                                                   paste0(paste0(featureAttribs, collapse = ", "), "\n"),
            #                                                   paste0(paste0(c(head(featureAttribs, 9), "..."), collapse = ", "), "\n")
            #                                            )))
            #     feats <- TRUE
            #   }
            #   if(!all(names(theGroups) %in% c("gid"))){
            #     if(feats | points){
            #       groupString <- "            (groups) "
            #     } else {
            #       groupString <- " (groups) "
            #     }
            #     myAttributes <- c(myAttributes, paste0(groupString,
            #                                            ifelse(length(groupAttribs) <= 9,
            #                                                   paste0(paste0(names(theGroups)[!names(theGroups) %in% c("gid")], collapse = ", "), "\n"),
            #                                                   paste0(paste0(c(head(names(theGroups)[!names(theGroups) %in% c("gid")], 9), "..."), collapse = ", "), "\n")
            #                                            )))
            #   }
            # }
            # if(is.null(myAttributes)){
            #   myAttributes <- " --\n"
            # }
            #
            #
            # cat(yellow(class(object)), "        ", object@type, "\n", sep = "")
            # cat("            ", geomGroups, length(unique(theFeats)), " ", myFeat, " | ", length(thePoints$fid), " ", myUnits, "\n", sep = "")
            # cat(yellow("crs         "), myCrs, "\n", sep = "")
            # cat(yellow("attributes "), myAttributes, sep = "")
            # if(!theType == "grid"){
            #   # make a tiny map
            #   tinyMap <- .makeTinyMap(geom = object)
            #   cat(yellow("tiny map  "), tinyMap)
            # } else {
            #   theRes <- getRes(object)
            #   theExt <- getExtent(object)
            #   cat(yellow("resolution "), as.numeric(theRes), "(x, y)\n")
            #   cat(yellow("extent     "), c(theExt$x, theExt$y), "(xmin, xmax, ymin, ymax)")
            # }
            theSources <- object@sources
            theConcepts <- object@concepts
            theClasses <- object@classes

            nrSources <- dim(theSources)[1]
            nrClasses <- dim(theClasses$harmonised)[1]
            usedClasses <- length(unique(theConcepts$harmonised$class))
            nrConcepts <- dim(theConcepts$harmonised)[1]

            itemsPerSource <- theConcepts$external %>%
              group_by(has_source) %>%
              summarise(items = n()) %>%
              rename(id = has_source) %>%
              bind_rows(tibble(id = "1", items = as.integer(nrConcepts)), .) %>%
              left_join(theSources, by = "id") %>%
              mutate(temp = paste0("'", label, "'", " (", items, ")")) %>%
              arrange(desc(items))

            if(dim(itemsPerSource)[1] > 3){
              sourceList <- paste0("    -> ", paste0(itemsPerSource$temp[1:3], collapse = ", "), ", ...\n\n")
            } else {
              sourceList <- paste0("    -> ", paste0(itemsPerSource$temp, collapse = ", "), "\n\n")
            }

            itemsPerGroup <- theConcepts$harmonised %>%
              separate(col = "id", sep = "[.]", into = paste0("id_", 0:usedClasses), fill = "right") %>%
              select(-id_0)

            if(usedClasses > 1){
              items1 <- itemsPerGroup %>%
                filter(!is.na(id_2))
            } else {
              items1 <- itemsPerGroup
            }
            items1 <- items1 %>%
              group_by(id_1) %>%
              summarise(items_1 = n()) %>%
              mutate(props_1 =  items_1 / sum(items_1) * 100)

            # "\u221F"
            cat(paste0("  sources : ", nrSources, "\n"))
            cat(sourceList)

            cat("  classes :", nrClasses, "\n")
            cat("  concepts:", nrConcepts, "\n")



          }
)
