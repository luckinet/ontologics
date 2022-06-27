#' Ontology class (S4) and methods
#'
#'
#'
#' @slot classes [`data.frame(.)`][data.frame]\cr
#' @slot sources [`data.frame(.)`][data.frame]\cr
#' @slot concepts [`data.frame(.)`][data.frame]\cr
#' @slot labels [`data.frame(.)`][data.frame]\cr
#' @slot mappings [`character(1)`][character]\cr

onto <- setClass(Class = "onto",
                 slots = c(classes = "data.frame",
                           sources = "data.frame",
                           concepts = "data.frame",
                           labels = "data.frame",
                           mappings = "data.frame"
                 )
)

setValidity("onto", function(object){

  errors = character()

  if(!.hasSlot(object = object, name = "classes")){
    errors = c(errors, "the geom does not have a 'classes' slot.")
  } else {
    if(!is.data.frame(object@classes)){
      errors = c(errors, "the slot 'classes' is not a data.frame.")
    }
    if(!all(c("level", "class") %in% names(object@classes))){
      errors = c(errors, "the ontology must have a classes-table with the columns 'level' and 'class'.")
    }
  }

  if(!.hasSlot(object = object, name = "sources")){
    errors = c(errors, "the geom does not have a 'sources' slot.")
  } else {
    if(!is.data.frame(object@sources)){
      errors = c(errors, "the slot 'sources' is not a data.frame.")
    }
    if(!all(c("sourceID", "sourceName", "description", "homepage", "license", "notes") %in% names(object@sources))){
      errors = c(errors, "the ontology must have a sources-table with the columns 'sourceID', 'sourceName', 'description', 'homepage', 'license' and 'notes'.")
    }
  }

  if(!.hasSlot(object = object, name = "concepts")){
    errors = c(errors, "the geom does not have a 'concepts' slot.")
  } else {
    if(!is.data.frame(object@concepts)){
      errors = c(errors, "the slot 'concepts' is not a data.frame.")
    }
    if(!all(c("code", "broader", "sourceID") %in% names(object@concepts))){
      errors = c(errors, "the ontology must have a concepts-table with the columns 'code', 'broader' and 'sourceID'.")
    }
  }

  if(!.hasSlot(object = object, name = "labels")){
    errors = c(errors, "the geom does not have a 'labels' slot.")
  } else {
    if(!is.data.frame(object@labels)){
      errors = c(errors, "the slot 'labels' is not a data.frame.")
    }
    if(!all(c("code", "class", "label_en") %in% names(object@labels))){
      errors = c(errors, "the ontology must have a labels-table with the columns 'code', 'class' and 'label_en'.")
    }
  }

  if(!.hasSlot(object = object, name = "mappings")){
    errors = c(errors, "the geom does not have a 'mappings' slot.")
  } else {
    if(!is.data.frame(object@mappings)){
      errors = c(errors, "the slot 'mappings' is not a data.frame.")
    }
    if(!all(c("code", "external") %in% names(object@mappings))){
      errors = c(errors, "the ontology must have a mappings-table with the columns 'code' and 'external'.")
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
          }
)
