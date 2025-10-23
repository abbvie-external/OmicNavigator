# Careful! This function is optimized for speed. All safety checks are assumed
# to have already been applied.
#
# Assumptions: set1/set2 are character vectors with no duplicated elements
#
# https://en.wikipedia.org/wiki/Overlap_coefficient
# https://en.wikipedia.org/wiki/Jaccard_index
calc_overlap <- function(set1, set2) {
  set_intersect <- set1[match(set2, set1, 0L)]
  n_intersect <- length(set_intersect)
  # set_union <- .Internal(unique(c(set1, set2), incomparables = FALSE, fromLast = FALSE, nmax = NA))
  set_union <- unique(c(set1, set2))
  n_union <- length(set_union)
  jaccard <- n_intersect / n_union
  overlap <- n_intersect / min(length(set1), length(set2))
  return(list(overlapSize = n_intersect, overlap = overlap, jaccard = jaccard))
}

is_unique <- function(x) length(unique(x)) == length(x)

calc_pairwise_overlaps <- function(sets) {
  # Check assumptions on the sets, so that they don't need to be checked again
  # when performing each pairwise overlap.
  sets_are_vectors <- vapply(sets, is.vector, logical(1))
  if (any(!sets_are_vectors)) {
    stop("Sets must be vectors")
  }
  sets_are_atomic <- vapply(sets, is.atomic, logical(1))
  if (any(!sets_are_atomic)) {
    stop("Sets must be atomic vectors, i.e. not lists")
  }
  sets <- lapply(sets, as.character)
  sets_are_unique <- vapply(sets, is_unique, logical(1))
  if (any(!sets_are_unique)) {
    stop("Sets must be unique, i.e. no duplicated elements")
  }

  names_sets <- names(sets)
  n_sets <- length(sets)
  n_overlaps <- choose(n = n_sets, k = 2)
  vec_term1 <- character(length = n_overlaps)
  vec_term2 <- character(length = n_overlaps)
  vec_overlapSize <- integer(length = n_overlaps)
  vec_overlap <- numeric(length = n_overlaps)
  vec_jaccard <- numeric(length = n_overlaps)
  overlaps_index <- 1
  for (i in seq(n_sets - 1)) {
    term1 <- names_sets[i]
    set1 <- sets[[i]]
    for (j in seq(i + 1, n_sets)) {
      term2 <- names_sets[j]
      set2 <- sets[[j]]
      overlap_list <- calc_overlap(set1, set2)
      vec_term1[overlaps_index] <- term1
      vec_term2[overlaps_index] <- term2
      vec_overlapSize[overlaps_index] <- overlap_list[["overlapSize"]]
      vec_overlap[overlaps_index] <- overlap_list[["overlap"]]
      vec_jaccard[overlaps_index] <- overlap_list[["jaccard"]]
      overlaps_index <- overlaps_index + 1
    }
  }
  result <- data.frame(term1 = vec_term1,
                       term2 = vec_term2,
                       overlapSize = vec_overlapSize,
                       overlap = vec_overlap,
                       jaccard = vec_jaccard,
                       stringsAsFactors = FALSE)
  return(result)
}

#' Add overlaps between annotation gene sets
#'
#' The app's network view of the enrichments results requires pairwise overlap
#' metrics between all the terms of each annotation in order to draw the edges
#' between the nodes/terms. These overlaps are calculated automatically when
#' installing or exporting an OmicNavigator study. If you'd like, you can
#' manually calculate these pairwise overlaps by calling \code{addOverlaps}
#' prior to installing or exporting your study.
#'
#' @inherit shared-add
#'
#' @export
addOverlaps <- function(study, reset = FALSE) {
  checkStudy(study)

  if (isEmpty(study[["annotations"]])) {
    warning("Cannot calculate overlaps without annotations")
    return(study)
  }

  message("Calculating pairwise overlaps. This may take a while...")

  annotationsAvailable <- names(study[["annotations"]])
  overlapsList <- vector(mode = "list", length = length(annotationsAvailable))
  names(overlapsList) <- annotationsAvailable
  for (annotationID in annotationsAvailable) {
    terms <- study[["annotations"]][[annotationID]][["terms"]]
    # filter to only include terms included in at least one enrichment
    if (!is.null(study[["enrichments"]])) {
      termsEnriched <- character()
      for (i in seq_along(study[["enrichments"]])) {
        termsTmp <- lapply(study[["enrichments"]][[i]][[annotationID]],
                           function(x) x[["termID"]])
        termsEnriched <- c(termsEnriched, unlist(termsTmp, use.names = FALSE))
      }
      termsEnriched <- unique(termsEnriched)
      terms <- terms[names(terms) %in% termsEnriched]
    }
    overlaps <- calc_pairwise_overlaps(terms)
    overlaps <- overlaps[overlaps[["overlapSize"]] > 0, ]
    row.names(overlaps) <- NULL # reset row names post-filtering
    overlapsList[[annotationID]] <- overlaps
  }

  if (reset) {
    study[["overlaps"]] <- list()
  }

  study[["overlaps"]] <- modifyList(study[["overlaps"]], overlapsList)

  return(study)
}
