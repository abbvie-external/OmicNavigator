#' Combine two or more studies
#'
#' Create a new OmicNavigator study by combining two or more existing study
#' objects.
#'
#' This is a convenience function to quickly and conveniently combine studies.
#' However, it is naive, and you will likely need to edit the new study after
#' combining. When there are conflicting elements (e.g. different study names or
#' different maintainers), then the value for the latter study is kept. As a
#' concrete example, if you combined 5 studies, the name of the combined study
#' would be the name of the 5th study.
#'
#' The behavior is more complex for study elements that are nested lists of data
#' frames (e.g. results). If the 5 studies included a results table for the same
#' modelID/testID combination, then only the results from the 5th study would be
#' retained. However, if they each defined a different modelID, then the results
#' for all 5 modelIDs would be included in the combined study. Please note that
#' you should be extra cautious in the situation where the studies have the same
#' modelID/testID combination. Ideally they should all have the same column
#' names. Since a data frame is technically a list, the workhorse function
#' \code{\link[utils:modifyList]{modifyList}} will retain any uniquely named
#' columns from earlier studies along with the columns from the final study.
#'
#' Note that as a shortcut you can also combine studies using the S3 method
#' \code{\link[base:c]{c}}.
#'
#' If a study you would like to combine is already installed, you can convert it
#' to a study object by importing it with \code{\link{importStudy}}.
#'
#' @param ... Two or more objects of class \code{onStudy}
#'
#' @return Returns a new combined OmicNavigator study object, which is a named
#'   nested list with class \code{onStudy}
#'
#' @seealso
#'   \code{\link{createStudy}},
#'   \code{\link{importStudy}}
#'
#' @examples
#'
#' # Define threee study objects
#' studyOne <- createStudy(name = "One",
#'                         description = "First study",
#'                         studyMeta = list(metafield1 = "metavalue1"))
#'
#' studyTwo <- createStudy(name = "Two",
#'                         description = "Second study",
#'                         maintainer = "The Maintainer",
#'                         studyMeta = list(metafield2 = "metavalue2"))
#'
#' studyThree <- createStudy(name = "Three",
#'                           description = "Third study",
#'                           studyMeta = list(metafield3 = "metavalue3"))
#'
#' # Combine the three studies
#' combineStudies(studyOne, studyTwo, studyThree)
#'
#' # Equivalently, can use c()
#' c(studyOne, studyTwo, studyThree)
#'
#' @export
combineStudies <- function(...) {
  studies <- list(...)

  if (length(studies) < 2) {
    stop("combineStudies requires two or more onStudy objects as input")
  }
  lapply(studies, checkStudy)

  combined <- Reduce(f = combineStudiesPair, x = studies)
  checkStudy(combined)
  return(combined)
}

# The workhorse function that combines two studies at a time. Only really needed
# because default NULL elements like version/maintainer/maintainerEmail are
# difficult to combine intelligently using modifyList() alone
combineStudiesPair <- function(studyOne, studyTwo) {
  studyCombined <- studyOne
  elements <- names(studyOne)
  for (e in elements) {
    if (is.null(studyTwo[[e]])) {
      next
    } else if (is.list(studyTwo[[e]])) {
      studyCombined[[e]] <- modifyList(studyOne[[e]], studyTwo[[e]])
    }
    else {
      studyCombined[[e]] <- studyTwo[[e]]
    }
  }
  return(studyCombined)
}

#' @export
c.onStudy <- function(...) {
  combineStudies(...)
}
