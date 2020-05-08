#' @export
print.oaStudy <- function(x, ...) {

  cat("== OmicAnalyzer ==\n")
  display(unclass(x))

  return(invisible(x))
}

display <- function(x, prefix = "*  ", ...) {
  UseMethod("display")
}

display.NULL <- function(x, ...) {
  cat("Not available\n")
}

display.list <- function(x, ...) {
  for (i in seq_along(x)) {
    cat(sprintf("\n* %s: ", names(x)[i]))
    display(x[[i]], prefix)
  }
}

display.data.frame <- function(x, ...) {
  cat(sprintf("%d rows x %d columns", nrow(x), ncol(x)))
}

display.matrix <- function(x, ...) {
  cat(sprintf("%d rows x %d columns", nrow(x), ncol(x)))
}

display.character <- function(x, ...) {
  if (length(x) == 1) {
    cat(sprintf("\"%s\"", x))
  } else {
    cat(sprintf("%d items", length(x)))
  }
}

display.default <- function(x, ...) {
  print(x)
}
