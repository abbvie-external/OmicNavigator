#' @export
summary.oaStudy <- function(x, ...) {
  display(x)
  return(invisible(x))
}

display <- function(x, indent = "") {
  for (i in seq_along(x)) {
    if (isEmpty(x[[i]])) {
      next
    } else if (is.data.frame(x[[i]])) {
      writeLines(displayDataFrame(x[[i]], names(x)[i], indent = indent))
    } else if (is.list(x[[i]])) {
      writeLines(displayList(x[[i]], names(x)[i], indent = indent))
      display(x[[i]], indent = paste0(indent, "  "))
    } else if (is.character(x[[i]])) {
      if (length(x[[i]]) == 1) {
        writeLines(displayCharacter(x[[i]], names(x)[i], indent = indent))
      }
    }
  }
}

displayDataFrame <- function(x, name, indent = "") {
  paste(indent, "o-", name, ":", nrow(x), "x", ncol(x))
}

displayList <- function(x, name, indent = "") {
  paste(indent, "|-", name, "(", length(x), ")")
}

displayCharacter <- function(x, name, indent = "") {
  paste(indent, "o-", name, ":", x)
}
