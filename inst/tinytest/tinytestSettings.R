# Shared tinytest settings

library(tinytest)

# For ttdo output
options(
  diffobj.format = "ansi256",
  diffobj.mode = "unified"
)

concatenateOutput <- function(x) {
  output <- utils::capture.output(print(x))
  output <- paste(output, collapse = "\n")
  return(output)
}
