# For removing shared database columns that aren't relevant to the filtered
# result
is_available <- function(x) !all(is.na(x))
