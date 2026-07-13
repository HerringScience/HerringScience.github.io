

# helper function
read_char_var <- function(nc, varname) {
  raw <- ncvar_get(nc, varname)
  
  if (is.matrix(raw)) {
    return(trimws(apply(raw, 2, paste, collapse = "")))
  } else {
    return(trimws(raw))
  }
}