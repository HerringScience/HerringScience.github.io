
# Small helper for map axis padding
pad_range <- function(x, pad = 0.05) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) return(c(NA, NA))
  
  if (length(unique(x)) == 1) {
    return(c(x[1] - 0.05, x[1] + 0.05))
  }
  
  r <- range(x, na.rm = TRUE)
  buffer <- diff(r) * pad
  c(r[1] - buffer, r[2] + buffer)
}