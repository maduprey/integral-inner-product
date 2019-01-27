# Author: Michael Duprey
# Date: May 13, 2018

# Integral inner product --------------------------------------------------

"%i%" <- function(u, v) {
  # Verify numeric vector
  if (!(is.vector(u) & is.numeric(u))) stop("Argument 1 not a numeric vector")
  if (!(is.vector(v) & is.numeric(v))) stop("Argument 2 not a numeric vector")
  
  # Verify eqivalent dimension
  if (length(u) != length(v)) stop("Arguments are of unequal dimension")
  
  # Define an inner product int from 0 to 1 of u(x) * v(x) with respect to x
  c <- convolve(u, rev(v), type = "open") # Compute u(x) * v(x)
  f <- rev(sapply(1:length(c), function(i) {paste0("(", rev(c)[i], ")*x^", i-1, "")}))
  f <- paste0(f[!grepl("(0)", f, fixed = T)], collapse = " + ")
  message(paste("f(x) =", f))
  int <- integrate(function(x) {eval(parse(text = f))}, 0, 1)
  
  # Returns a scalar for the inner product of u(x) and v(x)
  return(int$value)
}