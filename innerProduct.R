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

# Example usage
u <- c(3, .5, 6) 
v <- c(1, 0, 2)
s <- u %i% v
s


# Gram-Schmidt process ----------------------------------------------------

gramSchmidt <- function(V) {
  # Verify numeric vector
  if (!is.list(V)) stop("Argument is not a list")
  if (!all(sapply(V, function(x) is.numeric(unlist(x))))) stop("Argument is not a numeric list")
  
  # Verify eqivalent dimension
  if (length(unique(sapply(V, function(x) length(unlist(x))))) > 1) stop("Arguments contains vectors of unequal dimension")
  
  # Compute orthogonal vectors
  W <- NULL
  W[[1]] <- V[[1]]
  for(i in 2:length(V)) {
    projVi <- 0
    for(j in 1:(i-1)) {
      projVi_Wj <- ((V[[i]] %i% W[[j]]) / (W[[j]] %i% W[[j]])) * W[[j]]
      projVi <- projVi + projVi_Wj
    }
    W[[i]] <- V[[i]] - projVi
  }
  
  W <- W[!sapply(W, function(x) all(is.na(x)))] # Removes NaN-vectors
  W <- W[!sapply(W, function(x) ifelse(length(unique(x)) == 1, ifelse(unique(x) == 0, T, F), F))] # Removes 0-vectors
  
  # Normalize vectors
  E <- lapply(W, function(x) x / sqrt(x %i% x))
  
  # Returns an orthonormal spanning set of vectors
  return(E)
}

# Example usage
V <- list(c(1, 3, 2), c(2, 0, 0), c(1, 3, 0))
E <- gramSchmidt(V)
E