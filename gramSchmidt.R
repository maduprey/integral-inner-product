# Author: Michael Duprey
# Date: May 13, 2018

# Gram-Schmidt process ----------------------------------------------------
source("./innerProduct.R")

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