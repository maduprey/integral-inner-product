# Author: Michael Duprey
# Date: May 13, 2018

# Example usage -----------------------------------------------------------
source("./innerProduct.R")
source("./gramSchmidt.R")

# Integral inner product
u <- c(3, .5, 6) 
v <- c(1, 0, 2)
s <- u %i% v
s

# Gram-Schmidt process
V <- list(c(1, 3, 2), c(2, 0, 0), c(1, 3, 0))
E <- gramSchmidt(V)
E