source("./innerProduct.R")
source("./gramSchmidt.R")

# Calculate the integral inner product for vectors u, v
u <- c(3, .5, 6) 
v <- c(1, 0, 2)
s <- u %i% v
s

# Invoke Gram-Schmidt process for three vectors from the R^3 inner product space
V <- list(c(1, 3, 2), c(2, 0, 0), c(1, 3, 0))
E <- gramSchmidt(V)
E
