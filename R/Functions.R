
# Ordering matrix cf. SOCPROG ----
order_matrix <- function(x){
  newMATRIX <- x[sort(rownames(x), decreasing = FALSE),
                 sort(colnames(x), decreasing = FALSE)]
}

# Permute GBI Matrix (by Mauricio Cantor) ----
null_checkerboard <- function(mat.gbi, iter, ...) {
  tmp.permute <- vegan::permatswap(mat.gbi, 
                            times = iter, 
                            method = "quasiswap", 
                            fixedmar = "both", 
                            shuffle = "both", 
                            mtype = "prab")
}

# SRI (by Mauricio Cantor) ----
SRI =  function (matr) {
  if (any(is.na(matr))) {
    matr <- na.omit(matr)
    cat("The data matrix contains NA, and have been removed.\n")
  }
  
  matr1 = matr
  N <- nrow(matr1)
  matr1[matr1 > 1] <- 1
  n <- apply(matr1, 2, sum)
  tmatr <- t(matr1)
  df <- as.matrix(t(matr))
  a <- df %*% t(df)
  b <- df %*% (1 - t(df))
  c <- (1 - df) %*% t(df)
  d <- ncol(df) - a - b - c
  
  Dice <- data.frame()
  inmat <- data.frame()
  denmat <- data.frame()
  
  for (i in 1:nrow(a)) {
    for (j in 1:ncol(a)) {
      # Simple Ratio Index
      Dice[i, j] <- 1 * a[i, j]/(1 * a[i, j] + b[i, j] + c[i, j])
      
      # Numerator of the SRI
      inmat[i, j] <- 1 * a[i, j] 
      
      # Denominator of the SRI
      denmat[i, j] <- (1 * a[i, j] + b[i, j] + c[i, j])
    }
  }
  
  rownames(Dice)=colnames(Dice)=colnames(matr)
  rownames(inmat)=colnames(inmat)=colnames(matr)
  rownames(denmat)=colnames(denmat)=colnames(matr)
  
  # Returns a list of 3 levels
  list(SRI = Dice, # Simple-Ratio indices
       SRI.numerator = inmat,  # Numerator of the SRI
       SRI.denominator = denmat) # Denominator of the SRI
}

# Unfold Matrix (by ALexandre Machado) ----
matrix_unfold <- function(x) {
  x[lower.tri(x, diag=FALSE)]
}

# Rescale (by Alexandre Machado)
#' @title Rescaling distribution
#' @description Rescales a distribution to a chosen range
#' @param x vector to be rescaled
#' @param r.out vector with limits of the new distribution
#' @return vector with rescaled distribution
#' @examples
#' # generating 10-sample uniform distribution from 1 to 100 and rescaling it to 0 to 1
#' data <- runif(10, 1, 100)
#' rescale(data, r.out=c(0,1))

rescale <- function(x, r.out) {
  p <- (x - min(x)) / (max(x) - min(x))
  r.out[[1]] + p * (r.out[[2]] - r.out[[1]])
}

