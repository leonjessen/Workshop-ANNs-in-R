# Encoding function for converting a given peptide 'x' to a numerical
# representation using the 'm' matrix, e.g. using BLOSUM [1]
# 1. https://en.wikipedia.org/wiki/BLOSUM
encode_peptide <- function(x, m){
  X_enc <- lapply(strsplit(x, ""), function(x_i){
    x_i_enc_m <- m[x_i,] # matrix of dim = length(pep) x 20
    x_i_enc_v <- as.vector(t(x_i_enc_m)) # vector of length = length(pep) * 20
    return(x_i_enc_v)
  })
  X_enc <- do.call(rbind, X_enc)
  rownames(X_enc) <- x
  colnames(X_enc) <- paste0("feature_", 1:ncol(X_enc))
  return(X_enc)
}
alanine_scan <- function(x){
  out <- lapply(strsplit(x, ""), function(x_i){
    k <- length(x_i)
    scan <- sapply(1:k, function(i){
      mut <- x_i
      mut[i] <-  "A"
      mut <- paste0(mut, collapse = "")
      return(mut)
    })
    return(scan)
  })
  names(out) <- x
  return(out)
}
find_mut_pos <- function(x,y){
  X <- strsplit(x, "")
  Y <- strsplit(y, "")
  out <- sapply(1:length(x), function(i){
    p_mismatch = which(X[[i]] != Y[[i]])
    if( length(p_mismatch) < 1 ){ p_mismatch = NA }
    return(p_mismatch)
  })
  return(out)
}
get_class = function(x){
  x_class = apply(x, 1, which.max)
  return(x_class - 1)
}
predict_classes = function(object, x){
  y_pred = predict(object, x)
  y_pred_class = get_class(y_pred)
  return(y_pred_class)
}
