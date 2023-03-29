is.fraction <-
  function(x, tol = .Machine$double.eps^0.5) {
    whole <- abs(x - round(x)) < tol
    frac <- !whole
    return(frac)
  }