impute = function(idx, V, M, D, s) {
  I = diag(dim(D)[2])
  y = t(t(D[idx, ]))
  S = I * s[idx,]
  S_star = I - S
  return(y + S_star %*% (M + V %*% S %*% ginv(S %*% V %*% S) %*% (y - S %*% M)))
}

calc_W = function(idx, V, s) {
  I = diag(dim(V)[1])
  S = I * s[idx,]
  S_star = I - S
  return(S_star %*% (V - V %*% S %*% ginv(S %*% V %*% S) %*% S %*% V) %*% S_star)
}

calc_log_likelihood = function(D, V, M) {
  tryCatch({
    sm = 0
    nrows = dim(D)[1]
    for (y in as.data.frame(t(D))) {
      sm = sm + t(y - M) %*% ginv(V) %*% (y - M)
    }
    return(-nrows / 2 * log(abs(det(V))) - 1 / 2 * sm)
  }, error = function(err) {
    message('error happened')
  })
}


calc_err_cov = function(nrows, V, s, w.sum) {
  w.sum = matrix(0, ncol = dim(V)[1], nrow = dim(V)[2])
  for (idx in 1:nrows) {
    w = calc_W(idx, V, s)
    w.sum = w.sum + w / nrows
  }
  return(w.sum)
}

run_iterations = function(m,
                          max = 1000,
                          epsilon = 0.00001,
                          correlated = TRUE,
                          ml.cap = 2) {
  D = m$D
  D_prev = D
  
  
  M = m$M
  
  V = m$V
  if (!correlated) {
    V = m$V_diag
  }
  s = m$s
  
  nrows = dim(D)[1]
  ncols = dim(D)[2]
  
  ml.prev = calc_log_likelihood(D, V, M)
  if (is.nan(ml.prev)) {
    warning('ml.prev is NaN')
  }
  
  ml = NULL
  iteration = 1
  ml.abs = epsilon + 1
  
  ml.all = vector()
  
  while (ml.abs > epsilon) {
    not_acceptable = T
    ml.prev = ifelse(iteration > 1, ml, ml.prev)
    
    if (iteration > max) {
      #message('terminated. Max iteration reached')
      break
      
    }
    
    w.sum = matrix(0, ncol = ncols, nrow = nrows)
    D = t(sapply(
      1:nrows,
      impute,
      V = V,
      M = M,
      D = m$D,
      s = s
    ))
    D_prev = D
    w.sum = calc_err_cov(nrows, V, s, w.sum)
    M = as.matrix(colMeans(D, na.rm = T))
    V <- cov(D, use = "pairwise.complete.obs") + w.sum
    
    ml = calc_log_likelihood(D, V, M)
    ml.abs = abs(ml - ml.prev) / abs(ml.prev)
    ml.all[iteration] = ml.abs
    if (is.nan(ml.abs)) {
      message('ML abs is NaN. ml=', ml, ', ml.prev=', ml.prev)
      #ml.abs = 0
    }
    if (ml.abs > ml.cap && iteration > 1) {
      ml.all[iteration] = ml.all[iteration-1]
                                 
    }

    iteration = iteration + 1
}
message(' iteration ', iteration - 1, ' ML: ', ml.abs)
ret = list()
ret$D = D
ret$M = M
ret$V = V
ret$ml.all = ml.all

return (ret)
}
