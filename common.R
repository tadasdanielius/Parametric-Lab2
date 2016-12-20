impute = function(idx, V,M,D,s) {
  I = diag(dim(D)[2])
  y = t(t(D[idx, ]))
  S = I * s[idx,]
  S_star = I - S
  return(y + S_star %*% (M + V %*% S %*% ginv(S %*% V %*% S) %*% (y - S %*% M)))
}

calc_W = function(idx, V, s){
  I = diag(dim(V)[1])
  S = I * s[idx,]
  S_star = I - S
  return(S_star %*% (V - V %*% S %*% ginv(S %*% V %*% S) %*% S %*% V) %*% S_star)
}

calc_log_likelihood = function(D, V, M) {
  sm = 0
  nrows = dim(D)[1]
  for (y in as.data.frame(t(D))) {
    sm = sm+ t(y-M) %*% ginv(V) %*% (y-M)
  }
  return(-nrows/2 * log(det(V)) - 1/2 * sm)
}


calc_err_cov = function(nrows, V, s, w.sum) {
  w.sum = matrix(0, ncol = dim(V)[1], nrow = dim(V)[2])
  for (idx in 1:nrows) {
    w = calc_W(idx, V, s)
    w.sum = w.sum + w / nrows
  }
  return(w.sum)
}

run_iterations = function(m, max=100, epsilon=0.00001) {
  D = m$D
  D_prev = D
  
  
  M = m$M
  V = m$V
  s = m$s
  
  nrows = dim(D)[1]
  ncols = dim(D)[2]
  ets_err = matrix(0, ncol=1, nrow=max)
  for (iteration in 1:max) {
    w.sum = matrix(0, ncol = ncols, nrow = nrows)
    D = t(sapply(1:nrows, 
                 impute, 
                 V=V, 
                 M=M, 
                 D=m$D, 
                 s=s))
    eps = sum(abs(D_prev-D))
    ets_err[iteration,] = eps
    D_prev = D
    if (eps < epsilon) {
      message('epsilon ',eps, ' < ', epsilon, ' Terminating.' )
      break;
    }
    w.sum = calc_err_cov(nrows, V, s, w.sum)
    M = as.matrix(colMeans(D, na.rm=T))
    V <- cov(D, use = "pairwise.complete.obs") + w.sum
    ml = calc_log_likelihood(D, V, M)
    message('iteration ', iteration, ' ML: ', ml, ' Epsilon: ', eps)
  }
  
  ret = list()
  ret$D = D
  ret$M = M
  ret$V = V
  ret$err = ets_err
  return (ret)
}

