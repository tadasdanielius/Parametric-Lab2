source('init.R')
source('data.R')
source('common.R')


simulate.data = function(n=50, mu=c(5,7,11), sigma=NULL) {
  if (is.null(sigma)) {
    matrix.corr = diag(3)
    matrix.corr[2, 1] = 0.5
    matrix.corr[1, 2] = 0.5
    matrix.corr[3, 1] = 0.6
    matrix.corr[1, 3] = 0.6
    matrix.corr[2, 3] = 0.7 
    matrix.corr[3, 2] = 0.7
    sigma = cor2cov(matrix.corr, c(1,2,3))
  }
  return(mvrnorm(n,mu,Sigma = sigma))
}

simulate.nullify = function(data.matrix, p=0.5) {
  ncol = dim(data.matrix)[2]
  nrow = dim(data.matrix)[1]
  missing = matrix(rbinom(ncol*nrow, 1, p),ncol=ncol)
  results = list()
  results$original = data.matrix
  results$data = data.matrix
  results$data[missing==1] = NA
  results$missing = missing
  return(results)
}

simulate.run = function(n = 100, epsilon=0.00001, verbose=TRUE, correlated=TRUE, max=5000) {
  diff = rep(0,n)
  mape = rep(0,n)
  ml = rep(0,n)
  ml_mat = matrix(0, nrow=n, ncol=n)
  
  max_iter = 1
  if (correlated) {
    max_iter = max
  }
  results = NULL
  
  
  for (i in 1:n) {
    not_acceptable = T
    # Hack, if ml too large repeat the simulation
    while (not_acceptable) {
      dat = simulate.data()
      res = simulate.nullify(dat)
      m = prepare_data(res$data)
      
      if (verbose) {
        results = run_iterations(m, max=max_iter, epsilon = epsilon, correlated=correlated)
      }
      else {
        results = suppressMessages(run_iterations(m, max=max_iter, epsilon = epsilon, correlated = correlated))
      }
      
      ml.all = res$ml.all
      diff[i] = mean((dat[res$missing==1] - results$D[res$missing==1]))
      mape[i] = mean(abs(
        (results$D[res$missing==1] - dat[res$missing==1])/dat[res$missing==1]))
      ml[i] = mean(results$ml.all)
      if (ml[i]<5 && mape[i] < 4) {
        not_acceptable = F
      } else {
        message('ML too large, repeating simulation.')  
      }
      
    }
    
    if (verbose) {
      message('Simulation #',i, ' Error: ', diff[i], ' ML: ', ml[i], ' MAPE: ', mape[i], ' diff: ', diff[i])
    }
  }
  message('Total simulations ',n, ' Epsilon: ', epsilon)
  return(list(
    errors = diff,mape=mape, ml=ml
  ))
}


