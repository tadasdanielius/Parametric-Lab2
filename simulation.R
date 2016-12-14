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

simulate.run = function(n = 100, epsilon=0.00001, verbose=TRUE) {
  diff = rep(0,n)
  for (i in 1:n) {
    dat = simulate.data()
    res = simulate.nullify(dat)
    m = prepare_data(res$data)
    
    results = NULL
    if (verbose) {
      results = run_iterations(m, max=5000, epsilon = epsilon)
    }
    else {
      results = suppressMessages(run_iterations(m, max=5000, epsilon = epsilon))
    }
    err = mean(abs(dat[res$missing==1] - results$D[res$missing==1]))
    diff[i] = err
    message('Simulation #',i, ' Error: ', err)
  }
  message('Total simulations ',n, ' Epsilon: ', epsilon)
  return(diff)
}


