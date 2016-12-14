load_data = function() {
  
  D = as.matrix(read.csv('data/input_data.csv'))
  m = prepare_data(D)
  m$D.orig = D

  return(m)
}

prepare_data = function (D) {
  m = list()
  
  m$D = D
  m$D.orig = D
  m$M = as.matrix(colMeans(D, na.rm = T))
  m$V = cov(m$D, use = "pairwise.complete.obs")
  m$s = matrix(as.numeric(!is.na(m$D)),dim(m$D))
  m$D[which(is.na(m$D))] = 0
  I = diag(dim(m$D.orig)[2])
  m$V_diag = as.matrix(I * cov(m$D.orig, use="pairwise.complete.obs"))
  
  return(m)  
}