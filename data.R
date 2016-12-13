load_data = function() {
  m = list()
  m$D.orig = as.matrix(read.csv('data/input_data.csv'))

  m$M = as.matrix(colMeans(m$D, na.rm=T))
  
  I = diag(dim(m$D.orig)[2])
  m$V = as.matrix(cov(m$D.orig, use="pairwise.complete.obs"))
  m$V_diag = as.matrix(I * cov(m$D.orig, use="pairwise.complete.obs"))
  
  m$s = matrix(as.numeric(!is.na(m$D)),dim(m$D))
  m$D[which(is.na(m$D))] = 0
  
  return(m)
}
