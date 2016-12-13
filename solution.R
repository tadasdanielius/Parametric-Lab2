prepare_data = function() {
  m = list()
  m$D = as.matrix(read.csv('data/input_data.csv'))
  m$M = as.matrix(colMeans(m$D, na.rm=T))
  m$V = as.matrix(diag(dim(m$D)[2]) * cov(m$D, use="pairwise.complete.obs"))
  m$s = matrix(as.numeric(!is.na(D)),dim(D))
  m$D[which(is.na(D))] = 0
  
  return(m)
}


fill_with_mean = function(idx, m) {
  I = diag(dim(m$D)[2])
  y = t(t(m$D[idx, ]))
  S = I * s[idx,]
  S_star = I - S
  return(y + S_star %*% (m$M + m$V %*% S %*% ginv(S %*% m$V %*% S) %*% (y - S %*% m$M)))
}



non_correlated = function(m) {
  D = m$D
  V = m$V
  M = m$M
  s = m$s

  for (i in 1:dim(D)[1]) {
    y = t(t(D[i, ]))
    S = diag(dim(D)[2]) * s[i,]
    S_star = diag(dim(D)[2]) - S
    
    m = y + S_star %*% (M + V %*% S %*% ginv(S %*% V %*% S) %*% (y - S %*% M))
    D[i, ] = m
  }
  
  #dim(D[rowSums(is.na(D))!=dim(D)[2],])[1]
  
  #s = matrix(as.numeric(!is.na(D)),dim(D))
  #colnames(s) = c('x', 'y', 'z')
  
  #S = diag(dim(s)[2]) * s
  #as.numeric(s).
  #sapply(s, is.logical)
  #s[, var:= as.numeric(get(var)), with=FALSE]
  print(D)
}


matrices = prepare_data()
m = matrices
D = m$D
V = m$V
M = m$M


non_correlated(m)

fn = function(idx, mat) {
  print(mat[idx,])
  return(mat[idx,])
}


M = matrix( c(2,6,5,1,10,4), nrow = 2,ncol = 3,byrow = TRUE)
t = M %*% t(M)
print(t)
M
ind = 1
ya <- t(t(D[ind, ]))
sa <- diag(3)
sa[which(is.na(ya)), ] <- 0
s_ <- diag(3) - sa
y[which(is.na(ya))] <- 0
ma <- ya + s_ %*% (M + V %*% sa %*% ginv(sa %*% V %*% sa) %*% (ya - sa %*% M))
ma



ya <- t(t(D[ind, ]))
sa <- diag(3)
sa[which(is.na(ya)), ] <- 0
s_ <- diag(3) - sa
ya[which(is.na(ya))] <- 0
ma <- ya + s_ %*% (M + V %*% sa %*% ginv(sa %*% V %*% sa) %*% (ya - sa %*% M))
ma
