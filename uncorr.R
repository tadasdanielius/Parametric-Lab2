source('init.R')
source('data.R')
source('common.R')

matrices = load_data()
imputed = t(sapply(1:dim(matrices$D)[1], 
                    impute, 
                    V=matrices$V_diag, 
                    M=matrices$M, 
                    D=matrices$D, 
                    s=matrices$s))
print (imputed)
