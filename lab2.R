source('init.R')
source('data.R')
source('common.R')
source('simulation.R')


# A dalis
matrices = load_data()
imputed = t(sapply(1:dim(matrices$D)[1], 
                    impute, 
                    V=matrices$V_diag, 
                    M=matrices$M, 
                    D=matrices$D, 
                    s=matrices$s))
print (imputed)


# B Dalis
m = load_data()
results = run_iterations(m, max=1000)

print(results$D)


# Monte-carlo
results = simulate.run(10, 0.0000001, F)
