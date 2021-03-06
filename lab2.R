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
results = run_iterations(m, max=1000, epsilon = 0.001, correlated=FALSE, ml.cap=10)
plot(results$ml.all, type='l')
print(results$D)


# Monte-carlo
n = 100
results = simulate.run(n, 0.001, F, correlated = FALSE)
summary(results$errors)
summary(results$mape)
plot(results$mape,type='l')
plot(results$errors,type='l')

res_corr = simulate.run(n, 0.001, T, correlated = T)
summary(res_corr$errors)
summary(res_corr$mape)
plot(res_corr$mape, type='l')
plot(res_corr$errors, type='l')

par(mfrow=c(1,2))

g_range <- range(0, results$errors, res_corr$errors)
plot(results$errors,type='l', col='red', ylim=g_range, ann=F)
lines(res_corr$errors, col='blue')
lines(rep(mean(res_corr$errors), length(res_corr$errors)), type="l", pch=22, lty=4, col="blue")
lines(rep(mean(results$errors), length(results$errors)), type="l", pch=22, lty=4, col="red")
legend(1, 1, c("Uncorrelated","Correlated"), cex=0.8, col=c("red","blue"), pch=22)
title(xlab='Iteracijos')
title(ylab='Vidutinis skirtumas tarp realių ir užpildytų')


g_range <- range(0, results$mape, res_corr$mape)
plot(results$mape,type='l', col='red', ann=F)
lines(res_corr$mape, col='blue')
lines(rep(mean(res_corr$mape), length(res_corr$mape)), type="l", pch=22, lty=4, col="blue")
lines(rep(mean(results$mape), length(results$mape)), type="l", pch=22, lty=4, col="red")
legend(1, 0.4, c("Uncorrelated","Correlated"), cex=0.7, col=c("red","blue"), pch=22)
title(xlab='Iteracijos')
title(ylab='Mean Absolute % error (MAPE)')


plot(res_corr$ml, type='l')
