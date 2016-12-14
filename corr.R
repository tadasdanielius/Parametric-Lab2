source('init.R')
source('data.R')
source('common.R')

m = load_data()
results = run_iterations(m, max=1000)

print(results$D, digits=2)
