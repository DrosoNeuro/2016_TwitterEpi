library("chron")
source("sir_func.R")

##fit a timeseries to an sir curve
##data: the data in an array or a list of arrays, if a list, model is evaluated on each element seperatly
##remove: do not use these time steps for scoring
fitSIR <- function(data, gamma_center = 1/3, rnaught_center = 2.5, steps = 25,  resolution = 1, remove = c(9999999))
{

bestfiterror = 99999
bestfitGamma = 1/81
bestfitRnaught = 991

for(step in 0:steps)
{
bestfiterror = 99999
gamma_center_adjusted = log(gamma_center, 3)

gammas = 3^seq(gamma_center_adjusted - 10*5^-step, gamma_center_adjusted + 10*5^-step, length.out = 25)

Rnaughts = seq(max(1.0,rnaught_center - 10*2^-(1+step)),rnaught_center + 10*2^-(1+step),length.out=25)

for(gamma in gammas)
{
for(Rnaught in Rnaughts)
{

error = 0

if(is.list(data))
{
Sirmodel <- list()
for(listitr in 1:length(data))
{

I_0 = data[[listitr]][1]

S_0 = 1-I_0
R_0 = 0


vt = 1:length(data[[listitr]])
beta = Rnaught*gamma

params = c(gamma=gamma, beta = beta)
initvals = c(S=S_0, I=I_0, R = R_0)

sirmodel = as.data.frame(lsoda(initvals,vt,SIRfunc,params))

error = error + sum(((sirmodel$I-data[[listitr]])*(sirmodel$I-data[[listitr]]))[-remove])
Sirmodel <- c(Sirmodel,sirmodel)
}
sirmodel <- Sirmodel
}
else
{

I_0 = data[1]

S_0 = 1-I_0
R_0 = 0


vt = 1:length(data)
beta = Rnaught*gamma

params = c(gamma=gamma, beta = beta)
initvals = c(S=S_0, I=I_0, R = R_0)

sirmodel = as.data.frame(lsoda(initvals,vt,SIRfunc,params))

error = sum(((sirmodel$I-data)*(sirmodel$I-data))[-remove])
}
if(error <= bestfiterror)
{
bestfiterror = error
bestfitGamma = gamma
bestfitRnaught = Rnaught
SIRmodel <- sirmodel
#print(SIRmodel)
}
}
}

rnaught_center = bestfitRnaught
gamma_center = bestfitGamma


print(rnaught_center)
print(range(Rnaughts))
print(gamma_center)
print(range(gammas))
print(error)
print(bestfiterror)
}

params = c(error = bestfiterror, gamma = bestfitGamma, beta = bestfitGamma * bestfitRnaught, r0 = bestfitRnaught,SIRmodel=SIRmodel)

return(params)
}
