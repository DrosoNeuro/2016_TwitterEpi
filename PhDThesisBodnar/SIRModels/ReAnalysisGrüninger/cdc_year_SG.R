source("solver_multi_SG.R")

library("chron")
data = read.csv("data/curves/cdc_long.csv",sep="\t")
data1 <- read.csv("data/curves/prediction_r_results.csv",sep=",")
plot(data1$cdcoffset,ylim=c(0,10),xlim=c(0,150),col="red")
lines(data1$full_base,col="blue")
lines(data1$full_both2,col="blue",lty=5)
lines(data1$predictions_both2,col="black",lty=2)

datasub = data[50:100,]

datasub$ILI = datasub$ILI/100

data$ILI = data$ILI/100

year1 = data[1:46,]$ILI
year2 = data[47:94,]$ILI
year3 = data[95:152,]$ILI

alldata = list(year1,year2,year3)

fit1 <- fitSIR(year1)
fit2 <- fitSIR(year2)
fit3 <- fitSIR(year3)

fit_all <- fitSIR(alldata)

plot(data$ILI)
lines(seq(1,46),fit1$SIRmodel.I)
lines(seq(47,94),fit2$SIRmodel.I)
lines(seq(95,152),fit3$SIRmodel.I)

lines(fit_all[[7]],lty="dashed")
lines(seq(47,94),fit_all[[11]],lty="dashed")
lines(seq(95,152),fit_all[[15]],lty="dashed")

#using twitter data
year1 <- df_nat[30:81]$rel_sick_user
year2 <- df_nat[82:133]$rel_sick_user
year3 <- df_nat[134:185]$rel_sick_user

plot(data$ILI,ylim=c(0,0.01))
lines(seq(1,52),fit1$SIRmodel.I)
lines(seq(53,104),fit2$SIRmodel.I)
lines(seq(105,156),fit3$SIRmodel.I)

#using cdc_data 
cdc_offset <- data1$cdcoffset[-c(1)]/100

year1 <- cdc_offset[1:45]
year2 <- cdc_offset[45:93]
year3 <- cdc_offset[93:148]
alldata = list(year1,year2,year3)

fit1 <- fitSIR(year1,rnaught_center=2.5)
fit2 <- fitSIR(year2,rnaught_center=2.5)
fit3 <- fitSIR(year3,rnaught_center=2.5)

fit_all <- fitSIR(alldata)

pdf("SIR_model_cdc_data_25.pdf",width=8,height=4.5)
plot(cdc_offset,xlab="Week",ylab="Relative amount of ILI patients")
lines(seq(1,45),fit1$SIRmodel.I)
lines(seq(45,93),fit2$SIRmodel.I)
lines(seq(93,148),fit3$SIRmodel.I)

lines(seq(1,45),fit_all[[7]],lty="dashed")
lines(seq(45,93),fit_all[[11]],lty="dashed")
lines(seq(93,148),fit_all[[15]],lty="dashed")
dev.off()

gammas_cdc_25 <- c(fit1$gamma,fit2$gamma,fit3$gamma,fit_all$gamma)
betas_cdc_25 <- c(fit1$beta,fit2$beta,fit3$beta,fit_all$beta)
errors_cdc_25 <- c(fit1$error,fit2$error,fit3$error,fit_all$error)

#using full_both_model
full_both <- data1$full_both2[-c(1,2)]/100

year1 <- full_both[1:45]
year2 <- full_both[45:93]
year3 <- full_both[93:147]
alldata <- list(year1,year2,year3)

fit_full1 <- fitSIR(year1,rnaught_center=2)
fit_full2 <- fitSIR(year2,rnaught_center=2)
fit_full3 <- fitSIR(year3,rnaught_center=2)

fit_full_all <- fitSIR(alldata,rnaught_center = 2)


pdf("SIR_model_full_model_25.pdf",width=8,height=4.5)
plot(cdc_offset,xlab="Week",ylab="Relative amount of ILI patients")
lines(seq(1,45),fit_full1$SIRmodel.I)
lines(seq(45,93),fit_full2$SIRmodel.I)
lines(seq(93,147),fit_full3$SIRmodel.I)

lines(seq(1,45),fit_full_all[[7]],lty="dashed")
lines(seq(45,93),fit_full_all[[11]],lty="dashed")
lines(seq(93,147),fit_full_all[[15]],lty="dashed")
dev.off()

gammas_full_25 <- c(fit_full1$gamma,fit_full2$gamma,fit_full3$gamma,fit_full_all$gamma)
betas_full_25 <- c(fit_full1$beta,fit_full2$beta,fit_full3$beta,fit_full_all$beta)
errors_full_25 <- c(fit_full1$error,fit_full2$error,fit_full3$error,fit_full_all$error)

save(list=c("gammas_full_25","betas_full_25","errors_full_25",
            "gammas_cdc_25","betas_cdc_25","errors_cdc_25"),file=paste0("Replications",".RData"))

#using full base_model
full_base <- data1$full_base[-c(1)]/100

year1 <- full_base[1:45]
year2 <- full_base[45:93]
year3 <- full_base[93:148]
alldata <- list(year1,year2,year3)

fit_full_base1 <- fitSIR(year1,rnaught_center=2,gamma_center = 1/2)
fit_full_base2 <- fitSIR(year2,rnaught_center=2,gamma_center = 1/2)
fit_full_base3 <- fitSIR(year3,rnaught_center=2,gamma_center = 1/2)

fit_full_base_all <- fitSIR(alldata,rnaught_center = 2,gamma_center = 1/2)


pdf("SIR_model_full_base_model_100_colorised.pdf",width=8,height=4.5)
plot(cdc_offset,xlab="Week",ylab="Relative amount of ILI patients",type="l",col="red")
lines(full_base,col="blue")
lines(seq(1,45),fit_full_base1$SIRmodel.I)
lines(seq(45,93),fit_full_base2$SIRmodel.I)
lines(seq(93,148),fit_full_base3$SIRmodel.I)

lines(seq(1,45),fit_full_base_all[[7]],lty="dashed")
lines(seq(45,93),fit_full_base_all[[11]],lty="dashed")
lines(seq(93,148),fit_full_base_all[[15]],lty="dashed")
dev.off()

pdf("SIR_model_full_both_25_colorised.pdf",width=8,height=4.5)
plot(cdc_offset,xlab="Week",ylab="Relative amount of ILI patients",type="l",col="red")
lines(full_both,col="blue")
lines(seq(1,45),fit_full1$SIRmodel.I)
lines(seq(45,93),fit_full2$SIRmodel.I)
lines(seq(93,147),fit_full3$SIRmodel.I)

lines(seq(1,45),fit_full_all[[7]],lty="dashed")
lines(seq(45,93),fit_full_all[[11]],lty="dashed")
lines(seq(93,147),fit_full_all[[15]],lty="dashed")
dev.off()


#using full AR2-model
full_AR2 <- data1$full_autocor2[-c(1,2)]/100

year1 <- full_AR2[1:45]
year2 <- full_AR2[45:93]
year3 <- full_AR2[93:147]
alldata <- list(year1,year2,year3)

fit_full_AR2_1 <- fitSIR(year1,rnaught_center=2,gamma_center = 1/2)
fit_full_AR2_2 <- fitSIR(year2,rnaught_center=2,gamma_center = 1/2)
fit_full_AR2_3 <- fitSIR(year3,rnaught_center=2,gamma_center = 1/2)

fit_full_AR2_all <- fitSIR(alldata,rnaught_center = 2,gamma_center = 1/2)


pdf("SIR_model_full_AR2_100_colorised.pdf",width=8,height=4.5)
plot(cdc_offset,xlab="Week",ylab="Relative amount of ILI patients",type="l",col="red")
lines(full_AR2,col="blue")
lines(seq(1,45),fit_full_AR2_1$SIRmodel.I)
lines(seq(45,93),fit_full_AR2_2$SIRmodel.I)
lines(seq(93,147),fit_full_AR2_3$SIRmodel.I)

lines(seq(1,45),fit_full_AR2_all[[7]],lty="dashed")
lines(seq(45,93),fit_full_AR2_all[[11]],lty="dashed")
lines(seq(93,147),fit_full_AR2_all[[15]],lty="dashed")
dev.off()




#calculating two curves, one based on the gammas and betas given in citep{bodnar_data_2015}, one given in Grüninger 2017
#based on the initial value given in the full model
full_both2 <-full_both <- data1$full_both2[-c(1,2)]/100

#define starting points for each year
I_Zeros <- c(full_both2[1],full_both2[45],full_both2[93])
vts = c(45,49,55) #nubmer of weeks in each yearls models as defined by Bodnar 2015

#get yearly and combined gammas and betas
gammas_bodnar <- c(0.1176,0.7317,0.6046,0.6765)
betas_bodnar <- c(0.1195,0.9020,0.7264,0.7935)

#calculate yearly curves
calc_curves <- function(gammas,betas,yearly=T){
  SIRmodels <- list()
  for (i in 1:3){
    initvals <- c(S=1-I_Zeros[i],I=I_Zeros[i],R=0)
    if(yearly==T){
      params <- c(gamma=gammas[i],beta=betas[i]) 
    } else {
      params <- c(gamma=gammas[4],beta=betas[4])
    }
    vt <- 1:vts[i]
    sirmodel = as.data.frame(lsoda(initvals,vt,SIRfunc,params))
    SIRmodels <- c(SIRmodels,sirmodel)
  }
    return(SIRmodels)
  }

yearly_bodnar <- calc_curves(gammas_bodnar,betas_bodnar,T)
combined_bodnar <- calc_curves(gammas_bodnar,betas_bodnar,F)

yearly_grun <- calc_curves(gammas_full_25,betas_full_25,T)
combined_grun <- calc_curves(gammas_full_25,betas_full_25,T)

pdf("SIR_model_full_model_25_comparison_yearly.pdf",width=10,height=5)
plot(cdc_offset,xlab="Week",ylab="Relative amount of ILI patients",col="red",type="l")
lines(seq(1,45),yearly_bodnar[[3]],col="cyan")
lines(seq(45,93),yearly_bodnar[[7]],col="cyan")
lines(seq(93,147),yearly_bodnar[[11]],col="cyan")

lines(seq(1,45),yearly_grun[[3]],col="blue")
lines(seq(45,93),yearly_grun[[7]],col="blue")
lines(seq(93,147),yearly_grun[[11]],col="blue")
dev.off()

pdf("SIR_model_full_model_25_comparison_combined.pdf",width=10,height=5)
plot(cdc_offset,xlab="Week",ylab="Relative amount of ILI patients",col="red",type="l")

lines(seq(1,45),combined_bodnar[[3]],col="cyan",lty="dashed")
lines(seq(45,93),combined_bodnar[[7]],col="cyan",lty="dashed")
lines(seq(93,147),combined_bodnar[[11]],col="cyan",lty="dashed")

lines(seq(1,45),combined_grun[[3]],col="blue",lty="dashed")
lines(seq(45,93),combined_grun[[7]],col="blue",lty="dashed")
lines(seq(93,147),combined_grun[[11]],col="blue",lty="dashed")
dev.off()


#testin whether same ratio of beta and gamma yields same result (it doesn't)
gammas1 <- c(0.1,0.15,0.2)
betas1 <- c(0.2,0.3,0.4)

gammas2 <- c(0.2,.3,.4)
betas2 <- c(.4,.6,.8)

yearly_1 <- calc_curves(gammas1,betas1,T)
yearly_2 <- calc_curves(gammas2,betas2,T)

plot(cdc_offset,xlab="Week",ylab="Relative amount of ILI patients",col="red",type="l")
lines(seq(1,45),yearly_1[[3]],col="cyan")
lines(seq(45,93),yearly_1[[7]],col="cyan")
lines(seq(93,147),yearly_1[[11]],col="cyan")

lines(seq(1,45),yearly_2[[3]],col="blue")
lines(seq(45,93),yearly_2[[7]],col="blue")
lines(seq(93,147),yearly_2[[11]],col="blue")
