#function to plot doubledecker plots from objects received from data_summary.R
#data structure fed to function must be a 2x2 table containing the total numbers of sick only, healthy only and sick&healthy users
#"sickness_state" takes values "sick", "healthy" or "random" and signifies the state that the users represented in the dataste *should* be in
library("vcd")

double_decker_plus <- function(table,sickness_state = c("random","healthy","sick"),path=""){
  filename <- paste0(path,"doubledecker_",sickness_state,".pdf")
  pdf(file=filename,width=14)
  doubledecker(table,pop=F,varnames=F,
               gp = gpar(fill = c("cyan","green","green","deeppink")))
  labeling_cells(text = format(table,scientific=T), margin=0)(table)
  dev.off()
}