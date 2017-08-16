#function to plot doubledecker plots from objects received from data_summary.R
#data structure fed to function must be a 2x2 table containing the total numbers of sick only, healthy only and sick&healthy users
#"sickness_state" takes values "sick", "healthy" or "random" and signifies the state that the users represented in the dataste *should* be in
library("vcd")

double_decker_plus <- function(table,sickness_state = c("random","healthy","sick"),path=""){
  filename <- paste0(path,"doubledecker_",sickness_state,".pdf")
  pdf(file=filename,width=10,height=5)
  doubledecker(table,pop=F,varnames=F,
               gp = gpar(fill = c("cyan","green","green","red")))
  labeling_cells(text = format(table,scientific=T), margin=0)(table)
  dev.off()
  
  filename <- paste0(path,"barplot_",sickness_state,".pdf")
  pdf(file=filename,width=10,height=5)
  par(cex=1.2)
  barplot(c(table[1,1],table[1,2],table[2,2]),width=c(0.1,0.1,0.1),
          names.arg=c("sick only","sick & healthy","healthy only"),
          ylab="number of users")
  dev.off()
  
}
