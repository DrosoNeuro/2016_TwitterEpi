## A function to extract meta information from a bunch of big.matrix .desc-files
require(bigmemory)
extract_rows <- function(path="./"){
  files_to_process <- list.files(path=path,"*.desc")
  files_to_process <- paste0(path,files_to_process)
  
  getrow <- function(desc_file){
    desc_obj <- dget(desc_file)
    nrows <- desc_obj@description$nrow
  }
  nrows <- sum(sapply(files_to_process,getrow))
  ncols <- dget(files_to_process[1])@description$ncol
  write(c("nrows: ",nrows),2,file=paste0(path,"tot_col_rows.txt"))
  write(c("ncols: ",ncols),2,file=paste0(path,"tot_col_rows.txt"),append=T)
}



