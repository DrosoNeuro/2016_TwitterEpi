## functions for manipulation of the bigmatrix data set
require(bigmemory)


#combine submatrices
combine_bigmatrices <- function(in_path="./",out_path="./"){
  descs <- list.files(in_path,pattern=".*\\.desc")
  name <- paste0(gsub("([0-9]{7}).*","\\1",descs[1]),
                 "-",
                 gsub("[0-9]{7}-([0-9]{7}).*","\\1",tail(descs,1)))
  datadescs <- lapply(paste0(in_path,descs),dget)
  data_sets <- lapply(datadescs,attach.big.matrix,backingpath=in_path)
  nrows <- sapply(data_sets,nrow)
  ncols <- sapply(data_sets,ncol)
  temp <- filebacked.big.matrix(nrow=sum(nrows),ncol=ncols[1],
                                backingfile=paste0(name,".bin"),
                                descriptor = paste0(name,".desc"),
                                backingpath=out_path)
  
  
  for (i in 1:ncols[1]){
    start = 1
    end = nrows[1]
    for (j in 1:length(nrows)){
      temp[start:end,i] <- data_sets[[j]][,i]
      start <- start + nrows[j]
      if (j != length(nrows)){
        end <- end+nrows[j+1]
      }
      cat("\n finished data set: ", j, "in column: ", i,"\n")  
    }
    cat("\n finished column: ", i, "\n")
  }
  flush(temp)
  gc()
  return(name)
}

feather_to_bigmatrix <- function(in_path="./",out_path="./"){
  require("feather")
  files_to_process <- list.files(in_path,pattern=".*\\.feather")
  files_to_process <- paste0(in_path,files_to_process)
  cols <- c("userID","longitude","latitude","time", "sick", "state")
  pre_loop_time <- proc.time()
  for (i in seq(1,length(files_to_process)-3,by=4)){
    start.time <- proc.time()
    name <- paste0(gsub("([0-9]{7}).*","\\1",files_to_process[i]),
                   "-",
                   gsub("([0-9]{7}).*","\\1",files_to_process[i+3]))
    temp1 <- as.matrix(read_feather(files_to_process[i]))
    temp2 <- as.matrix(read_feather(files_to_process[i+1]))
    temp3 <- as.matrix(read_feather(files_to_process[i+2]))
    temp4 <- as.matrix(read_feather(files_to_process[i+3]))
    temp <- rbind(temp1,temp2,temp3,temp4)
    colnames(temp) <- cols
    BigTemp <- as.big.matrix(temp,
                             backingfile=paste0(name,".bin"),
                             descriptor =paste0(name,".desc"),
                             backingpath = out_path)
    end.time <- proc.time()
    diff.time <- end.time-start.time
    tot.time <- proc.time()-pre_loop_time
    cat("\n Loop: ", i , " finished after ", diff.time[3])
    cat("\n Total time: ",tot.time[3],"\n")
    gc(reset=T)
    #print(paste0("done with loop ",i))
  }
  
}