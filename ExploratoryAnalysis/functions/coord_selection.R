#function to make selection of datatable based on coordinate (lon_west,lon_est,lat_south,lat_north); also returns index
coord_selection  <- function(datatable,coord_selec) #
{
  index <-datatable[,longitude >=coord_selec[1] & longitude <= coord_selec[2] 
                    & latitude >= coord_selec[3] & latitude <=coord_selec[4]]
  selec <- datatable[index,]
  
  index <- which(index)
  return(list(selec,index))
}