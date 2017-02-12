#function to format numbers in an array optimally for output on plots; 
# "n" indicates the nth power of ten after which scientific notation is preferred
# for example, "n = 4" means that "1000" is written as "1000", but "10000" is written as "1e+04"
# "digits" is the number of significant digits that should be used when displaying digits
# any numbers smaller than order(10^(5-digits)) are written in non-scientific format by default
format_scientific <- function(unformatted_array,n=5,digits=2){
  return(unlist(lapply(unformatted_array,format,scientific=max(n-(5+digits),-5+digits),digits=digits)))
}
