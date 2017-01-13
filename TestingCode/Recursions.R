#see https://www.programiz.com/r-programming/recursion

#recursive function to find factorial
recursive.factorial <- function(x) {
  if (x==0) return(1)
  else return (x*recursive.factorial(x-1))
}

recursive.factorial(5)
