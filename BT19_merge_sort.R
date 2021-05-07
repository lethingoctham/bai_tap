Merge <- function(a, p, q, r){
  n1 <- q - p + 1
  n2 <- r - q
  L <- numeric(n1+1)
  R <- numeric(n2+1)
  for(i in 1:n1){
    L[i] <- a[p+i-1]
  }
  for(j in 1:n2){
    R[j] <- a[q+j]
  }
  L[n1+1] <- Inf
  R[n2+1] <- Inf
  i=1
  j=1
  for(k in p:r){
    if(L[i] <= R[j]){
      a[k] <- L[i]
      i <- i +1
    }else{
      a[k] <- R[j]
      j <- j+1
    }
  }
  a
}
Merge(c(1,3,5, 2,4,6), 1, 3, 6)

