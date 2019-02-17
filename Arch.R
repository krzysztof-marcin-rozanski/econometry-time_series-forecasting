#ARCH(1)
Arch<-function(a,w,n){
  eps0<-0
  vol0<-1
  rn<-rnorm(n, mean = 0, sd = 1)
  eps<-eps0
  vol<-vol0
  for(i in 1:n-1){
    volnext<-w+(a*(eps[i]^2))
    epsnext<-sqrt(volnext)*rn[i] 
    eps<-c(eps,epsnext)
    vol<-c(vol,volnext)
  }
  
  return(eps)
}

#ARCH(1) Figure 1
set.seed(42)
arch1<-Arch(0.5,1,500)
plot.ts(arch1)
#ARcH(1) Figure 2
set.seed(42)
arch2<-Arch(0.95,1,500)
plot.ts(arch2)
#ARCH(1) Figure 3
set.seed(42)
arch3<-Arch(1.1,1,500)
plot.ts(arch3)
#ARCH(1) Figure 4
set.seed(42)
arch4<-Arch(3,1,200)
plot.ts(arch4)
#ARCH(1) Figure 5
set.seed(42)
plot.ts(arch4[100:140])

#ARCH(1)  Figure 6, a=4 being explosive
set.seed(42)
archexp<-Arch(10,1,200)
plot.ts(archexp)


cat("The end of Arch script.\n")