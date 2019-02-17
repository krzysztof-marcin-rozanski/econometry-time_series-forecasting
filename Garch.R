#GARCH(1,1)
Garch<-function(a,b,w,n){
  eps0<-0
  vol0<-0
  rn<-rnorm(n, mean = 0, sd = 1)
  eps<-eps0
  vol<-vol0
  for(i in 1:n-1){
    volnext<-w+(a*(eps[i]^2))+(b*(vol[i]))
    epsnext<-sqrt(volnext)*rn[i] 
    eps<-c(eps,epsnext)
    vol<-c(vol,volnext)
  }
  
  return(eps)
}

#GARCH(1,1) Figure 1 & Figure 2
set.seed(42)
garch1<-Garch(0.2,0.7,1,500)
set.seed(42)
garch2<-Garch(0.7,0.2,1,500)
par(mfrow=c(2,1))
plot.ts(garch1)
plot.ts(garch2)


#GARCH(p,q)
Garch_pq<-function(a,b,w,n){
  p<-length(b)
  q<-length(a)
  eps0<-rep(0,max(p,q))
  vol0<-rep(0,max(p,q))
  rn<-rnorm(n, mean = 0, sd = 1)
  eps<-eps0
  vol<-vol0
  for(i in max(p,q):n-1){
    volnext<-w+(sum(a*(eps[(i-q+1):i]^2)))+(sum(b*(vol[(i-p+1):i])))
    epsnext<-sqrt(volnext)*rn[i] 
    eps<-c(eps,epsnext)
    vol<-c(vol,volnext)
  }
  return(eps)
}

#GARCH(p=3, q=3) Figure 3
par(mfrow=c(1,1))
set.seed(42)
g<-Garch_pq(a=c(0.05,0.04,0.03), b=c(0.05,0.04,0.3), 1, 100)
plot.ts(g)

#GARCH(1, 1) and GARCH(p=1, q=1)  comparison Figure 4
par(mfrow=c(2,1))
set.seed(42)
g_pq<-Garch_pq(0.7,0.2,1,200)
set.seed(42)
g_11<-Garch(0.7,0.2,1,200)
plot.ts(g_pq)
plot.ts(g_11)

cat("The end of Garch script.\n")
