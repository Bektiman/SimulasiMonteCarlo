SimulasiPhi <- function(n,tol=NULL){
par(mfrow=c(2,1))
plot(-1:1,-1:1, type = "n", xlab = "Random X", ylab = "Random Y")
phi <- NULL
jml <- 0
hit <- 0
for (i in 1:n) {
  data <- matrix(runif(2,-1,1))
  beda <- sqrt(data[1]^2+data[2]^2)
  data <- cbind(data,beda)
  points(data[1],data[2],col=ifelse(beda<=1,"blue","red"))
  jml <- jml +1
  if (beda <= 1 ) { 
    hit <- hit + 1
  }
  
  phi[i] <- 4*hit/jml
  
  if (!is.null(tol)){
    if(abs(3.14159265358979323846-phi[i])<tol){
      a <- "Batas Toleransi dalam Perulangan"
      break
    } else{
      a <- "Batas Toleransi diluar Perulangan"
    }
    
  }else{
    a <- "Tanpa batas toleransi"
  }
}
plot(c(0,n),c(0,5), type="n",xlab = "Jumlah Iterasi", ylab = "Nilai Phi")
lines(phi, col="black")
abline(h= 3.14159265358979323846, col="red")
solusi <- print(list("Nilai Phi" = phi[jml],"Jumlah Perulangan" =jml,"Keadaan"=a))
return(solusi)
}
SimulasiPhi(10,0.01)
fungsi <- function(x,y=NULL,z=NULL) {
  #y<-((sin(10*x^2))^2*sin(x))*x+.1
  #y<-(1/sqrt(2*3.141592))
  #y <- x^2
   y <- cos(50*x) + sin^2(20*x)
  #y<-log(x)
  return(y)
}
mins <- function(func,a,b) {
  opt <- optimize(func,interval = c(a,b), maximum = F)
  return(floor(opt$minimum))
}
maxs  <- function(func,a,b) {
  opt <- optimize(func,interval = c(a,b), maximum = T)
  return(ceiling(opt$maximum))
}
IntegralPlot <- function(func,a,b,n) {
  jml <- 0
  hit <- 0
  t <- mins(func,a,b)
  s <- maxs(func,a,b)
  listing <- abs(matrix(c(a,b,s,t,func(a),func(b))))
  batas <- max(listing)
  plot(0:1,0:3,type = "n", xlab = "X", ylab = "f(x)")
  curve(func,a,b,col= "black", lwd=2, add = T )
  abline(h = 0,v = 0)
  for (i in 1:n) {
    x <- matrix(runif(1,a,b))
    y <- matrix(runif(1,0,batas))
    fx <- func(x)
    points(x,y,col=ifelse(abs(y) <= abs(fx) ,"blue","red"))
    jml <- jml +1
    if (abs(y) <= abs(fx)) { 
      hit <- hit + 1
    }
    
  }
  pp <- abs(b-a)*abs(s-t)
  luas <- hit/jml*pp
  return(luas)
}
IntegralPlot(fungsi,0,1,1000)
SimulasiIntgrl(fungsi,0,2,10000)
plot(a:b,fungsi(a):fungsi(b), type = "n", xlab = "X", ylab = "f(x)")
curve(fungsi,0,1,col= "black", lwd=2, add = T )
fungsi(2)
SimulasiIntgrl(fungsi,0,1,1000)
log(0.1)
?log
integrate(f = fungsi,lower = 0,upper = 4)

Perbandingaintegral <- function(f,a,b,n=1000) {
  t <- mins(f,a,b)
  s <- maxs(f,a,b)
  listing <- abs(matrix(c(a,b,s,t,f(a),f(b))))
  batas <- max(listing)
  sumy <- 0
  ys <- 0
  hit <- 0
  hasil <- NULL
  result <- NULL
  for (i in 1:n) {
    a1 <- runif(1,a,b)
    b1 <- runif(1,0,batas)
    y <- f(a1)
    ys <- ys + y
    sumy <- sumy + 1 
    hasil[i] <- abs(b-a)*ys/sumy 
    if (abs(b1) <= abs(y)) { 
      hit <- hit + 1
    }
    result[i] <- abs(b-a)*abs(batas-0)*hit/sumy
    }
  plot(c(0,n),c(0,1), type="n",xlab = "Jumlah Iterasi", ylab = "Nilai Fungsi")
  lines(hasil, col="black")
  lines(result, col="blue")
  abline(h= 0, col="red")
  cat("hasil pendekatan plot = ",result[n])
  cat("\nhasil pendekatan mean =",hasil[n])
  return()
}
Perbandingaintegral(fungsi,0,1,1000)
integrate(fungsi,0,1)
a <- runif(1,0,1)
fungsi(a)
 a <-integrate(fungsi,0,1)
a
fungsi <- function(x,y=NULL,z=NULL) {
  #y<-((sin(10*x^2))^2*sin(x))*x+.1
  #y<-(1/sqrt(2*3.141592))
  #y <- x^2
  #y <- cos(50*x) + sin^2(20*x)
  y <- (cos(50*x) + sin(20*x))^2
  #y<-log(x)
  return(y)
}