fibonacci<-function(k){
  if(k==1)
    return(1)

  return(fibonacci(k-2)+(fibonacci(k-1)))
}
fibonacci(5)
#append

termosfibo<-function(n){
  x<-NULL
  x[1]<-fibonacci(1)
  x[2]<-fibonacci(2)
  for(i in 3:n){
    x<-append(x,fibonacci(i))
  }
  return(x)
}
x<-c(1,2,3)
class(x)
is.integer(x)
is.character(x)
x<-c(1,2,3)
#vetor
#lista
x<-c("a",2,3,4,"c")
x
x<-list("a",2,3,4)
x
y<-1:30
y
mat<-matrix(y,nrow=6,byrow=TRUE)
mat
mat<-as.data.frame(mate)
mat

x<-1:1000
sum(x)
prod(x)
x

somavetorial<-function(z){
  soma<-0
  taman<-length(z)
  
  for(k in 1:taman){
    soma=soma=z[k]
  }
  return(soma)
}
z<-1:100
z
somavetorial(z)
#Metodo esperto

somaEsperta<-function(z){
  k<-length(z)
  
  if(k==1){
    return(z)
  }
  j=floor(k/2)
  
  return(somaEsperta(z[1:j])+somaEsperta(z[(j+1):k]))
}
somaEsperta(z)
