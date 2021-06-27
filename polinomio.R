ingmetodpoli<-function(x,coeficientes){
  y<-0
  for(i in 1:length(coeficientes)){
    y<-y+coeficientes[i]*(x^(i-1))
  }
  return(y)
}
x<-1
coeficiente-c(1,2,3)
ingmetodpoli(x,coeficiente)
#Avaliando Polinomio
melhorpolino<-function(x,coeficientes){
  y<-rep(0,length(coeficientes))
  armazena_x<-1
  
  for(i in 1:length(coeficientes)){
    y[i]<-y[i]+coeficiente[i]*armazena_x
    armazena_x<-armazena_x*x
  }
  return(y)
}
x<-2
coeficientes<-c(1,2,3)
melhorpolino(x,coeficientes)
?polyroo
z<-c(6,-5,1)
polyroot(z)
install.packages("PolynomF")
library("PolynomF")
z<-polynom(c(4,5,6))
z
solve(z)
poly_calc(c(1,1,1))
