melhorpolino<-function(x,coeficiente){
  y<-rep(0,length(coeficiente))
  armazena_x<-1
  
  for(i in 1:length(coeficiente)){
    y[i]<-coeficiente[i]*armazena_x
    armazena_x<-armazena_x*x
  }
  return(y)
}

coeficiente<-c(30,-11,1,3)
x<--12
melhorpolino(x,coeficiente)
sum(melhorpolino(x,coeficiente))

bissecao<-function(f,a,b,erro=1e-3,n=200){
  itera<-0
  f1<-f(a)
  f2<-f(b)
  while(abs(b-a)>erro){
    itera<-itera+1
    if(itera>n){
      warning("O número máximo de iterações foi excedido")
      break
    }
    xmedio<-(a+b)/2
    ymedio<-f(xmedio)
    if(f1*ymedio>0){
      a<-xmedio
      f1<-ymedio
    }
    else{
      b<-xmedio
      f2<-ymedio
  }
  }
}
f(0)
f(-5)

bissecao(f,0,-5)