# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# PUNTO 1

ap<-function(xi){
  ea=20
  while(ea>0.05){
    xii=1/2*(xi+(2/xi))
    ea=abs((xii-xi)/xii)
    er=abs((sqrt(2)-xii)/sqrt(2))
    xi=xii
    print(xii)
    print(ea)
    print(er)
  }
}

ap(1)



# PUNTO 2
factorial<-function(n){
  fact = 1;
  i = 1;
  if(n==0){return(1)}
  for(i in 1:n){
    fact = fact * i;

  }

  return(fact)
}

i = 0
x = 0.5
e = 0
eant =0
ea=1

while(ea>0.05){
  e = e+((x^i)/factorial(i))
  ea=errorAbsoluto(e,eant)
  print(c("errorAbs",ea))
  print(c("errorRela",errorRelativoAproximado(exp(x),e)))
  eant=e
  i=i+1
}
print(c("exponencial",e))

errorAbsoluto<-function(val_act,val_ant){

      return(abs((val_act-val_ant)/ val_act))
  }
errorRelativoAproximado<-function(val_presente, val_aprox_ant){

  return(abs((val_presente-val_aprox_ant)/val_presente))
}





# PUNTO 3
factorial<-function(n){
  fact = 1;
  i = 1;
  if(n==0){return(1)}
  for(i in 1:n){
    fact = fact * i;

  }

  return(fact)
}

i = 0
x = pi/2
sen = 0
eant =0
ea=1

while(ea>0.05){
  sen =sen+ ((-1)^i*(x^2*(i+1)))/(factorial(2*(i+1)))
  ea=errorAbsoluto(sen,eant)
  print(c("errorAbs",ea))
  print(c("errorRela",errorRelativoAproximado(sin(x),sen)))
  eant=sen
  i=i+1
}
print(c("exponencial",sen))

sin(x)

errorAbsoluto<-function(val_act,val_ant){

  return(abs((val_act-val_ant)/ val_act))
}
errorRelativoAproximado<-function(val_presente, val_aprox_ant){

  return(abs((val_presente-val_aprox_ant)/val_presente))
}


