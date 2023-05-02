gx=function(x){
    if(x>=0){
      out1=sqrt(x)
    }else{
      out1=sqrt(-x)
    }  
  return(out1)
}

dgx=function(x){
    if(x>=0){
      out2=1/sqrt(x)
    }else{
      out2=-1/sqrt(-x)
    } 
  return(out2)
}

x0=0.01
res=1
while(res>=10^(-10)){
  y=x0-(gx(x0)/dgx(x0))
  res=gx(y)
  x0=y
}
print(y)

