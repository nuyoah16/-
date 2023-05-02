gx=function(x){
  out1=3*(x^5)-4*(x^4)+6*(x^3)+4*x-4
  return(out1)
}
dgx=function(x){
  out2=15*(x^4)-16*(x^3)+18*(x^2)+4
  return(out2)
}
x0=1.4
res=1
while(res>=10^(-10)){
  y=x0-(gx(x0)/dgx(x0))
  res=gx(y)
  x0=y
}
print(y)

