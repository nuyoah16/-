#y=sqrt(abs(x))图像绘制
gx=function(x){
  out1=matrix(0,1,length(x))
  for(i in 1:length(x)){
    if(x[i]>=0){
      out1[i]=sqrt(x[i])
    }else{
      out1[i]=sqrt(-x[i])
    }  
  }
  return(out1)
}
x=seq(-5,5,0.01)
y=gx(x)
plot(x,y,type='l')