# while(max(abs(d))>=10^(-5)){
#   #browser()
#   beta_new=beta0+alpha*gx(y,beta0,x1,x2,s)
#   d=beta_new-beta0
#   beta1=c(beta1,beta_new[1])
#   beta2=c(beta2,beta_new[2])
#   beta3=c(beta3,beta_new[3])
#   beta0=beta_new
#   times=times+1
#   all_times=c(all_times,times)
#   if(max(abs(d))<=10^(-4)){
#     beta1_f=c(beta1_f,beta_new[1])
#     beta2_f=c(beta2_f,beta_new[2])
#     beta3_f=c(beta3_f,beta_new[3])
#   }
# }

#debug(gx)
x1=c(1,2,3)
x2=c(2,2,2)
y=c(2,3,4)
b=data.frame(Y=y,X1=x1,X2=x2)
print(b)
beta=c(1,2,3)
index=seq(1:3)
a1=b$Y[1]-1+(exp(-(beta[1]+beta[2]*b$X1[1]+beta[3]*b$X2[1]))/(1+exp(-(beta[1]+beta[2]*b$X1[1]+beta[3]*b$X2[1]))))
a2=matrix(c(1,b$X1[1],b$X2[1]))
print(a1)
print(a2)
# gx(b,beta,index)
# gx=function(total,beta,index){
#   out=matrix(0,3,1)
#   for(i in index) {
#     temp1=total$Y[i]-1+(exp(-(beta[1]+beta[2]*total$X2[i]+beta[3]*total$X3[i]))/(1+exp(-(beta[1]+beta[2]*total$X2[i]+beta[3]*total$X3[i]))))
#     temp2=matrix(c(1,total$X2[i],total$X3[i]))
#     temp3=temp1*temp2
#     out=out+temp3
#   }
#   return(out)
# }

