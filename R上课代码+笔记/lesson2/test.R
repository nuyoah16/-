x=readline(prompt = '请输入年份')
x=as.numeric(x)
if(x%/%400==0){
  print('闰年')
}else if(x%/%4==0&x%/%100!=0)
  {
  print('闰年')
}else{
  print('平年')
}
