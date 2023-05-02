for(i in 1970:2020){
  if(i%%400==0){
    print(paste(i,'为闰年'))
  }else if(i%%4==0&i%%100!=0){
    print(paste(i,'为闰年'))
  }else{
    print(paste(i,'为平年'))
  }
}

