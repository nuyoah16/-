#dplyr包在数据处理方面有很多强大的功能：筛选（多种）/排序/重构变量/重命名变量/分类汇总
library('dplyr')
data=data.frame(name=c('jqy','cjh','yjx','mjj','wxg'),math=c(100,100,100,100,99),biology=c(100,100,90,98,97))
#tbl_data=tibble::as_tibble(data)
data1=filter(data,math==99)
#print(data1)
# print(select(data,1:2))#挑选出前两列
# print(select(data,'math':'biology'))
# print(select(data,starts_with('n')))#对表头进行筛选
# print(select(data,!(starts_with('n'))))#对表头进行筛选
df1=arrange(data,data$math,desc(data$biology))
df2=arrange(data,data$math)
df3=rename(data,ming=name,Chinese=math)#注意旧名和新名的位置
df4=mutate(data,new=math-biology)
print(df1)
print(df2)
print(data)
print(df3)
print(df4)
#select和filter的区别？
