library(TSP)
library(maps)
library(maptools)

dataframe<-read.csv("data.csv")
random<-function(dataframe,startname,typesel,r,num){

  f_dis<-function(x,y){
    r=6371000
    x=x*pi/180;y=y*pi/180
    a=c(cos(x[2])*cos(x[1]),cos(x[2])*sin(x[1]),sin(x[2]))
    b=c(cos(y[2])*cos(y[1]),cos(y[2])*sin(y[1]),sin(y[2]))
    cosg=sum(a*b)/sqrt(sum(a^2)*sum(b^2))
    dis=r*acos(cosg)
    return(dis)
  }
  start.lon<-dataframe[dataframe$name==startname,]$longtitude
  start.lat<-dataframe[dataframe$name==startname,]$latitude
  start<-cbind(start.lon,start.lat)
  select<-subset(dataframe,type==typesel)
  selcoor<-cbind(select$longtitude,select$latitude)
  index<-c()
  for(i in 1:nrow(select)){
    dis<-f_dis(start,selcoor[i,])
    if(dis<r)
      index<-c(index,i)
  }
  if(length(index)<num){
    output=select[index,]$X
  }else{
    output=select[index,]$X[1:num]
  }
  
  d<-dataframe[dataframe$name==startname,]
  for(i in output){
    d<-rbind(d,dataframe[dataframe$X==i,])
  }
  return(d)
  
}

df<-random(dataframe,"Seaside","library",100000,3)

routeplan<-function(df,startpoint){
  f_dis<-function(x,y){
    r=6371
    x=x*pi/180;y=y*pi/180
    a=c(cos(x[2])*cos(x[1]),cos(x[2])*sin(x[1]),sin(x[2]))
    b=c(cos(y[2])*cos(y[1]),cos(y[2])*sin(y[1]),sin(y[2]))
    cosg=sum(a*b)/sqrt(sum(a^2)*sum(b^2))
    dis=r*acos(cosg)
    return(dis)
  }
  k<-cbind(df$longtitude,df$latitude)
  len<-nrow(df)
  dis_mat<-matrix(NA,len,len)
  for (i in 1:len){
    for(j in 1:len){
      dis_mat[i,j]=f_dis(k[i,],k[j,])
    }
  }
  colnames(dis_mat)<-rownames(dis_mat)<-df$name
  tsp<-TSP(dis_mat)
  tour<-solve_TSP(tsp,method="2-opt")
  path<-as.integer(tour)
  tour_length(tsp,tour)
  tsp_map<-df[path,]
  line<-tsp_map$name
  c1<-line[which(line==startpoint):length(line)]
  c2<-line[1:which(line==startpoint)]
  route<-c(as.vector(c1), as.vector(c2))
  return(route)
}
routeplan(df,"Seaside")


wholeroute<-function(dataframe,startname,typesel,r,num){
  df<-random(dataframe,startname,typesel,r,num)
  route<-routeplan(df,startname)
  return(route)
}
wholeroute(dataframe,"Seaside","library",300000,5)
