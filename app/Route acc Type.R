library(TSP)
library(maps)
library(maptools)
library(leaflet)
library(stringr)
library(bitops)
library(ggmap)
library(ggplot2)
library(R.utils)

server<-function(input, output, session){
  output$route <- renderLeaflet({
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

find_geom <- function(x){
  output <- geocode(x, output = "latlona")[,c(1,2)]
  output$type <- "start"
  output$name <- x
  output[,"X"] <- 30000
  return(output)
}



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
  colnames(dis_mat)<-rownames(dis_mat)<-df$X
  tsp<-TSP(dis_mat)
  tour<-solve_TSP(tsp,method="2-opt")
  path<-as.integer(tour)
  tour_length(tsp,tour)
  tsp_map<-df[path,]
  line<-tsp_map$X
  c1<-line[which(line==startpoint):length(line)]
  c2<-line[1:which(line==startpoint)]
  route<-c(as.vector(c1), as.vector(c2))
  return(route)
}

input <- "Columbia University"
start <- find_geom(input)
names(start) <- c("longtitude","latitude","type","name","X")
df <- rbind(df,start)
order <- as.vector(routeplan(df,30000))

col <- NULL
for (i in 1:length(order)){
  col[i] <- which(df$X==order[i])
  
  }
route_df <- df[as.vector(col),c(2,3)]
nn <- nrow(route_df)

# Functions
# =========
viaroute <- function(lat1, lng1, lat2, lng2) {
  R.utils::withTimeout({
    repeat {
      res <- try(
        route <- rjson::fromJSON(
          file = paste("http://router.project-osrm.org/route/v1/driving/",
                       lng1, ",", lat1, ";", lng2, ",", lat2,
                       "?overview=full", sep = "", NULL)))
      if (class(res) != "try-error") {
        if (!is.null(res)) {
          break
        }
      }
    }
  }, timeout = 10, onTimeout = "warning")
  return(res)
}

decode_geom <- function(encoded) {
  scale <- 1e-5
  len = str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat <- dlat <- lng <- dlnt <- b <- shift <- result <- 0
  
  while (index <= len) {
    # if (index == 80) browser()
    shift <- result <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lat = lat + dlat;
    
    shift <- result <- b <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng
    
    array[df.index,] <- c(lat = lat * scale, lng = lng * scale)
    df.index <- df.index + 1
  }
  
  geometry <- data.frame(array[1:df.index - 1,])
  names(geometry) <- c("lat", "lng")
  return(geometry)
}

map <- function() {
  m <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
  return(m)
}

map_route <- function(df, my_list) {
  m <- map()
  m <- addCircleMarkers(map = m,
                        lat = df$latitude,
                        lng = df$longtitude,
                        color = "blue",
                        stroke = FALSE,
                        radius = 6,
                        fillOpacity = 0.8) %>%
    addLayersControl(baseGroups = c("OSM", "Stamen.TonerLite")) %>%
    {
      for (i in 1:length(my_list)) {
        . <- addPolylines(., lat = my_list[[i]]$lat, lng = my_list[[i]]$lng, color = "red", weight = 4)
      }
      return(.)
    }
  return(m)
}

# Main
# ======
m <- map()
m <- m %>% addCircleMarkers(lat = route_df$latitude,
                            lng = route_df$longtitude,
                            color = "red",
                            stroke = FALSE,
                            radius = 5,
                            fillOpacity = 0.8)
print(m)



rownames(route_df)<- c(1:nn)
my_list <- list()
r <- 1
for (i in 1:(nn-1)) {
  for (j in ((i+1):nn)) {
    my_route <- viaroute(route_df$latitude[i], route_df$longtitude[i],route_df$latitude[j], route_df$longtitude[j])
    geom <- decode_geom(my_route$routes[[1]]$geometry)
    my_list[[r]] <- geom
    r <- r + 1
  }
}

print(map_route(route_df, my_list))

  })
}

ui <- fluidPage(
  mainPanel(leafletOutput("route"))
)
  
shinyApp(ui = ui, server = server)