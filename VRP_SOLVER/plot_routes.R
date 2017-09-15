plot_routes<-function(solution,coordenadas){
        library(ggplot2)
        library(ggmap)
        center<-c(long=mean(coordenadas[,3]),lat=mean(coordenadas[,2]))
        base_map<-qmap(center,zoom=11,maptype="roadmap")
        route_map<-base_map
        colors<-c("dodgerblue4","darkorchid4","green3","red2","yellow1","darkorange1")
        suppressMessages(for(i in 1:length(solution)){
                iter_route<-vector()
                legs<-data.frame()
                iter_route<-as.numeric(unlist(strsplit(solution[i],"-")))
                for(j in 1:(length(iter_route)-1)){
                        legs<-rbind(legs,route(c(coordenadas[iter_route[j]+1,3],coordenadas[iter_route[j]+1,2]),c(coordenadas[iter_route[j+1]+1,3],coordenadas[iter_route[j+1]+1,2]),mode = "driving",alternatives = FALSE))
                }
                legs$leg<-seq(1,nrow(legs),1)
                route_map<-route_map + geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),alpha=0.8,size=1,color=colors[i],data=legs)
        })
        route_map
        
}