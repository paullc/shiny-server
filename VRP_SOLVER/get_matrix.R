get_matrix<-function(datos){
        library(ggmap)
        result<-list()
        distance<-matrix(rep(0,nrow(datos)*nrow(datos)),nrow = nrow(datos),ncol = nrow(datos))
        time<-matrix(rep(0,nrow(datos)*nrow(datos)),nrow = nrow(datos),ncol = nrow(datos))
        suppressMessages(for(i in 1:nrow(datos)){
                for(j in 1:nrow(datos)){
                        if(i==j){
                                distance[i,j]<-0
                                time[i,j]<-0
                        }
                        else { 
                                route_dist<-data.frame()
                                route_dist<-mapdist(paste(datos[i,2:3],collapse = " "),paste(datos[j,2:3],collapse = " "))
                                distance[i,j]<-route_dist$km
                                time[i,j]<-route_dist$minutes
                        }
                }
        })
        result[[1]]<-distance
        result[[2]]<-time
        result[[3]]<-c(datos[2:nrow(datos),4])
        names(result)<-c("Distance","Time","Demand")
        result
}