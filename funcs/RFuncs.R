nNN<-function(img,r){
    ## BY-IMAGE computation:
    ## number of (nearest) neighbors (with centroids) within a ball of radius r
    ##   around each feature's centroid within the image named img.
    y.i<-y[y$uniqueImage==img,c("Location_Center_X","Location_Center_Y")]
    ## pairwise Euclidean distances
    d.x<-(outer(y.i$Location_Center_X,y.i$Location_Center_X,FUN="-")^2)
    d.y<-(outer(y.i$Location_Center_Y,y.i$Location_Center_X,FUN="-")^2)
    dist<-sqrt(d.x + d.y)
    dist<-(0.5*dist + 0.5*t(dist))
    ## number of (all) neighbors within a ball of radius=r
    n.nbrs<-apply(dist<r,1,sum)
    return(n.nbrs)
}
