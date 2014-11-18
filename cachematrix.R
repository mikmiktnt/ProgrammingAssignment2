# makeCacheMatrix create a list contaning methods that can be later recall from 
# chaceSolve in order to get/set the starting matrix and is inverse
makeCacheMatrix <- function(x = matrix()) {
    
    i<-NULL #reset the matrix inverse to NULL
  
    get<-function(){x} #function returning the matrix
    
    setinverse<-function(inverse){i<<-inverse} #function setting the inverse of 
                                               #the matrix calculated with 
                                               #cachesolve
    
    getinverse<-function(){i} #function returning the inverse of the matrix
    
    list(get=get, setinverse=setinverse, getinverse=getinverse) #list of all the
                                                                 #functions
    
}

# cacheSolve check the existance of the inverse of the matrix defined in 
# makeCacheMatrix. If the inverse already exists the cached values is returned 
# otherwise is calculated

cacheSolve <- function(x, ...) {
# check the existance of the inverse  
    
    i<-x$getinverse()
    
    if(!is.null(i)){
        
        message("Getting cached data")
        
        return(i)
        
    }
#calculate the inverse if !is.null(i)=FALSE    
    data<-x$get()
    
    i<-solve(data)
    
    x$setinverse(i)
    
    i
}
