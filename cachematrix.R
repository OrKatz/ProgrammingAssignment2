## functions: makeCacheMatrix and cacheSolve - calculating matrix inverse and caching results
## by using these functions user will be able to calc store and retrive matrix inverse data

## This function is caching the inverse matrix value
#by creating an makeCacheMAtrix object that get a verible named matrix we will be able 

makeCacheMatrix <- function(x = matrix()) {
        
        v <- NULL
        #set the original matrix value to cache
        set <- function(y)
        {
                x <<- y
                v <<- NULL
        }
        
        #return the original matrix
        get <- function() x
        
        # setting the inverse matrix to cache
        setinverse <- function(inverse) v <<- inverse
        
        #getting the inverse matrix value (from cache) 
        getinverse <- function() v
        
        #return list of methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
                
}


## This function checking if matrix inverse value is in cache and if not calc the value
## function get a makeCacheMatrix object x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinverse()
        
        #if cached value exists returning v cache value
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        # getting the original matrix
        data <- x$get()
        
        #calcuating inverse matrix
        v <- solve(data)  %*% data
        
        #setting to cache (using setinverse())
        x$setinverse(v)
        
        #return the inverse matrix
        v
}
