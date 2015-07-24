## These functions are created to cache the inverse of a matrix. 
## makeCacheMatrix creates the special cache matrix, 
## cacheSolve returns the chaced Matrix inverse 


## This function creates a special matrix which is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL 
        set <- function(y) { #set the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,  #return a list of functions to set and get the matrix and solve and set ad get the inverse
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is to return the inverse of a matrix using a cached inverse if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinverse() # get the inverse from the funtion argument
        if(!is.null(m)) { # if the inverse exists,i.e has been calculated previously,return m
                message("getting cached data")
                return(m)
        }

	## if matrix inverse does not exist, solve it and return m
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
