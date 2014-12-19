#returns cache matrix object list
#list has three functions get,set, getinverse & setinverse
#usage
#source('cacheMatrix.R')
#cacheAble <- makeCacheMatrix(m)
#cacheSolve(cacheAble)
#cacheSolve(cacheAble) -- this time it will come from cache.
makeCacheMatrix <- function(m = matrix()) {
	 mInverse <- NULL
	 set <- function(y) {
			m <<- y
            mInverse <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) mInverse <<- inverse
        getinverse <- function() mInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

#cached solve object
#first check if cached version exists else computes
#matrix inverse and returns the value.
cacheSolve <- function(m, ...) {
	mInverse <- m$getinverse();
	if (!is.null(mInverse)) {
		message("returning cached version");
		return(mInverse);
	}
	
	#get the matrix object.
	matrixObj <- m$get();
	
	#compute inverse of the matrix using solve.
	#check for square matrix is handled by solve.
	mInverse <- solve(matrixObj);
	
	#set the inverse so that next time we can serve from cache.
	m$setinverse(mInverse);
	mInverse;
}
