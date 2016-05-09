
##This function is a special function which computer the inverse of a matrix
##Also this function set the values to environment
makeCacheMatrix <- function(x = matrix()) {
	invmatrix<-NULL
        set<-function(m1){
	x<<-m1
	invmatrix<<-NULL
	}
	get <-function() x
	setinverse<- function(solve) invmatrix <<- solve
	getinverse<- function() invmatrix
	list(set=set,get=get,
	setinverse=setinverse,
	getinverse=getinverse)
}



##If inverse exists, environment values is displayed else its calculated on the fly
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invm<-x$getinverse()
	if(!is.null(invm)){
	message("getting cached data")
	return(invm)
	}
	data<-x$get()
	invm<-solve(data,...)
	x$setinvm(invm)
	invm
}
