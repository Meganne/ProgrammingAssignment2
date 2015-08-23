## Because matrix inversion is usually a cumbersome calculation, one can
## cache the inverse of a matrix in order to increase the speed of the calculation
## and avoid executing it repeatedly.The following two functions accomplish this.


## makeCacheMatrix is a function that creates a matrix, within which is a function
## to define the values in the matrix, retrieve the values in the matrix, calculate
## the inverse of the matrix, and finally retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinv<-function(inverse)i<<-mean
        getinv<-function()i
        list(set=set,get=get,
             setinv=setinv,
             getinv=getinv)        
}


## cacheSolve is a function that solves for the inverse of the matrix as 
## defined by the function makeCacheMatrix. If it has already been solved,
## the function pulls the solution from the cache. If not, it solves the 
## matrix and adds the solution to the cache. 

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}


## Return a matrix that is the inverse of 'x'

test_matrix<-makeCacheMatrix(matrix(rnorm(9),3,3))
test_matrix$get()
cacheSolve(test_matrix)