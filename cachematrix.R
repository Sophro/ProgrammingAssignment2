# Course 2 ,Week 3, Assignment 2
# get a matrix (any dimension); keep the inverse calculated by cacheSolve
# m1  is a matrix, a1 <- makeCacheMatrix(m1)
# a1 will be = NULL the first time and then will be the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# Course 2 ,Week 3, Assignment 2
# calculates the matrix inverse using solve 
# m1  is a matrix, a1 <- makeCacheMatrix(m1)
# a1Inv <- cacheSolve(a1)
# a1$get() %*% a1Inv --> matrix identity 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
