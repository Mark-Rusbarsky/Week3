# makeCacheMatrix() builds 4 functions—set(), get(), setinverse() & getinverse()—          
# returns them within a list to the parent environment, & includes data objects x & m,     
# which are siblings of these 4 functions. Since x is initialized as a function argument,  
# its default value is an empty matrix. But since m is not a function argument,            
# it is set to NULL, initializing it as an object within the makeCacheMatrix() environment 
# to be used by later code in the function.                                                

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL	 
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,	# gives the name 'set' to set() function & get to get() function defined above #
       setinverse = setinverse,	#gives name 'setinverse' to setinverse() function defined above#
       getinverse = getinverse)	#gives name 'getinverse' to getinverse() function defined above#
}  

#	When set() is executed, it (1) assigns the input argument to the x object in the parent    
# environment & (2) assigns the value of NULL to the m object in the parent environment.     
# This clears any value of m that had been cached by a prior execution of cacheSolve().      
# Thus, if there is already a valid inverse cached in m, whenever x is reset, the value of   
# m cached in the memory of the object is cleared, forcing subsequent calls to               
# cacheSolve() to recalculate the inverse rather than retrieving the wrong value from cache. 
# If aMatrix <- makeCacheMatrix(matrix(1:4, 2, 2)), then x contains matrix(1:4, 2, 2), even   
# though aMatrix$set() has not been executed. This is because the value matrix(1:4, 2, 2)    
# was passed as an argument into the makeCacheMatrix() function. cacheSolve() starts with a  
# single argument x & an ellipsis that allows the caller to pass additional arguments into   
# the function. Next, it attempts to retrieve an inverse from the object passed in as the    
# argument. First, it calls the getinverse() function on the input object as follows         
# m <- x$getinverse(). Then it checks to see whether the result is NULL. Since               
# makeCacheMatrix() sets the cached inverse to NULL whenever a new matrix is set into the    
# object, if the value here is not equal to NULL, there is a valid, cached inverse & can     
# return it to the parent environment. If the result of !is.null(m) is FALSE, cacheSolve()   
# gets the matrix from the input object, calculates solve(), uses the setinverse() function  
# on the input object to set the inverse in the input object & then returns the value of the 
# inverse to the parent environment by printing the inverse object. Note only cacheSolve()   
# executes solve() function, so makeCacheMatrix() is incomplete without cacheSolve().        

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
# To see how it works try the following:									                                                             
# aMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# aMatrix$get()                             # retrieve the value of x
# aMatrix$getinverse()                      # retrieve the value of m, which should be NULL
# cacheSolve(aMatrix)                       # to calculate inverse of matrix(1:4, 2, 2)
# aMatrix$getinverse()                      # retrieve it directly now that it is cached
# aMatrix$set(matrix(c(-2, 4, 1, -3),2,2))  # reset value with a new matrix
# cacheSolve(aMatrix)                       # to calculate inverse of matrix(c(-2, 4, 1, -3),2,2)
# aMatrix$getinverse()                      # retrieve the new matrix directly now that it is cached