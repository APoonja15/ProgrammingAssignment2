
# MakeCacheMatrix- This function creates a special matrix, which is able to store 
# its inverse in a cache using the principles of lexical scoping. 

makeCacheMatrix <- function(myMatrix = matrix()) {
  m <- NULL
  set <- function(y) {
    myMatrix <<- y
    m <<- NULL
  }

# the "set" function uses the argument y, which derives its value from the parent 
# environment using the super assignment operator <<- . It takes the value of the 
# "myMatrix" argument used in the "makeCacheMatrix" function  
    
  get <- function() myMatrix

# the "get" function retrieves the value of the matrix stored in the parent 
# function, using the "myMatrix" argument 
  
  setInverse <- function(solve) m <<- solve

# "setInverse" allows "makeCacheMatrix" to access the value of the m variable, 
#  which is defined later in cacheSolve. It accesses the value of m using 
#  lexical scoping
  
  getInverse <- function() m

# "getInverse" allows "makeCacheMatrix" to access the value of the inverse of 
# MyMatrix, since it takes the argument m, which is defined within the parent 
# environment  

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
# the purpose of the list is to assign names to each of the four functions within
# makeCacheMatrix. This allows the individual arguments of this function to be referenced 
# using the $ extract operator
}



# cacheSolve - Returns a matrix that is the inverse of "myMatrix" by using the 
# setInverse function in makeCacheMean. However, if the
# inverse of myMatrix has already been calculated, the value returned by 
# cacheSolve is obtained from the cache "makeCacheMatrix"

cacheSolve <- function(myMatrix, ...) {
  m <- myMatrix$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

# the if statement above evaluates the condition of m not being equal to NULL. 
#  if this condition is met, the code block is executed. If not, the code below 
#  is run instead     
  
  data <- myMatrix$get()
  m <- solve(data, ...)
  myMatrix$setInverse(m)
  m
  
# if the value of m is equal to NULL, matrix "myMatrix" is stored in a new 
# local variable "data". 
# The value of m is then assigned to the output of the solve function with the 
# argument being the retrieved matrix. Solve inverts the original matrix. 
# the function finishes by storing the new matrix as an argument to the setInverse function 
# and returning m to the parent environment.
}

