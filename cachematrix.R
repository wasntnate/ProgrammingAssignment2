#####
#
#  cacheMatrix.R, function for caching matrix inverse results
#
#  (c) Nate Bachmeier 2014
#
#####

#
# Make sure MASS library is loaded 
library(MASS);


# Function to hold computed values
#   Parameters: x (matrix)
makeCacheMatrix <- function(x = matrix()) {

  cached <- NULL;
  
  # Get the contained value
  get <- function(){ x;}
  
  # Update the contained value
  set <- function(y){ 
    x <<- y;
    cached <<- NULL;
  }
  
  # Get the inverted matrix
  inverse <- function(){
    #
    # If not in cache, compute
    if(is.null(cached)){
      cached <- ginv(x);
    }
    
    #return the result
    invisible(cached);
  }
  
  #
  # Return to caller the "interface"
  invisible(list(set=set, 
                 get=get, 
                 inverse=inverse));
}


#
# A wrapper function around makeCacheMatrix
cacheSolve <- function(x, ...) {
  #
  # Create wrapper
  mcm <- makeCacheMatrix(x);
  
  #
  # Compute Inverse
  inverseMatrix <- mcm$inverse();
  
  #
  # Call second time to make sure caching worked
  inverseMatrix2 <-mcm$inverse();
  
  #
  # Return result
  invisible(inverseMatrix);
}
