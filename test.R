# learn from thread
# a <- makeVector(c(1,2,3))
# class(a) is a list
# class(a$get) is a function 

makeVector <- function(x=numeric()){
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x 
    setmean <- function(mean) m <-- mean
    getmean <- function() m
    list(set=set, get=get, setmean=setmean, getmean=getmean)    
}

cachemean <- function(x,...){
    m <- x$getmean()  #query the x vector's cache 
    if(!is.null(m)){  # if there a cache 
        message("getting cached data")
        return(m)     # just return the cache 
    }
    data <- x$get()   # if there is no cache 
    m <- mean(data,...) # we actually compute them here
    x$setmean(m)      # save the result back to x's cache 
    m                 # return the result 
}

# create 100,000 dimension vectors 
test <- rnorm(100000)


#Notice that in the swatWithNewspaper function, we used the <<- operator to store 
#a value into the variable noOfBarks. If we had used <- to assign this value, we 
#would in effect have created a new variable called noOfBarks that would only have 
#existed within that particular function. By using <<- instead, we told R that we 
#are referring to the variable noOfBarks that already exists as a property of
#whichever instance of dog we are dealing with. In the assignment, we can see an 
#example of this in the function setmean.

dog <- function(name = "Fido", owner = "Bob", barkType = "Woof!", age = 3) {
    noOfBarks <- 5
    swatWithNewspaper <- function() {
        cat("Yelp!\n")
        noOfBarks <<- sample(3:10, 1) # random number from 3-10 
    }
    bark <- function() {
        for (i in 1:noOfBarks) cat(barkType)
    }
    info <- function() {
        cat(paste("Name: ", name))
        cat(paste("\nOwner: ", owner))    
        cat(paste("\nAge: ", age))
        cat("\n")
    }
    list(swatWithNewspaper = swatWithNewspaper, bark = bark, info = info)
}
 
snookums <- dog(name = "Snookums", owner = "Granny Smith", barkType = "Yap!", age = 12)

# function creates a special matrix object that can cache its inverse 
makeCatcheMatrix <- function(){
    
}

# function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
cacheSolve <- function(){
    
}