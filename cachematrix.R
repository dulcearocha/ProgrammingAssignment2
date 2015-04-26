## Las siguientes funciones calculan la inversa de una matriz y guarda en la memoria cache, de tal manera que, 
## cualquier usuario que necesite calcular la matriz inversa, el valor guardado previamente se devuelve en lugar 
## de repetir todos los calculos

## Esta función crea un objeto especial de "matrix", que es realmente una lista que contiene una función para: 
## para establecer el valor de la matriz; obtener el valor de la matriz, establecer el valor de la inversa 
## y obtener el valor de la inversa

makeCacheMatrix <- function(x = matrix()){
  i  <- NULL
  set <- function(y){
  	x <<- y
      i <<- NULL
  }
  get <- function() x
  setinversa <- function(inv) i <<- inv
  getinversa <- function() i
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}


## La siguiente función calcula la inversa de la "matrix" especial creado a partir de la función anterior

cacheSolve <- function(x = matrix(), ...) {
    i <- x$getinversa()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinversa(i)
    i
}

