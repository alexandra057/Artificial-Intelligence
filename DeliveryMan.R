library("DeliveryMan")

# Function for getting the h-value (i.e. the heuristic)
getHscore <- function(child, goal){
  return (abs(child[1] - goal[1]) + abs(child[2] - goal[2]))
}

getGscore <- function(child, current, combinedCost){
  cost = 0
  
  childx = child[1]
  childy = child[2]
  currentx = current[1]
  currenty = current[2]
  
  # Checks what position the current child has relative to the
  # Current node, and then return the edge-cost based on if we attempt
  # To move horizontally or vertically
  if ((childx - currentx) == 1){
    # Move right
    cost = hroads[x,y]
  }else if ((childx - currentx) == -1){
    # Move left
    cost = hroads[x-1,y]
  }else if ((childy - currenty) == 1){
    # Move upwards
    cost = vroads[x,y]
  }else{
    # Move downwards
    cost = vroads[x,y-1]
  }
  return(combinedCost + cost)
}

getChildren <- function(current, xSize, ySize) {
  children <- matrix(, nrow = 4, ncol=2, byrow = TRUE)
  # Add all possible horizontal and vertical neighbors
  x <- current[1]
  y <- current[2]
  # Possible x-neighbors
  children[,1] <- c(x-1, x, x, x+1)
  # Possible y-neighbors
  children[,2] <- c(y, y+1, y-1, y)
  
  # Remove all children with values outside of matrix dimensions
  children = children[children[,1] > 0,]
  children = children[children[,2] > 0,]
  children = children[children[,1] < xSize+1,]
  children = children[children[,2] < ySize+1,]
  
  return (children)
}


next_package = function(roads, car, packages) {
  x = car$x
  y = car$y
  distance_vector = h_cost(packages[,1], x, 
                           packages[,2], y)
  for (package in 1:5) {
    min_dist = Inf
    next_pickup = 0
    if (packages[package, 5] == 0 &
        distance_vector[package] < min_dist) {
      min_dist = distance_vector[package]
      next_pickup = package
    }
  }
  return (next_pickup)
