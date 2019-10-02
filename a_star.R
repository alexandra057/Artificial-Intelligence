library(DeliveryMan)

# function for getting the edge-cost of a neighbour node
g_cost = function(roads, x,y, neighbour_x, neighbour_y) {
  if ((x - neighbour_x) == 1) return (roads$hroads[x-1,y])
  if ((x - neighbour_x) == -1) return (roads$hroads[x,y])
  if ((y - neighbour_y) == 1) return (roads$vroads[x,y-1])
  return (roads$vroads[x,y])
}

# calculates the manhattan distance
h_cost = function(x, y, x_next, y_next) {
  return (abs(x-x_next) + abs(y-y_next))
}


# returns the next package to pick up (choosing the nearest package)
next_package = function(roads, car, packages) {
  x = car$x
  y = car$y
  best_distance = Inf
  best_index = 0
  distance_vector = vector()
  for (i in 1:5) {
    if (packages[i,5] != 0) next
    h_cost = h_cost(x,y,packages[i,1],packages[i,2])
    distance_vector = c(distance_vector, h_cost)
    if (h_cost < best_distance){
      best_distance = h_cost
      best_index = i
    }
  }
  return (best_index)
}


movements = c(4,6,8,2)
dx = c(-1,1,0,0)
dy = c(0,0,1,-1)


# gives the index of a node in arrays xs and ys
get_next_index = function(x_next,y_next,xs,ys) {
  for (i in 1:length(xs)) {
    if (xs[i] == x_next & ys[i] == y_next)
      return (i)
  }
  return (0)
}

next_move=function(roads,x_dest,y_dest,xs,ys,costs,used,directions) {
  while (TRUE) {
    current_idx = 0
    best_f = Inf
    
    # find best node to expand (PQ workaround)
    for (i in 1:length(xs)) {
      if (used[i]) next
      # we are at finish, return
      if (xs[i] == x_dest & ys[i] == y_dest) {
        return (directions[i])
      }
      
      f = h_cost(xs[i],ys[i],x_dest,y_dest) + costs[i]
      if (f<best_f) {
        best_f = f
        current_idx = i
      }
    }
    
    used[current_idx] = TRUE
    for (i in 1:4) {
      x_next= xs[current_idx] + dx[i]
      y_next= ys[current_idx] + dy[i]
      
      # checking if the next node has invalid coordinates
      if (x_next>10 | x_next<1 | y_next<1 | y_next>10) next
      
      next_idx = get_next_index(x_next,y_next,xs,ys)
      
      # add node to front if we didn't find it (i.e. if node is not in the arrays xs and ys)
      if (next_idx == 0) {
        xs = c(x_next,xs)
        ys = c(y_next,ys)
        used = c(FALSE, used)
        costs = c(Inf, costs)
        directions = c(5, directions)
        next_idx = 1
        # since we append the node to front, our previous index needs to be updated
        current_idx = current_idx + 1
      }
      
      # calculate the f-cost for the next node
      next_cost = costs[current_idx] + g_cost(roads, xs[current_idx], ys[current_idx], x_next, y_next)
      if (next_cost >= costs[next_idx]) next
      
      costs[next_idx] = next_cost
      directions[next_idx] = directions[current_idx]
      
      # if current_idx is a start node, we want to record our first move.
      if (directions[next_idx] != 5) next
      directions[next_idx] = movements[i]
    }
  }
}


# a_star_path = vector()


a_star=function(roads,car,packages){
  #car$nextMove=sample(c(2,4,6,8),1)
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=next_package(roads,car,packages)
  } else {
    toGo=car$load
    offset=2
  }
  # our goal coordinates (depending on load value)
  x_dest = packages[toGo,1+offset]
  y_dest = packages[toGo,2+offset]
  x=car$x
  y=car$y
  car$nextMove=next_move(roads,x_dest,y_dest, c(x), c(y), c(0), c(FALSE), c(5))
  # a_star_path <<- c(a_star_path, car$nextMove)
  car$mem=list()
  return (car)
}

#runDeliveryMan(a_star, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T)

testDM(a_star, verbose = 1, returnVec = FALSE, n = 500, seed = 21, timeLimit = 250)
