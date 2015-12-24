# --- Day 18: Like a GIF For Your Yard ---
#   
# After the million lights incident, the fire code has gotten stricter: 
# now, at most ten thousand lights are allowed. You arrange them in 
# a 100x100 grid.
# 
# Never one to let you down, Santa again mails you instructions on the 
# ideal lighting configuration. With so few lights, he says, 
# you'll have to resort to animation.
# 
# Start by setting your lights to the included initial configuration 
# (your puzzle input). A # means "on", and a . means "off".
# 
# Then, animate your grid in steps, where each step decides the 
# next configuration based on the current one. 
# Each light's next state (either on or off) depends on its current 
# state and the current states of the eight lights adjacent to it 
# (including diagonals). Lights on the edge of the grid might have 
# fewer than eight neighbors; the missing ones always count as "off".
# 
# For example, in a simplified 6x6 grid, the light marked A has 
# the neighbors numbered 1 through 8, and the light marked B, 
# which is on an edge, only has the neighbors marked 1 through 5:
#   
# 1B5...
# 234...
# ......
# ..123.
# ..8A4.
# ..765.
# The state a light should have next is based on its current state 
# (on or off) plus the number of neighbors that are on:
#   
# A light which is on stays on when 2 or 3 neighbors are on, 
# and turns off otherwise.
# A light which is off turns on if exactly 3 neighbors are on, 
# and stays off otherwise.
# All of the lights update simultaneously; they all consider the 
# same current state before moving to the next.
# 
# In your grid of 100x100 lights, given your initial configuration, 
# how many lights are on after 100 steps?

# Read data
input = readLines("~/Google Drive/Lorenzo/AdventCalendar/input18.txt")
input = do.call(rbind, strsplit(input, "", TRUE))

# Find neighbors
find_neighbors = function(df, i, j){
  x = matrix(c(
    c(i-1, i-1, i-1, i, i, i+1, i+1, i+1),
    c(j-1, j, j+1, j-1, j+1, j-1, j, j+1)), ncol = 2)
  x = x[x[,1] >= 1 & x[,1] <= 100 & x[,2] >= 1 & x[,2] <= 100,]
  apply(x, 1, function(vec){
    df[vec[1], vec[2]]
  })
}

# Update status
update_status = function(df, i, j){
  neigh = find_neighbors(df, i, j)
  current_status = df[i, j]
  if(current_status == "." & sum(neigh == "#") == 3)
    return("#")
  if(current_status == "#" & sum(neigh == "#") %in% c(2,3))
    return("#")
  return(".")
}

# For every combination update the grid
one_update = function(df){
  do.call(rbind, lapply(1:100, function(i, df){
    do.call(cbind, lapply(1:100, function(j, i, df){
      update_status(df, i, j)
    }, i, df))
  }, df))
}

# Update 100 times
df = input
for(k in 1:100)
  df = one_update(df)
sum(df == "#")

# --- Part Two ---
#   
# You flip the instructions over; Santa goes on to point out that this is all just an 
# implementation of Conway's Game of Life. At least, it was, until you notice that 
# something's wrong with the grid of lights you bought: four lights, one in each corner, 
# are stuck on and can't be turned off. The example above will actually run like this:
# 
# In your grid of 100x100 lights, given your initial configuration, but with the four 
# corners always in the on state, how many lights are on after 100 steps?

# Update status
update_status = function(df, i, j){
  if(i == 1 & (j == 1 | j == 100) | i == 100 & (j == 1 | j == 100))
    return("#")
  neigh = find_neighbors(df, i, j)
  current_status = df[i, j]
  if(current_status == "." & sum(neigh == "#") == 3)
    return("#")
  if(current_status == "#" & sum(neigh == "#") %in% c(2,3))
    return("#")
  return(".")
}

# Update 100 times
df = input
df[1,1] = df[1,100] = df[100,1] = df[100,100] = "#"
for(k in 1:100)
  df = one_update(df)
sum(df == "#")
