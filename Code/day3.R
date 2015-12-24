# --- Day 3: Perfectly Spherical Houses in a Vacuum ---
#   
# Santa is delivering presents to an infinite two-dimensional 
# grid of houses.
# 
# He begins by delivering a present to the house at his starting 
# location, and then an elf at the North Pole calls him via radio 
# and tells him where to move next. 
# Moves are always exactly one house to the north (^), 
# south (v), east (>), or west (<). 
# After each move, he delivers another present to the house at his 
# new location.
# 
# However, the elf back at the north pole has had a little too much 
# eggnog, and so his directions are a little off, 
# and Santa ends up visiting some houses more than once. 
# How many houses receive at least one present?
# 
# For example:
#   
# > delivers presents to 2 houses: one at the starting location, 
# and one to the east.
# ^>v< delivers presents to 4 houses in a square, including twice 
# to the house at his starting/ending location.
# ^v^v^v^v^v delivers a bunch of presents to some very lucky 
# children at only 2 houses.

library(dplyr)
input = readLines("~/Google Drive/Lorenzo/AdventCalendar/input3.txt")
input = strsplit(input, "", fixed = TRUE)[[1]]

move_santa = function(x, y, moves){
  if(length(moves) == 0)
    return(data.frame(x, y))
  if(moves[1] == "^")
    return(rbind(
      data.frame(x, y),
      move_santa(x, y + 1, moves[-1])
    ))
  if(moves[1] == "v")
    return(rbind(
      data.frame(x, y),
      move_santa(x, y - 1, moves[-1])
    ))
  if(moves[1] == ">")
    return(rbind(
      data.frame(x, y),
      move_santa(x + 1, y, moves[-1])
    ))
  if(moves[1] == "<")
    return(rbind(
      data.frame(x, y),
      move_santa(x - 1, y, moves[-1])
    ))
}

all_moves = data.frame(x = 0, y = 0)
x = 0
y = 0
for(i in seq(0, nrow(all_moves), 500)){
  all_moves = rbind(all_moves, 
    move_santa(x, y, input[(i + 1):min(c(i + 500, length(input)))])[-1, ])
  x = all_moves$x[nrow(all_moves)]
  y = all_moves$y[nrow(all_moves)]
}

all_moves %>% 
  select(x, y) %>% 
  distinct() %>% 
  nrow()

# --- Part Two ---
#   
# The next year, to speed up the process, 
# Santa creates a robot version of himself, Robo-Santa, 
# to deliver presents with him.
# 
# Santa and Robo-Santa start at the same location 
# (delivering two presents to the same starting house), 
# then take turns moving based on instructions from the elf, 
# who is eggnoggedly reading from the same script as the previous year.
# 
# This year, how many houses receive at least one present?
# 
# For example:
#   
# ^v delivers presents to 3 houses, because Santa goes north, 
# and then Robo-Santa goes south.
# ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa 
# end up back where they started.
# ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one 
# direction and Robo-Santa going the other.

s_input = input[seq(1,nrow(all_moves),2)]
r_input = input[seq(2,nrow(all_moves),2)]

all_moves_s = data.frame(x = 0, y = 0)
x = 0
y = 0
for(i in seq(0, length(s_input), 500)){
  all_moves_s = rbind(all_moves_s, 
                    move_santa(x, y, s_input[(i + 1):min(c(i + 500, length(s_input) - 1))])[-1, ])
  x = all_moves_s$x[nrow(all_moves_s)]
  y = all_moves_s$y[nrow(all_moves_s)]
}
all_moves_r = data.frame(x = 0, y = 0)
x = 0
y = 0
for(i in seq(0, length(r_input), 500)){
  all_moves_r = rbind(all_moves_r, 
                      move_santa(x, y, r_input[(i + 1):min(c(i + 500, length(r_input) - 1))])[-1, ])
  x = all_moves_r$x[nrow(all_moves_r)]
  y = all_moves_r$y[nrow(all_moves_r)]
}

all_moves_r %>% 
  rbind(all_moves_s) %>% 
  select(x, y) %>% 
  distinct() %>% 
  nrow()