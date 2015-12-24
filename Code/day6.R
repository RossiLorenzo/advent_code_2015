# --- Day 6: Probably a Fire Hazard ---
#   
# Because your neighbors keep defeating you in the holiday house 
# decorating contest year after year, 
# you've decided to deploy one million lights in a 1000x1000 grid.
# 
# Furthermore, because you've been especially nice this year, 
# Santa has mailed you instructions on how to display the ideal lighting 
# configuration.
# 
# Lights in your grid are numbered from 0 to 999 in each direction; 
# the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. 
# The instructions include whether to turn on, turn off, or toggle various 
# inclusive ranges given as coordinate pairs. 
# Each coordinate pair represents opposite corners of a rectangle, 
# inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 
# lights in a 3x3 square. The lights all start turned off.
# 
# To defeat your neighbors this year, all you have to do is set up your 
# lights by doing the instructions Santa sent you in order.
# 
# For example:
#   
# turn on 0,0 through 999,999 would turn on (or leave on) every light.
# toggle 0,0 through 999,0 would toggle the first line of 1000 lights, 
# turning off the ones that were on, and turning on the ones that were off.
# turn off 499,499 through 500,500 would turn off (or leave off) the middle 
# four lights.
# After following the instructions, how many lights are lit?

library(dplyr)
library(stringr)

# Read instructions and format them
instructions = readLines("~/Google Drive/Lorenzo/AdventCalendar/input6.txt")
instructions = data_frame(original = instructions) %>% 
  mutate(
    type = ifelse(grepl("turn off", original), "off", "unk"),
    type = ifelse(grepl("turn on", original), "on", type),
    type = ifelse(grepl("toggle", original), "toggle", type),
    numbers = str_extract_all(instructions, "[0-9]+"))
# Set the lights grid to 0
lights_grid = matrix(0, nrow = 1000, ncol = 1000)
# Function to execute command
execute_command = function(type, x, y, grid){
  x = as.numeric(x) + 1
  x = seq(x[1], x[2], 1)
  y = as.numeric(y) + 1
  y = seq(y[1], y[2], 1)
  if(type == "on")
    grid[x, y] = 1
  if(type == "off")
    grid[x, y] = 0
  if(type == "toggle")
    grid[x, y] = abs(1 - grid[x, y])
  return(grid)
}
# Execute the instructions
for(i in 1:nrow(instructions)){
  x = instructions$numbers[[i]][c(1,3)]
  y = instructions$numbers[[i]][c(2,4)]
  lights_grid = execute_command(instructions$type[i], x, y, lights_grid)
}
sum(lights_grid)

# --- Part Two ---
#   
# You just finish implementing your winning light pattern when you 
# realize you mistranslated Santa's message from Ancient Nordic Elvish.
# 
# The light grid you bought actually has individual brightness controls; 
# each light can have a brightness of zero or more. The lights all start at zero.
# 
# The phrase turn on actually means that you should increase 
# the brightness of those lights by 1.
# 
# The phrase turn off actually means that you should decrease the brightness 
# of those lights by 1, to a minimum of zero.
# 
# The phrase toggle actually means that you should increase the brightness 
# of those lights by 2.
# 
# What is the total brightness of all lights combined after following 
# Santa's instructions?
# 
# For example:
#   
# turn on 0,0 through 0,0 would increase the total brightness by 1.
# toggle 0,0 through 999,999 would increase the total brightness by 2000000.

# Set the lights grid to 0
lights_grid_new = matrix(0, nrow = 1000, ncol = 1000)
# Function to execute command
execute_command_new = function(type, x, y, grid){
  x = as.numeric(x) + 1
  x = seq(x[1], x[2], 1)
  y = as.numeric(y) + 1
  y = seq(y[1], y[2], 1)
  if(type == "on")
    grid[x, y] = grid[x, y] + 1
  if(type == "off"){
    grid[x, y] = grid[x, y] - 1
    grid[grid == -1] = 0
  }
  if(type == "toggle")
    grid[x, y] = grid[x, y] + 2
  return(grid)
}
# Execute the instructions
for(i in 1:nrow(instructions)){
  x = instructions$numbers[[i]][c(1,3)]
  y = instructions$numbers[[i]][c(2,4)]
  lights_grid_new = execute_command_new(instructions$type[i], x, y, lights_grid_new)
}
sum(lights_grid_new)
