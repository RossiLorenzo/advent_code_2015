# Every year, Santa manages to deliver all of his presents 
# in a single night.
# 
# This year, however, he has some new locations to visit; 
# his elves have provided him the distances between every 
# pair of locations. He can start and end at any two 
# (different) locations he wants, but he must visit each 
# location exactly once. What is the shortest distance he
# can travel to achieve this?
# 
# For example, given the following distances:
#   
# London to Dublin = 464
# London to Belfast = 518
# Dublin to Belfast = 141
# The possible routes are therefore:
#   
# Dublin -> London -> Belfast = 982
# London -> Dublin -> Belfast = 605
# London -> Belfast -> Dublin = 659
# Dublin -> Belfast -> London = 659
# Belfast -> Dublin -> London = 605
# Belfast -> London -> Dublin = 982
# The shortest of these is London -> Dublin -> Belfast = 605, 
# and so the answer is 605 in this example.
# 
# What is the distance of the shortest route?

library(gtools)

paths = readLines("~/Google Drive/Lorenzo/AdventCalendar/input9.txt")
paths = data.frame(From = gsub(" .*", "", paths),
                   To = gsub(" .*", "", gsub(".*to ", "", paths)),
                   Distance = as.numeric(gsub(".* ", "", paths)),
                   stringsAsFactors = FALSE)
paths_switch = paths
paths_switch$From = paths$To
paths_switch$To = paths$From
paths = rbind(paths, paths_switch)

all_places = unique(paths$From)
all_perms = permutations(length(all_places), length(all_places), all_places)

distance = apply(all_perms, 1, function(x){
  tmp = 0
  for(i in 2:length(x))
    tmp = tmp + paths$Distance[paths$From == x[i-1] & paths$To == x[i]]
  return(tmp)
})

# --- Part Two ---
#   
# The next year, just to show off, Santa decides to take 
# the route with the longest distance instead.
# 
# He can still start and end at any two (different) 
# locations he wants, and he still must visit each location 
# exactly once.
# 
# What is the distance of the longest route?

max(distance)
