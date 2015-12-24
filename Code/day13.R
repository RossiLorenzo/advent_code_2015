# --- Day 13: Knights of the Dinner Table ---
#   
#   In years past, the holiday feast with your family hasn't gone so well. 
# Not everyone gets along! This year, you resolve, will be different. 
# You're going to find the optimal seating arrangement and avoid all
# those awkward conversations.
# 
# You start by writing up a list of everyone invited and the amount 
# their happiness would increase or decrease if they were to find themselves
# sitting next to each other person. You have a circular table that will 
# be just big enough to fit everyone comfortably, and so each person will
# have exactly two neighbors.
# 
# For example, suppose you have only four attendees planned, and you 
# calculate their potential happiness as follows:
#   
# Alice would gain 54 happiness units by sitting next to Bob.
# Alice would lose 79 happiness units by sitting next to Carol.
# Alice would lose 2 happiness units by sitting next to David.
# Bob would gain 83 happiness units by sitting next to Alice.
# Bob would lose 7 happiness units by sitting next to Carol.
# Bob would lose 63 happiness units by sitting next to David.
# Carol would lose 62 happiness units by sitting next to Alice.
# Carol would gain 60 happiness units by sitting next to Bob.
# Carol would gain 55 happiness units by sitting next to David.
# David would gain 46 happiness units by sitting next to Alice.
# David would lose 7 happiness units by sitting next to Bob.
# David would gain 41 happiness units by sitting next to Carol.
# Then, if you seat Alice next to David, Alice would
# lose 2 happiness units (because David talks so much), 
# but David would gain 46 happiness units (because Alice is such a good listener), 
# for a total change of 44.
# 
# If you continue around the table, you could then seat Bob next
# to Alice (Bob gains 83, Alice gains 54). Finally, seat Carol, who sits next to Bob (Carol gains 60, Bob loses 7) and David (Carol gains 55, David gains 41). The arrangement looks like this:
#   
#   +41 +46
# +55   David    -2
# Carol       Alice
# +60    Bob    +54
# -7  +83
# After trying every other seating arrangement in this hypothetical
# scenario, you find that this one is the most optimal, 
# with a total change in happiness of 330.
# 
# What is the total change in happiness for the optimal 
# seating arrangement of the actual guest list?

# Libraries
library(gtools)
library(dplyr)

# Get and reshape data
input = readLines("~/Google Drive/Lorenzo/AdventCalendar/input13.txt")
input_df = data_frame(
  Original = input,
  From = gsub(" .*", "", input),
  To = gsub("\\.", "", gsub(".* ", "", input)),
  Value = as.numeric(gsub("\\D", "", input))) %>% 
  mutate(Value = ifelse(grepl("lose", Original), -Value, Value))

# Find all saeting plans
all_guests = unique(input_df$From)
all_combinations = permutations(length(all_guests), length(all_guests), all_guests)

# For every combination calcualte the happiness
all_happiness = apply(all_combinations, 1, function(x){
  x = c(x, x[1])
  sum(sapply(2:length(x), function(i){
    input_df$Value[input_df$From == x[i - 1] & input_df$To == x[i]] +
      input_df$Value[input_df$From == x[i] & input_df$To == x[i - 1]]
  }))
})
max(all_happiness)

# --- Part Two ---
#   
# In all the commotion, you realize that you forgot to seat yourself. 
# At this point, you're pretty apathetic toward the whole thing, 
# and your happiness wouldn't really go up or down regardless of who
# you sit next to. You assume everyone else would be just as ambivalent
# about sitting next to you, too.
# 
# So, add yourself to the list, and give all happiness relationships 
# that involve you a score of 0.
# 
# What is the total change in happiness for the optimal seating 
# arrangement that actually includes yourself?

new_input_df = input_df %>% 
  rbind(data_frame(Original = "", From = rep("Lorenzo", length(all_guests)), To = all_guests, Value = 0)) %>% 
  rbind(data_frame(Original = "", From = all_guests, To = "Lorenzo", Value = 0))

# Find all saeting plans
all_guests = unique(new_input_df$From)
all_combinations = permutations(length(all_guests), length(all_guests), all_guests)

# For every combination calcualte the happiness
all_happiness = apply(all_combinations, 1, function(x){
  x = c(x, x[1])
  sum(sapply(2:length(x), function(i){
    new_input_df$Value[new_input_df$From == x[i - 1] & new_input_df$To == x[i]] +
      new_input_df$Value[new_input_df$From == x[i] & new_input_df$To == x[i - 1]]
  }))
})
max(all_happiness)