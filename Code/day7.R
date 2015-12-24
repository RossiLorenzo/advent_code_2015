# This year, Santa brought little Bobby Tables a set of wires 
# and bitwise logic gates! Unfortunately, little Bobby is a 
# little under the recommended age range, and he needs help 
# assembling the circuit.
# 
# Each wire has an identifier (some lowercase letters) and 
# can carry a 16-bit signal (a number from 0 to 65535). 
# A signal is provided to each wire by a gate, another wire, 
# or some specific value. Each wire can only get a signal from
# one source, but can provide its signal to multiple 
# destinations. A gate provides no signal until all of its 
# inputs have a signal.
# 
# The included instructions booklet describe how to connect 
# the parts together: x AND y -> z means to connect wires x 
# and y to an AND gate, and then connect its output to wire z.
# 
# For example:
#   
# 123 -> x means that the signal 123 is provided to wire x.
# x AND y -> z means that the bitwise AND of wire x and wire y
# is provided to wire z.
# p LSHIFT 2 -> q means that the value from wire p is 
# left-shifted by 2 and then provided to wire q.
# NOT e -> f means that the bitwise complement of the value 
# from wire e is provided to wire f.
# Other possible gates include OR (bitwise OR) and 
# RSHIFT (right-shift). 
# If, for some reason, you'd like to emulate the circuit 
# instead, almost all programming languages 
# (for example, C, JavaScript, or Python) provide 
# operators for these gates.
# 
# For example, here is a simple circuit:
# 
# 123 -> x
# 456 -> y
# x AND y -> d
# x OR y -> e
# x LSHIFT 2 -> f
# y RSHIFT 2 -> g
# NOT x -> h
# NOT y -> i
# After it is run, these are the signals on the wires:
# 
# d: 72
# e: 507
# f: 492
# g: 114
# h: 65412
# i: 65079
# x: 123
# y: 456
# In little Bobby's kit's instructions booklet 
# (provided as your puzzle input), what signal is 
# ultimately provided to wire a?

# Libraries
library(dplyr)

# Read and format data
bitops = readLines("~/Google Drive/Lorenzo/AdventCalendar/input7.txt")
# bitops = c("NOT y -> i", "NOT x -> h", "y RSHIFT 2 -> g", "123 -> x", "456 -> y")
tokens = strsplit(gsub(" -> ", " ", bitops), " ", fixed = TRUE)

# Do operations
bitops = rbind_all(lapply(tokens, function(i){
  # Equal
  if(length(i) == 2)
    return(data_frame(
      target = i[2], 
      result = "value1",
      value1 = i[1],
      value2 = NA,
      dependency1 = !is.na(as.numeric(i[1])),
      dependency2 = TRUE))
  # Not
  if(length(i) == 3)
    return(data_frame(
      target = i[3], 
      result = paste0("bitwNot(value1)"),
      value1 = i[2],
      value2 = NA,
      dependency1 = !is.na(as.numeric(i[2])),
      dependency2 = TRUE))
  # Or
  if(i[2] == "OR")
    return(data_frame(
      target = i[4],
      result = paste0("bitwOr(value1, value2)"),
      value1 = i[1],
      value2 = i[3],
      dependency1 = !is.na(as.numeric(i[1])), 
      dependency2 = !is.na(as.numeric(i[3]))))
  # And
  if(i[2] == "AND")
    return(data_frame(
      target = i[4],
      result = paste0("bitwAnd(value1, value2)"),
      value1 = i[1],
      value2 = i[3],
      dependency1 = !is.na(as.numeric(i[1])), 
      dependency2 = !is.na(as.numeric(i[3]))))
  # Shifts
  if(i[2] == "LSHIFT")
    return(data_frame(
      target = i[4],
      result = paste0("bitwShiftL(value1, value2)"),
      value1 = i[1],
      value2 = i[3],
      dependency1 = !is.na(as.numeric(i[1])), 
      dependency2 = TRUE))
  if(i[2] == "RSHIFT")
    return(data_frame(
      target = i[4],
      result = paste0("bitwShiftR(value1, value2)"),
      value1 = i[1],
      value2 = i[3],
      dependency1 = !is.na(as.numeric(i[1])), 
      dependency2 = TRUE))
}))

# Created a dataset containing the evaluated nodes
evaluated = filter(bitops, dependency1, dependency2) %>% 
  transmute(target, result = value1)
to_evaluate = filter(bitops, !dependency1 | !dependency2)

while(nrow(to_evaluate) > 0){
  print(nrow(to_evaluate))
  # Update dependencies
  to_evaluate = to_evaluate %>% 
    group_by(value1) %>% 
    mutate(dependency1 = any(value1 %in% evaluated$target, dependency1)) %>% 
    ungroup() %>% 
    group_by(value2) %>% 
    mutate(dependency2 = any(value2 %in% evaluated$target, dependency2)) %>% 
    ungroup()
  # Find met dependecies
  met_dep = filter(to_evaluate, dependency1, dependency2) %>%
    left_join(evaluated, by = c("value1" = "target")) %>% 
    left_join(evaluated, by = c("value2" = "target")) %>% 
    mutate(value1 = ifelse(is.na(`result.y`), value1, `result.y`),
           value2 = ifelse(is.na(`result`), value2, `result`)) %>% 
    group_by(value1, value2, result.x) %>% 
    mutate(evaluate_me = gsub("value1", unique(value1), `result.x`),
           evaluate_me = ifelse(is.na(unique(value2)), evaluate_me, gsub("value2", unique(value2), evaluate_me))) %>%
    ungroup() %>% 
    group_by(target) %>% 
    transmute(result = eval(parse(text = evaluate_me)))
  # Update evaluated and to_evaluate
  evaluated = rbind(evaluated, met_dep)
  to_evaluate = filter(to_evaluate, !dependency1 | !dependency2)
}
evaluated$result[evaluated$target == "lx"]
# Now, take the signal you got on wire a, override wire 
# b to that signal, and reset the other wires 
# (including wire a). What new signal is ultimately 
# provided to wire a?

bitops$value1[bitops$target == "b"] = 46065
