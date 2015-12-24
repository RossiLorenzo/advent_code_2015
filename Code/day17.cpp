#include <Rcpp.h>
#include <iostream>
#include <string>
using namespace Rcpp;

// Function to remove from a vector all the elements before the index
NumericVector remove_el_before(NumericVector myarray, int index){
  NumericVector newarray(myarray.length() - index - 1);
  for(int i = 0; i < newarray.length(); i++)
    newarray[i] = myarray[i + index + 1];
  return(newarray);
}

// [[Rcpp::export]]
int left_eggnog(int liters, NumericVector containers, int used, int constraint) {
  // First check to see if I'm already out of liters
  if(liters < 0)
    return(0);
  // Check the constraint
  if(used > constraint)
    return(0);
  // Chcek if I actually have it right
  if(liters == 0)
    return(1);
  // Check if I finished containers
  if(containers.length() == 0)
    return(0);
  int res = 0;
  // In case I have still space try all containers left
  for(int i = 0; i < containers.length(); i++)
    res += left_eggnog(liters - containers[i], remove_el_before(containers, i), used + 1, constraint);
  return(res);
}

// [[Rcpp::export]]
int min_containers(int liters, NumericVector containers, int used) {
  // First check to see if I'm already out of liters
  if(liters < 0)
    return(999);
  // Chcek if I actually have it right
  if(liters == 0)
    return(used);
  // Check if I finished containers
  if(containers.length() == 0)
    return(999);
  int res = 999;
  // In case I have still space try all containers left
  for(int i = 0; i < containers.length(); i++){
    int tmp_res = min_containers(liters - containers[i], remove_el_before(containers, i), used + 1);
    if(tmp_res < res)
      res = tmp_res;
  }
  return(res);
}

/*** R
# --- Day 17: No Such Thing as Too Much ---
#   
# The elves bought too much eggnog again - 150 liters this time. 
# To fit it all into your refrigerator, you'll need to move it 
# into smaller containers. You take an inventory of the capacities 
# of the available containers.
# 
# For example, suppose you have containers of size 20, 15, 10, 5, 
# and 5 liters. If you need to store 25 liters, there are four ways
# to do it:
# 15 and 10
# 20 and 5 (the first 5)
# 20 and 5 (the second 5)
# 15, 5, and 5
# 
# Filling all containers entirely, how many different combinations 
# of containers can exactly fit all 150 liters of eggnog?

# Read the input 
input = readLines("~/Google Drive/Lorenzo/AdventCalendar/input17.txt")
input = as.numeric(input)

# Call the C++ with 150 as target
paste("Result of part 1 is:", left_eggnog(150, input, 0, 999))

# --- Part Two ---
#   
# While playing with all the containers in the kitchen, another load of eggnog arrives! 
# The shipping and receiving department is requesting as many containers as you can spare.
#   
# Find the minimum number of containers that can exactly fit all 150 liters of eggnog. 
# How many different ways can you fill that number of containers and still hold exactly 150 litres?
#   
# In the example above, the minimum number of containers was two. There were three ways to 
# use that many containers, and so the answer there would be 3.
mc = min_containers(150, input, 0)
paste("Result of part 2 is:", left_eggnog(150, input, 0, mc), "with", mc, "containers")
*/