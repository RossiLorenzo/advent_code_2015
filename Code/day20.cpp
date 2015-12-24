#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector all_divisor(int num) {
  int square_root = (int) sqrt(num) + 1;
  NumericVector tmp_res(2 * square_root);
  int mycount = 0;
  for (int i = 1; i < square_root; i++){ 
    if (num % i == 0){
      tmp_res[mycount] = i;
      tmp_res[mycount + 1] = num/i;
      mycount = mycount + 2;
    }
  }
  // Resized vector
  std::sort( tmp_res.begin(), tmp_res.end() );
  tmp_res.erase( std::unique( tmp_res.begin(), tmp_res.end() ), tmp_res.end() );
  tmp_res.erase(0);
  return(tmp_res);
}

// [[Rcpp::export]]
int elves(int to_reach){
  int presents = 0;
  int house = 0;
  while(presents < to_reach){
    house++;
    NumericVector all_elves = all_divisor(house);
    presents = 0;
    for(int i = 0; i < all_elves.length(); i++)
      presents = presents + 10 * all_elves[i];
  }
  return(house);
}

// [[Rcpp::export]]
int new_elves(int to_reach){
  int presents = 0;
  int house = 0;
  while(presents < to_reach){
    house++;
    NumericVector all_elves = all_divisor(house);
    presents = 0;
    for(int i = 0; i < all_elves.length(); i++){
      if(house <= all_elves[i] * 50)
        presents = presents + 11 * all_elves[i];
    }
  }
  return(house);
}

/*** R
# --- Day 20: Infinite Elves and Infinite Houses ---
#   
# To keep the Elves busy, Santa has them deliver some presents by hand, door-to-door. 
# He sends them down a street with infinite houses numbered sequentially: 1, 2, 3, 4, 5, and so on.
# 
# Each Elf is assigned a number, too, and delivers presents to houses based on that number:
#   
# The first Elf (number 1) delivers presents to every house: 1, 2, 3, 4, 5, ....
# The second Elf (number 2) delivers presents to every second house: 2, 4, 6, 8, 10, ....
# Elf number 3 delivers presents to every third house: 3, 6, 9, 12, 15, ....
# There are infinitely many Elves, numbered starting with 1. Each Elf delivers presents 
# equal to ten times his or her number at each house.
# 
# So, the first nine houses on the street end up like this:
#   
# House 1 got 10 presents.
# House 2 got 30 presents.
# House 3 got 40 presents.
# House 4 got 70 presents.
# House 5 got 60 presents.
# House 6 got 120 presents.
# House 7 got 80 presents.
# House 8 got 150 presents.
# House 9 got 130 presents.
# The first house gets 10 presents: it is visited only by Elf 1, which delivers 1 * 10 = 10 presents. 
# The fourth house gets 70 presents, because it is visited by Elves 1, 2, and 4, for a total of 
# 10 + 20 + 40 = 70 presents.
# 
# What is the lowest house number of the house to get at least as many presents as the number 
# in your puzzle input?
# 
# Your puzzle input is 36,000,000.
elves(36000000)

# --- Part Two ---
#   
# The Elves decide they don't want to visit an infinite number of houses. 
# Instead, each Elf will stop after delivering presents to 50 houses. 
# To make up for it, they decide to deliver presents equal to eleven times their number at each house.
#   
# With these changes, what is the new lowest house number of the house to get at 
# least as many presents as the number in your puzzle input?
new_elves(36000000)
*/
