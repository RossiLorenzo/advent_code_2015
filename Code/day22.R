# --- Day 22: Wizard Simulator 20XX ---
#   
# Little Henry Case decides that defeating bosses with swords and stuff is boring. 
# Now he's playing the game with a wizard. Of course, he gets stuck on another boss and needs 
# your help again.
# 
# In this version, combat still proceeds with the player and the boss taking alternating turns. 
# The player still goes first. Now, however, you don't get any equipment; instead, you must choose 
# one of your spells to cast. The first character at or below 0 hit points loses.
# 
# Since you're a wizard, you don't get to wear armor, and you can't attack normally. 
# However, since you do magic damage, your opponent's armor is ignored, and so the boss effectively 
# has zero armor as well. As before, if armor (from a spell, in this case) would reduce damage 
# below 1, it becomes 1 instead - that is, the boss' attacks always deal at least 1 damage.
# 
# On each of your turns, you must select one of your spells to cast. If you cannot afford to 
# cast any spell, you lose. Spells cost mana; you start with 500 mana, but have no maximum limit. 
# You must have enough mana to cast a spell, and its cost is immediately deducted when you cast it. 
# Your spells are Magic Missile, Drain, Shield, Poison, and Recharge.
# 
# Magic Missile costs 53 mana. It instantly does 4 damage.
# Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
# Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, 
# your armor is increased by 7.
# Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn 
# while it is active, it deals the boss 3 damage.
# Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn 
# while it is active, it gives you 101 new mana.
# 
# Effects all work the same way. Effects apply at the start of both the player's turns and the boss' 
# turns. Effects are created with a timer (the number of turns they last); at the start of each turn, 
# after they apply any effect they have, their timer is decreased by one. 
# If this decreases the timer to zero, the effect ends. You cannot cast a spell that would start 
# an effect which is already active. However, effects can be started on the same turn they end.
# 
# For example, suppose the player has 10 hit points and 250 mana, and that the boss has 13 
# hit points and 8 damage:
# 
# -- Player turn --
# - Player has 10 hit points, 0 armor, 250 mana
# - Boss has 13 hit points
# Player casts Poison.
# 
# -- Boss turn --
# - Player has 10 hit points, 0 armor, 77 mana
# - Boss has 13 hit points
# Poison deals 3 damage; its timer is now 5.
# Boss attacks for 8 damage.
# 
# -- Player turn --
# - Player has 2 hit points, 0 armor, 77 mana
# - Boss has 10 hit points
# Poison deals 3 damage; its timer is now 4.
# Player casts Magic Missile, dealing 4 damage.
# 
# -- Boss turn --
# - Player has 2 hit points, 0 armor, 24 mana
# - Boss has 3 hit points
# Poison deals 3 damage. This kills the boss, and the player wins.
# Now, suppose the same initial conditions, except that the boss has 14 hit points instead:
# 
# -- Player turn --
# - Player has 10 hit points, 0 armor, 250 mana
# - Boss has 14 hit points
# Player casts Recharge.
# 
# -- Boss turn --
# - Player has 10 hit points, 0 armor, 21 mana
# - Boss has 14 hit points
# Recharge provides 101 mana; its timer is now 4.
# Boss attacks for 8 damage!
# 
# -- Player turn --
# - Player has 2 hit points, 0 armor, 122 mana
# - Boss has 14 hit points
# Recharge provides 101 mana; its timer is now 3.
# Player casts Shield, increasing armor by 7.
# 
# -- Boss turn --
# - Player has 2 hit points, 7 armor, 110 mana
# - Boss has 14 hit points
# Shield's timer is now 5.
# Recharge provides 101 mana; its timer is now 2.
# Boss attacks for 8 - 7 = 1 damage!
#   
# -- Player turn --
# - Player has 1 hit point, 7 armor, 211 mana
# - Boss has 14 hit points
# Shield's timer is now 4.
# Recharge provides 101 mana; its timer is now 1.
# Player casts Drain, dealing 2 damage, and healing 2 hit points.
# 
# -- Boss turn --
# - Player has 3 hit points, 7 armor, 239 mana
# - Boss has 12 hit points
# Shield's timer is now 3.
# Recharge provides 101 mana; its timer is now 0.
# Recharge wears off.
# Boss attacks for 8 - 7 = 1 damage!
#   
# -- Player turn --
# - Player has 2 hit points, 7 armor, 340 mana
# - Boss has 12 hit points
# Shield's timer is now 2.
# Player casts Poison.
# 
# -- Boss turn --
# - Player has 2 hit points, 7 armor, 167 mana
# - Boss has 12 hit points
# Shield's timer is now 1.
# Poison deals 3 damage; its timer is now 5.
# Boss attacks for 8 - 7 = 1 damage!
#   
# -- Player turn --
# - Player has 1 hit point, 7 armor, 167 mana
# - Boss has 9 hit points
# Shield's timer is now 0.
# Shield wears off, decreasing armor by 7.
# Poison deals 3 damage; its timer is now 4.
# Player casts Magic Missile, dealing 4 damage.
# 
# -- Boss turn --
# - Player has 1 hit point, 0 armor, 114 mana
# - Boss has 2 hit points
# Poison deals 3 damage. This kills the boss, and the player wins.
# 
# You start with 50 hit points and 500 mana points. The boss's actual stats are in your puzzle 
# input. What is the least amount of mana you can spend and still win the fight? 
# (Do not include mana recharge effects as "spending" negative mana.)

library(dplyr)
library(stringr)

input = readLines("~/Google Drive/Lorenzo/AdventCalendar/input22.txt")
input = as.numeric(unlist(str_extract_all(input, "[0-9]+")))

simulate_turn = function(card, env_list, hard){
  if(hard)
    env_list$my_hit = env_list$my_hit - 1
  # Check if I have poison or recharge active
  if(env_list$poison_turns > 0){
    env_list$boss_hit = env_list$boss_hit - 3
    env_list$poison_turns = env_list$poison_turns - 1
  }
  if(env_list$recharge_turns > 0){
    env_list$mana = env_list$mana + 101
    env_list$recharge_turns = env_list$recharge_turns - 1
  }
  if(env_list$shield_turns > 0)
    env_list$shield_turns = env_list$shield_turns - 1
  # Simulate me playing the card
  if(card == "missile"){
    env_list$cost = env_list$cost + 53
    env_list$mana = env_list$mana - 53
    env_list$boss_hit = env_list$boss_hit - 4
  }
  if(card == "drain"){
    env_list$cost = env_list$cost + 73
    env_list$mana = env_list$mana - 73
    env_list$boss_hit = env_list$boss_hit - 2
    env_list$my_hit = env_list$my_hit + 2
  }
  if(card == "shield"){
    env_list$cost = env_list$cost + 113
    env_list$mana = env_list$mana - 113
    env_list$shield_turns = 6
  }
  if(card == "poison"){
    env_list$cost = env_list$cost + 173
    env_list$mana = env_list$mana - 173
    env_list$poison_turns = 6
  }
  if(card == "recharge"){
    env_list$cost = env_list$cost + 229
    env_list$mana = env_list$mana - 229
    env_list$recharge_turns = 5
  }
  # Simulate the boss turn
  if(env_list$shield_turns > 0){
    env_list$my_hit = env_list$my_hit - max(1, env_list$boss_damage - 7)
    env_list$shield_turns = env_list$shield_turns - 1
  }else{
    env_list$my_hit = env_list$my_hit - env_list$boss_damage
  }
  # Check all the active spells
  if(env_list$poison_turns > 0){
    env_list$boss_hit = env_list$boss_hit - 3
    env_list$poison_turns = env_list$poison_turns - 1
  }
  if(env_list$recharge_turns > 0){
    env_list$mana = env_list$mana + 101
    env_list$recharge_turns = env_list$recharge_turns - 1
  }
  return(env_list)
}

simulate_game = function(starting_list, hard){
  # Check if game has finished
  if(starting_list$mana < 0)
    return(99999)
  if(starting_list$boss_hit <= 0)
    return(starting_list$cost)
  if(starting_list$my_hit <= 0)
    return(99999)
  # Check which move can be next
  available_actions = c("missile", "drain", "shield", "poison", "recharge")
  # available_actions = c("missile", "poison")
  if(starting_list$shield_turns > 1)
    available_actions = setdiff(available_actions, "shield")
  if(starting_list$poison_turns > 1)
    available_actions = setdiff(available_actions, "poison")
  if(starting_list$recharge_turns > 1)
    available_actions = setdiff(available_actions, "recharge")
  # For every action simulate what happens
  res = unlist(lapply(available_actions, function(i){
    starting_list = simulate_turn(i, starting_list, hard)
    simulate_game(starting_list, hard)
  }))
  return(res)
}

starting_list = list(cost = 0, mana = 500, my_hit = 50, boss_hit = input[1], boss_damage = input[2], 
                     shield_turns = 0, poison_turns = 0, recharge_turns = 0)
x = simulate_game(starting_list, FALSE)
min(x)

# --- Part Two ---
#   
# On the next run through the game, you increase the difficulty to hard.
# 
# At the start of each player turn (before any other effects apply), you lose 1 hit point. 
# If this brings you to or below 0 hit points, you lose.
# 
# With the same starting stats for you and the boss, what is the least amount of mana you can 
# spend and still win the fight?

x = simulate_game(starting_list, hard = TRUE)
min(x)
