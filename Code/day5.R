# --- Day 5: Doesn't He Have Intern-Elves For This? ---
# 
# Santa needs help figuring out which strings in his text file are naughty or nice.
# 
# A nice string is one with all of the following properties:
# 
# It contains at least three vowels (aeiou only), like aei, xazegov, 
# or aeiouaeiouaeiou.
# It contains at least one letter that appears twice in a row, 
# like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
# It does not contain the strings ab, cd, pq, or xy, even if they are part of one 
# of the other requirements.
# For example:
# 
# ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), 
# a double letter (...dd...), and none of the disallowed substrings.
# aaa is nice because it has at least three vowels and a double letter, 
# even though the letters used by different rules overlap.
# jchzalrnumimnmhp is naughty because it has no double letter.
# haegwjzuvuyypxyu is naughty because it contains the string xy.
# dvszwmarrgswjxmb is naughty because it contains only one vowel.
# How many strings are nice?

all_strings = readLines("~/Google Drive/Lorenzo/AdventCalendar/input5.txt")

is_nice = function(x){
  # Check that has at least 3 vowels
  if(nchar(x) - nchar(gsub("a|e|i|o|u", "", x)) < 3)
    return(FALSE)
  # Check that has at least one letter twice in a row
  split_x = strsplit(x, "", fixed = TRUE)[[1]]
  if(!any(split_x == c(0, split_x[-length(split_x)])))
    return(FALSE)
  # Check that has not the banned strings
  if(nchar(x) - nchar(gsub("ab|cd|pq|xy", "", x)) != 0)
    return(FALSE)
  return(TRUE)
}

nice_strings = all_strings[sapply(all_strings, is_nice)]

# --- Part Two ---
#   
# Realizing the error of his ways, Santa has switched to a better model 
# of determining whether a string is naughty or nice. 
# None of the old rules apply, as they are all clearly ridiculous.
# 
# Now, a nice string is one with all of the following properties:
#   
# It contains a pair of any two letters that appears at least twice in 
# the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), 
# but not like aaa (aa, but it overlaps).
# It contains at least one letter which repeats with exactly one 
# letter between them, like xyx, abcdefeghi (efe), or even aaa.
# For example:
#   
# qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) 
# and a letter that repeats with exactly one letter between them (zxz).
# xxyxx is nice because it has a pair that appears twice and a letter 
# that repeats with one between, even though the letters used by each rule overlap.
# uurcxstgmygtbstg is naughty because it has a pair (tg) but no 
# repeat with a single letter between them.
# ieodomkazucvgmuy is naughty because it has a repeating letter
# with one between (odo), but no pair that appears twice.
# How many strings are nice under these new rules?

is_nice_part2 = function(x){
  # Check that has a two letters repeating at least once
  split_x = strsplit(x, "", fixed = TRUE)[[1]]
  all_2grams = sapply(1:(length(split_x) - 1), function(i){
    paste0(split_x[i:(i+1)], collapse = "")
  })
  repeated_row = all_2grams == c("0", all_2grams[-length(all_2grams)])
  reptwice = repeated_row == c("0", repeated_row[-length(repeated_row)])
  repeated_row[reptwice] = FALSE
  all_2grams = all_2grams[!repeated_row]
  tabled_2grams = table(all_2grams)
  if(max(tabled_2grams) < 2)
    return(FALSE)
  # Check that has at one letter repeating with one in the middle
  repeated = split_x == c("0", "0", split_x[-c((length(split_x) - 1) : length(split_x))])
  if(!any(repeated))
    return(FALSE)
  return(TRUE)
}

nice_strings_part2 = all_strings[sapply(all_strings, is_nice_part2)]

