# --- Day 11: Corporate Policy ---
#   
#   Santa's previous password expired, 
# and he needs help choosing a new one.
# 
# To help him remember his new password after 
# the old one expires, Santa has devised a method of
# coming up with a password based on the previous one. 
# Corporate policy dictates that passwords must be exactly
# eight lowercase letters (for security reasons), 
# so he finds his new password by incrementing his old 
# password string repeatedly until it is valid.
# 
# Incrementing is just like counting with numbers: xx, xy, xz,
# ya, yb, and so on. Increase the rightmost letter one step;
# if it was z, it wraps around to a, and repeat with the next 
# letter to the left until one doesn't wrap around.
# 
# Unfortunately for Santa, a new Security-Elf recently started, 
# and he has imposed some additional password requirements:
#   
# Passwords must include one increasing straight of at 
# least three letters, like abc, bcd, cde, and so on, up to xyz. 
# They cannot skip letters; abd doesn't count.
# Passwords may not contain the letters i, o, or l, as
# these letters can be mistaken for other characters and are therefore
# confusing.
# Passwords must contain at least two different, 
# non-overlapping pairs of letters, like aa, bb, or zz.
# For example:
# 
# hijklmmn meets the first requirement (because it 
# contains the straight hij) but fails the second requirement 
# requirement (because it contains i and l).
# abbceffg meets the third requirement (because it repeats 
# bb and ff) but fails the first requirement.
# abbcegjk fails the third requirement, because it only
# has one double letter (bb).
# The next password after abcdefgh is abcdffaa.
# The next password after ghijklmn is ghjaabcc,
# because you eventually skip all the passwords that 
# start with ghi..., since i is not allowed.
# Given Santa's current password (your puzzle input), 
# what should his next password be?
# 
# Your puzzle input is vzbxkghb.


# Check that the 3 requirements are met
requirements = function(x){
  banned = c("o", "i", "l")
  if(grepl(paste0(banned, collapse = "|"), x))
    return(FALSE)
  three_cons = paste0(letters[-c((length(letters) - 1) : length(letters))], letters[-c(1, length(letters))], letters[-c(1,2)])
  if(!grepl(paste0(three_cons, collapse = "|"), x))
    return(FALSE)
  repeated = paste0(letters, letters)
  if(sum(sapply(repeated, function(i){ grepl(i, x) })) <= 1)
    return(FALSE)
  TRUE
}

# Next password
next_pwd = function(x){
  letters_split = strsplit(x, "", fixed = TRUE)[[1]]
  i = length(letters_split)
  while(letters_split[i] == "z" ){
    letters_split[i] = "a"
    i = i - 1
    if(i == 0)
      break
  }
  if(i != 0)
    letters_split[i] = letters[which(letters %in% letters_split[i]) + 1]
  paste0(letters_split, collapse = "")
}

# Find the next one
input = "vzbxkghb"
newpwd = next_pwd(input)
while(!requirements(newpwd))
  newpwd = next_pwd(newpwd)
newpwd

# And the next one again
input = newpwd
newpwd = next_pwd(input)
while(!requirements(newpwd))
  newpwd = next_pwd(newpwd)
newpwd