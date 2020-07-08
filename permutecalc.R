# Assignment 1: permute


#Collaborators: Taylor Dieffenbach, Kris Lokere
#
# <- this hash (#) symbol precedes comments in R. You compupter
# will ignore lines that begin with a hash.
#
#

install.packages("stringr")       # Install stringr package
library("stringr")                # Load stringr package


#
#
#
# This assignment has 8 sub-parts. You should do them in order.
# Collaboration is encouraged, and your code should be your own.
# See the details of the extyension school collaboration policy in the syllabus
# (TLDR you must cite your sources **including stack overflow** and collaborators)
#
# 1) download and install R and RStudio
#
# 2) create a folder named Dashboard. This is where you will
# keep all of your R code. Make a subfolder named Permutation.
# Put these files (app.R and permutecalc.R) and into that folder.
#
#  Dashboard
#  |
#  +-- Permute
#      |
#      +-- permutecalc.R
#      +-- app.R
#
# 3) write and test code for the function Perm.apply (see below)
#
# below, we have marked the places where you will write code like this
# ** your code here **
#
# you may find the following functions useful as you are writing your functions:
# print() # (for testing and debugging)
# str_detect()
# str_locate()
# character()
# str_sub()
# paste()
# any function that you have already written
#
# 4) write and test code for the function Perm.cycle.convert (see below)
#
# 5) write and test code for the function Perm.multiply (see below)
#
# 6) write and test code for the function Perm.powerString (see below)
#
# 7) write and test code for the function Perm.inverse (see below)
#
# 8) write and test code for the function Perm.conjugate (see below)
#
# When you are done, run app.R as a final test to make sure all of the parts are working
# and to bask in the glory of having written your own library of permutation functions.
# When you are finished, submit this file, still named permutecalc.R, on gradescope.
#
#
# If you get stuck at any point, spend at least 10-20 minutes trying to get
# un-stuck before you give up. Consult friends and the internet and come to 
# office hours. You are also welcome to email us! If we can't answer your question
# in more than a sentence or two, we will tell you to join our office hours 
# or, if we are feeling extremely generous, we will set up a zoom call with you :)



#The code in this file should work for any set of characters. ABCDE, 12345, etc.
library(stringr)






#3)
# Perm.apply has two inputs and one output
# x - a single character, passed as a string like "1" or "A"
# perm - a permutation written in cycle notation like "(13)(245)"
# output - a single character, returned as a string like "2" or "B"
#
# Perm.apply(x,perm) returns the result of the permutation perm applied to x
# Example usage:
#
# Perm.apply("5",("(13)(245)"))
# [1] "2"
#
Perm.apply <- function(x,perm){
  
  # dummy return value - delete the following line when you 
  # start writing your version of this function:
  #  return("5")
  
  # If x is not in the permutation, return x
  # ** your code here **
  
  if (!str_detect(perm, x)) return(x)
  
  x_location <- str_locate(perm, x)
  cursor <- x_location +1
  cursor
  
  target_char <- substr(perm, cursor, cursor)
  
  if (target_char != '(' & target_char != ')') return(target_char)
  
  cursor <- x_location
  target_char <- substr(perm, cursor, cursor)
  while (target_char != '(' & target_char != ')') {
    cursor <- (cursor - 1)
    target_char <- substr(perm, cursor, cursor)
  }
  
  target_char <- substr(perm, cursor +1, cursor +1)
  return(target_char)
  
  suppressWarnings()

  
  
}
#You may find the following lines useful for testing your code:
Perm.apply("5",("(13)(245)"))
Perm.apply("5",("(13)(24)"))
Perm.apply("5",("(13245)"))
#debug(Perm.apply)
#undebug(Perm.apply)






#4)
# Perm.cycle.convert has one input and one output
# fval - a vector consisting only of non-repeating digits
# corresponding to a permutation, like c("2","4","3","1"), 
# which corresponds to (124)
# output - a permutation written in cycle notation like "(124)"
#
# Perm.cycle.convert(fval) converts the vector fval to cycle notation
# The fval argument corresponds to what we've been calling the "function
# list", so fval=c("2","4","3","1") means a(1) = 2, a(2) = 4, a(3) = 3, a(4)=1
# This is specific to permutations of the digits 1 through 9
# It will be reused in other apps
# Example usage:
#
# Perm.cycle.convert(c("2","4","3","1"))
# [1] "(124)"
#

Perm.cycle.convert <- function(fval){
  
  #Build the answer as a vector of characters
  # ** your code here **
  
  fval <- c(fval, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  avail <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  for (k in 1:9) {
    if ((k==as.numeric(fval[k]))||(fval[k]=='0')) {avail[k] <-0}
  }
  
  a <- c()
  for (i in 1:9){
    if(avail[i]!=0){
      a <- c(a, "(")
      a <- c(a, i)
      avail[i] <- 0
      j <- i
      while(as.numeric(fval[j])!=i)
      {
        j <- as.numeric(fval[j])
        a <- c(a ,j)
        avail[j] <- 0
      }
      a <- c(a, ')')
    }
  }
  
  #collapse
  a_ <- paste(a, sep='', collapse='')
  return(a_)
  
  suppressWarnings()
}
  
  
  
  
  
  # a <- as.integer(unlist(strsplit(fval, ',')))
  # print(a)
  # b <- c(1)
  # # loop through the digits 1:9 in order
  # for (i in 1:length(a)){
  #   if (a[i] %in% b) {break} 
  #   else if (a[i] == i) {next} 
  #   else b <- c(b, a[i])}
  # print(b)
  # 
  # d <- c()
  # if (length(b) < length(a)) {
  #   for (i in 1:length(a)){
  #     if (a[i] %in% b) {next} 
  #     else if (a[i] == i) {next} 
  #     else if (a[i] %in% d) {break}
  #     else d <- c(d, a[i])}}
  # print(d)
  # 
  # e <- c()
  # if (length(b)+length(d) < length(a)) {
  #   for (i in 1:length(a)){
  #     if (a[i] %in% b) {next} 
  #     else if (a[i] %in% d) {next} 
  #     else if (a[i] == i) {next} 
  #     else if (a[i] %in% d) {break} 
  #     else d <- c(d, a[i])}}
  # print(e)
  # 
  # #collapse
  # 
  # if (length(b) > 0) {fin1 <- paste(b, sep='')}
  # if (length(d) > 0) {fin2 <- paste(d, sep='')}
  # if (length(e) > 0) {fin3 <- paste(e, sep='')}
  # 
  # if (length(b) > 0) {fin_1 <- paste(fin1, sep='', collapse='')}
  # if (length(b) > 0) {fin_1 <- paste('(', fin_1, ')', sep='', collapse='')}
  # if (length(d) > 0) {fin_2 <- paste(fin2, sep='', collapse='')}
  # if (length(d) > 0) {fin_2 <- paste('(', fin_2, ')', sep='', collapse='')}
  # if (length(e) > 0) {fin_3 <- paste(fin3, sep='', collapse='')}
  # if (length(e) > 0) {fin_3 <- paste('(', fin_3, ')', sep='')}
  # 
  # if (length(d) > 0) {final_2 <- paste(fin_1, fin_2, sep='')}
  # if (length(e) > 0) {final_3 <- paste(fin_1, fin_2, fin_3, sep='')}
  # 
  # print(length(b))
  # print(length(d))
  # print(length(e))
  # 
  # 
  # 
  # if (length(d)== 0 & length(e)== 0) {return(fin_1)} 
  # else if (length(e)==0) {return(final_2)}
  # else return(final_3)
  # 
  # }

  
 
  
  
  # If the symbol is unchanged or is already in a cycle, there is nothing to do
  # start a new cycle if you reach a number that you have already added to your cycle
  # Otherwise keep tracing through the cycle until you find the end
  # don't forget to close the cycle when it returns to its starting character
  # ** your code here **
  
  #Collapse the vector of cycles into a string and return it
  # ** your code here **
  

#You may find the following lines useful for testing your code:
Perm.cycle.convert(c("2","4","3","1"))
Perm.cycle.convert(c("2","1","3","4"))
Perm.cycle.convert(c("2","1","4","3"))
#debug(Perm.cycle.convert)
#undebug(Perm.cycle.convert)






#5)
# Perm.multiply has two inputs and one output
# a - a permutation written in cycle notation like "(24)(567)"
# b - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(134)"
#
# Perm.multiply(a,b) returns the result of multiplying the permutations ab
# or "b followed by a"
# Example usage:
#
# Perm.multiply("(123)","(12)(34)")
# [1] "(134)"
#
#Compute the product ab of two permutations of the symbols "1" through "9"
Perm.multiply <- function(a,b){
  
  # dummy return value - delete the following line when you 
  # start writing your version of this function:
  #return("(134)")
  
  if (a == "") {return(b)} else if (b=="") {return(a)}
  
  b_ <-0
  for (i in 1:9) {
    b_[i] <- Perm.apply(as.character(i), b)}
  
  ab_ <-0
  for (i in 1:9) {
    ab_[i] <- Perm.apply(b_[i],a)
  }
  
  if (paste(ab_, sep='', collapse='')=='123456789'){
    return('I')}
  else {return(Perm.cycle.convert(ab_))}
  
  suppressWarnings()
  }
  

  
  # a <- str_extract_all("(12)(34)", "(?<=\\().+?(?=\\))")[[1]]
  # b <- str_extract_all("(15)(23)", "(?<=\\().+?(?=\\))")[[1]]
  # a_b <- c(a, b)
  # 
  # new <-c()
  # 
  # for (k in 1:length(a_b)) {
  #   if (length(strsplit(a_b[k], "")[[1]]) > 2) {
  #   for (j in ((length(strsplit(a_b[k], "")[[1]])-1):1)) 
  #   {print(j)
  #     if (length(new)<1) 
  #       {new <- paste(new, strsplit(a_b[k], "")[[1]][1], strsplit(a_b[k], "")[[1]][j+1], sep='')}
  #     else new <- paste(new, paste(strsplit(a_b[k], "")[[1]][1], strsplit(a_b[k], "")[[1]][j+1], sep=''), sep=' ')}}
  #   else new <- paste(new, a_b[k])}
  # new
  # 
  # final <- strsplit(new, " ")[[1]]
  # 
  # 
  # is.not.null <- function(x) !is.null(x)
  # 
  # 
  # dup <- final[duplicated(final)]
  # dup_index <- which(final %in% dup)
  # for (u in 1:length(dup_index)) {
  #   if (length(dup_index)==0) {break}
  #   else if ((dup_index[u+1] - dup_index[u]) == 1){
  #     final <- final[!final %in% dup]
  #   }
  # }
  # final
  # 
  # d <- final
  # for (i in length(final):1) {
  #   if (grepl(as.character(strsplit(final[i], "")[[1]][1]), final[i-1])) 
  #     # {paste(d, as.character(d[length(d)]))
  #   {{if (which((strsplit(final[i-1], "")[[1]])==(strsplit(final[i], "")[[1]][1])) == 1) {
  #     e <- which((strsplit(final[i-1], "")[[1]])==(strsplit(final[i], "")[[1]][1]))+1}
  #     else e <- which((strsplit(final[i-1], "")[[1]])==(strsplit(final[i], "")[[1]][1]))-1}
  #     f <- strsplit(final[i-1], "")[[1]][e]
  #     d[i] <- paste(f, final[i], sep='')
  #     d <- d[!d %in% final[i-1]]
  #     if (length(d)==1) {break}}}
  # 
  # return(d)
  # }
  
  
  
  # don't forget to include a special case for the identity "I"
  # aI = Ia = a, bI = Ib = b
  # if one permutation is the identity, just return the other
  # ** your code here **
  
  # if not, make a vector of the function outputs
  # you may choose to save space/code by using the order
  # if the letters of the inputs as indices (so that you can ignore them)
  # ** your code here **
  
  #If input and output are equal, return the identity
  # ** your code here **
  
  #Otherwise generate cycle notation and return that
  # ** your code here **
  

#You may find the following lines useful for testing your code:
Perm.multiply("(123)","(12)(34)")
Perm.multiply("(12)(34)","(123)")
Perm.multiply("(12)(34)","(15)(23)")
#debug(Perm.multiply)
#undebug(Perm.multiply)
#if you did it right, your code will be vectorizable as well:
#vPerm.multiply <-   Vectorize(Perm.multiply,c("a","b"))






#6)
# Perm.powerString has two inputs and one output
# perm - a permutation written in cycle notation like "(135)"
# output - a list of permutations like "(135)<br/>(153)<br/>I"
#
# Perm.powerString(perm) makes a list of powers or perm separated by HTML 
# line breaks ("<br/>")
# Example usage:
#
# Perm.powerString("(135)")
# [1] "(135)<br/>(153)<br/>I"
#
# Makes a list of powers separated by HTML line breaks
Perm.powerString <- function(perm) {
  
  # dummy return value - delete the following line when you 
  # start writing your version of this function:
  #return("(135)<br/>(153)<br/>I")
  final <- c(perm)
  a <- c(perm)
  while(a != 'I') {
  a <- Perm.multiply(a, perm)
  final <- c(final, '<br/>', a)
  }
  
  fin <- paste(final, sep='', collapse='')
  return(fin)
  
  suppressWarnings()
  # ** your code here **
}
Perm.powerString("(135)")






#7)
# Perm.inverse has one input and one output
# perm - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(132)(4986)"
# 
# Perm.inverse(perm) returns the inverse permutation of perm, denoted perm^(-1)
# You can check your code by confirming that perm*perm^(-1) == I, the identity permutation.
# Example usage:
#
# Perm.inverse("(123)(4689)")
# [1] "(132)(4986)"
#
Perm.inverse <- function(perm) {
  
  # dummy return value - delete the following line when you 
  # start writing your version of this function:
  #return("(132)(4986)")
  
  final <- c(perm)
  power <- c(perm)
  while(power != 'I'){
    final <- power
    power <- Perm.multiply(power, perm)
  }
  return(final)
  
  
  # one way to finds the inverse is by taking powers of a permutation, 
  # stopping when the next power is the identity permutation. You may find  
  # the while-loop construction helpful.
  # ** your code here **
}
#You may find the following lines useful for testing your code:
Perm.inverse("(123)(4689)")






#8)
# Perm.conjugate has two inputs and one output
# a - a permutation written in cycle notation like "(24)(567)"
# b - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(143)(2789)"
#
# Perm.conjugate(a,b) returns the conjugate aba^(-1), where a^(-1) is 
# the inverse of a. Said differently, aa^(-1) == I, the identity permutation.
# Example usage:
#
# Perm.conjugate("(24)(567)","(123)(4689)")
# [1] "(143)(2789)"
#
Perm.conjugate <- function(a,b) {
  
  # dummy return value - delete the following line when you 
  # start writing your version of this function:
  #return("(143)(2789)")
  
  inv <- Perm.inverse(a)
  
  prod1 <- Perm.multiply(a, b)
  prod2 <- Perm.multiply(prod1, inv)
  
  return(prod2)
  
  # ** your code here **
}
#You may find the following lines useful for testing your code:
Perm.conjugate("(24)(567)","(123)(4689)")


