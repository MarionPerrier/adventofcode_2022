#Part 1
rucksacks <- read.table("input3.txt")
rucksacks <- cbind(rucksacks, "compart2")
colnames(rucksacks) <- c("compart1", "compart2")

#split the strings into the middle, first half of the string
# in compart1 last part in compart2
split_in_two_1 <- function(x) {
  substr(x["compart1"], nchar(x["compart1"])/2+1, nchar(x["compart1"]))
}
split_in_two_2 <- function(x) {
  substr(x["compart1"], 1, nchar(x["compart1"])/2)
}
rucksacks["compart2"] <- apply(rucksacks, 1, split_in_two_1)
rucksacks["compart1"] <- apply(rucksacks, 1, split_in_two_2)

#search of common char
common_elements <- apply(rucksacks, 1, function(x){Reduce(intersect, strsplit(x,""))})

myLetters <- c(letters, LETTERS)

#convert the equivalent numbers of the letters and sum it up
sum(sapply(common_elements, function(x){match(x,  myLetters)}))

#######
#Part2#
#######
#Read the input as a matrix with 3 columns
rucksacks <- scan('input3.txt', what=character())
rucksacks <- matrix(rucksacks, ncol = 3, byrow = TRUE)

#Look for common elements within the 3 elves rucksacks
common_elements <- apply(rucksacks, 1, function(x){Reduce(intersect, strsplit(x,""))})
#sum their number equivalent
sum(sapply(common_elements, function(x){match(x,  myLetters)}))
