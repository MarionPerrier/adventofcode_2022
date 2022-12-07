#This day5 has given me a lot of challenges. I eventually found the correct
#answer, but there might be more elegant and more optimised ways.
#I am, however, proud to have finished it despite the difficulty!

library(stringr)

input <- readLines("input5.txt")

#The input file doesn't have a proper structure. It look like this :
#    [D]
#[N] [C]
#[Z] [M] [P]
# 1   2   3
#
#move 1 from 2 to 1
#move 3 from 1 to 3
#move 2 from 2 to 1
#move 1 from 1 to 2

#The goal is to separetly get the map and then get the moves

#get the line with the stacks numbers
i <- grep("^ 1", input, perl = TRUE)

#give the total number of stacks to fill
number_of_stacks <- as.numeric(strsplit(input[i], " ")[[1]][length(strsplit(input[i]," ")[[1]])])

#this is gonna be the number of colomns in our matrix
#the number of line is gonna be i-1 (i being the line whith the numbers identifying the crates)
map <- matrix(, nrow=i-1, ncol=number_of_stacks)
stacks <- list()

line_count <- 1
for(line in input){
  if(line == input[i]){
    break
  }
  #This clips the line every 4 characters into a vector
  line_separated <- sapply(seq(from=1, to=nchar(line), by=4), function(x) substr(line, x, x+3))

  #remove unecessary white space
  line_separated <- str_squish(line_separated)
  line_separated <- str_replace_all(line_separated, c("\\[" = "", "\\]" = ""))
  #now add it to the matrix
  map[line_count,] <- line_separated
  #and remove the "" for NA
  map[map == ''] <- NA
  line_count <- line_count +1
}

#Now we have the map, we are gonna construct our stacks with a list
#seems like I can't append with an apply. Let's try with a for loop
for(col in 1:number_of_stacks){
  stacks <- append(stacks, list(map[,col]))
}
stacks <- lapply(stacks, function(x){ x[!is.na(x) & x != ""]})
#invert all the stacks for having the last crate in the last position
stacks <- lapply(stacks, function(x){rev(x)})

#create a copy for the part 2
stacks_crane9001 <- stacks

#now that the list of stacks is done, we can start filling it with the instructions.
instructions <- input[(i+2):length(input)] #i+2 because the lines with the stacks identifiers is i, and there is an empty line after that.

for(line in instructions){
  #this extract the numbers into vectors. "move" is orders[1] "from" is orders[2], "to" is orders[3]
  orders <- as.integer(str_split(str_squish(str_replace_all(line, c("move" = "", "from" = "", "to" = ""))), " ")[[1]])
  move <- orders[1]
  from <- orders[2]
  to <- orders[3]
  #add to the receiving stack
  stacks[[to]] <- append(stacks[[to]], rev(tail(stacks[[from]], move)))
  #remove from previous stack
  stacks[[from]] <- stacks[[from]][-((length(stacks[[from]])-move+1):length(stacks[[from]]))]
}

for(line in instructions){
  #this extract the numbers into vectors. "move" is orders[1] "from" is orders[2], "to" is orders[3]
  orders <- as.integer(str_split(str_squish(str_replace_all(line, c("move" = "", "from" = "", "to" = ""))), " ")[[1]])
  move <- orders[1]
  from <- orders[2]
  to <- orders[3]
  #add to the receiving stack
  stacks_crane9001[[to]] <- append(stacks_crane9001[[to]], tail(stacks_crane9001[[from]], move))
  #remove from previous stack
  stacks_crane9001[[from]] <- stacks_crane9001[[from]][-((length(stacks_crane9001[[from]])-move+1):length(stacks_crane9001[[from]]))]
}

answer1 <- paste0(lapply(stacks, function(x){x[length(x)]}), collapse="")
answer2 <- paste0(lapply(stacks_crane9001, function(x){x[length(x)]}), collapse="")
paste0("answer1: ",answer1," and answer2: ",answer2)
