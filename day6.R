#read text input as one string
input <- paste(readLines("input6.txt"), collapse="\n")

#check every 4 chunks of characters, the unique characters in the string
for(i in 1:nchar(input)){
  vector_letters <- unlist(strsplit(substr(input, i, i+3), ""))
  unique_letters <- unique(vector_letters)
  #if there is 4 unique characters, here is our solution !
  if(length(unique_letters) == 4){
    solution1 <- i+3
    break
  }
}

#check every 14 chunks of characters, the unique characters in the string
for(i in 1:nchar(input)){
  vector_letters <- unlist(strsplit(substr(input, i, i+13), ""))
  unique_letters <- unique(vector_letters)
  #if there is 14 unique characters, here is our solution !
  if(length(unique_letters) == 14){
    solution2 <- i+13
    break
  }
}

print(paste0("answer1: ", solution1, " and answer2: ", solution2))
