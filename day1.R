#Part 1
challenge_input = readLines("input1.txt")
challenge_input <- as.integer(challenge_input)

#create a variable containing the maximum calories value
calories_max <- 0

#read the table were you add all the calories contained in one elf
total_calorie <- 0
for(calorie in as.list(challenge_input)){
  if(is.na(calorie)){
    #if the total amount of calories for this current elf is bigger than the stored one, we replace it
    if(total_calorie > calories_max){
      calories_max <- total_calorie
    }
    #reset total_calories for the next elf
    total_calorie <- 0
    next
  }
  total_calorie = total_calorie + calorie
}
print(c("The maximum of calories carried by an elf is : ", calories_max))

#Part 2
#create a variable containing the maximum calories value
top3 <- c(0, 0, 0)

#read the table were you add all the calories contained in one elf
total_calorie <- 0
for(calorie in as.list(challenge_input)){
  if(is.na(calorie)){
    #we check if the total_calorie fits in the top 3. If it is, we add it at the appropriate place and remove the fourth number
    if(total_calorie > top3[1]){
      top3 <- append(top3, total_calorie, after=0)
      top3 <- top3[-length(top3)]
    }
    else if(total_calorie > top3[2]){
      top3 <- append(top3, total_calorie, after=1)
      top3 <- top3[-length(top3)]
    }
    else if(total_calorie > top3[3]){
      top3 <- append(top3, total_calorie, after=1)
      top3 <- top3[-length(top3)]
    }
    total_calorie <- 0
    next
  }
  total_calorie = total_calorie + calorie
}
#return the sum of the top3 array
print(c("The total ammount of calories carried by the top 3 elves is", sum(top3)))
