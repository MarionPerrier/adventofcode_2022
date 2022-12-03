#library to make life easier
library(stringr)

#Reading input (you can find the imput on the adventofcode website)
challenge_input <- read.table("input2.txt", sep = " ")

########
#Part 1#
########
challenge_input <- cbind(challenge_input, "Me")
colnames(challenge_input) <- c("Elf", "Move", "Me")

#Replace X, Y and Z by A B and C
challenge_input$Me <- str_replace_all(challenge_input$Move, c("X" = "A", "Y" = "B", "Z" = "C"))

total_point = 0

#Rock(A) = 1 point
#Paper(B) = 2 points
#Scissors(C) = 3 points

#Victory = 6 points
#Draw = 3 points
#Defeat = 0 points

score_calculation <- function(x) {
  if(x["Me"] == "A"){
    total_point = total_point + 1
    if(x["Elf"] == "A"){
      total_point = total_point + 3
    } else if (x["Elf"] == "C") {
      total_point = total_point + 6
    }
  } else if (x["Me"] == "B"){
    total_point = total_point + 2
    if(x["Elf"] == "B"){
      total_point = total_point + 3
    } else if (x["Elf"] == "A"){
      total_point = total_point + 6
    }
  } else {
    total_point = total_point + 3
    if(x["Elf"] == "C"){
      total_point = total_point + 3
    } else if (x["Elf"] == "B"){
      total_point = total_point + 6
    }
  }
  return(total_point)
}

points_before <- sum(apply(challenge_input, 1, score_calculation))

########
#Part 2#
########

#X = Lose
#Y = Draw
#Z = Win

#Replace the X-Y-Z by the appropriate letter play

what_to_play <- function(x) {
  #if it is a draw (Y), replace with the Elf's letter
  if(x["Move"] == "Y"){
    x["Me"] <- x["Elf"]
  }
  #If it is a lose, replace by appropriate letter
  #A rock win over C Scissors
  #B paper win over A Rock
  #C Scissors win over B Paper
  else if (x["Move"] == "X"){
    ifelse(x["Elf"] == "A", x["Me"] <- "C", ifelse(x["Elf"] == "B", x["Me"] <- "A", x["Me"] <- "B"))
  }
  #If it is a win, replace by appropriate letter
  else {
    ifelse(x["Elf"] == "A", x["Me"] <- "B", ifelse(x["Elf"] == "B", x["Me"] <- "C", x["Me"] <- "A"))
  }
}

challenge_input$Me <- apply(challenge_input, 1, what_to_play)

#Then calculate score with the #part1 function
points_after <- sum(apply(challenge_input, 1, score_calculation))

print(c("Part1 : ", points_before, "Part2 : ", points_after))
