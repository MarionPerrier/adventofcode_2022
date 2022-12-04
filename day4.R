sections <- read.table(text = gsub(",", "-", readLines('input4.txt')), sep = "-")
colnames(sections) <- c("Elf1_start", "Elf1_end", "Elf2_start", "Elf2_end")


########
#Part 1#
########

# a way to check overlap :
#> x1 <- c(1:5)
#> x2 <- c(4:7)
#> x3 <- c(2:4)
#> (x2 %in% x1)
#[1]  TRUE  TRUE FALSE FALSE
#> all(x2 %in% x1)
#[1] FALSE
#> all(x3 %in% x1)
#[1] TRUE
#> all(x1 %in% x3)
#[1] FALSE

count_overlap <- 0

is_entirely_overlaping <- function(x) {
  #For readability
  Elf1 <- c(x["Elf1_start"]:x["Elf1_end"])
  Elf2 <- c(x["Elf2_start"]:x["Elf2_end"])

  if(all(Elf1 %in% Elf2) || all(Elf2 %in% Elf1)) {
    count_overlap = count_overlap + 1
  }
  return(count_overlap)
}

part1_answer <- sum(apply(sections, 1, is_entirely_overlaping))

########
#Part 2#
########

is_overlaping <- function(x) {
  #For readability
  Elf1 <- c(x["Elf1_start"]:x["Elf1_end"])
  Elf2 <- c(x["Elf2_start"]:x["Elf2_end"])

  if(any(Elf1 %in% Elf2)) {
    count_overlap = count_overlap + 1
  }
  return(count_overlap)
}

part2_answer <- sum(apply(sections, 1, is_overlaping))

print(paste0("Answer part1 is ", part1_answer, " and answer part2 is ", part2_answer))
