library(magrittr)

test_input <- c(
  ")())())"
)

real_input <- readLines("./inputs/day01-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% strsplit(split = "") %>% unlist()
}

#- LOGIC ----------------------------------------------------------------------#

#- SOLUTION PART 1 ------------------------------------------------------------#

day01_part1_solution <- function(input) {
  parsed <- input %>% parse_input()
  sum(parsed == "(") - sum(parsed == ")")
}

test_output_part1 <- -3
test_result <- day01_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day01_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day01_part2_solution <- function(input) {
  parsed <- input %>% parse_input()
  numbers <- (parsed == "(") - (parsed == ")")
  numbers %>%
    Reduce(f = `+`, init = 0, accumulate = T) %>% 
    magrittr::extract(-1) %>% 
    which(x = . == -1) %>%
    magrittr::extract(1)
}

test_output_part2 <- 5
test_result <- day01_part2_solution(input = "()())")
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day01_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
