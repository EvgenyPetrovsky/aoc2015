library(magrittr)

test_input <- c(
  "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
  "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
)

real_input <- readLines("./inputs/day15-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% Map(f = parse_input_line)
}

parse_input_line <- function(input_line) {
  numbers <- input_line %>% gregexpr(pattern = "-?\\d+") %>% regmatches(x = input_line) %>% unlist()
  ingredient <- input_line %>% gsub(pattern = "^(\\w+): .*$", replacement = "\\1")
  list(
    ingredient = ingredient,
    capacity = numbers[1],
    durability = numbers[2],
    flavor = numbers[3],
    texture = numbers[4],
    calories = numbers[5])
}

#- LOGIC ----------------------------------------------------------------------#


#- SOLUTION PART 1 ------------------------------------------------------------#

day15_part1_solution <- function(input) {
  NULL
}

exp_test_result <- -1
act_test_result <- day15_part1_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day15_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day15_part2_solution <- function(input) {
  NULL
}

exp_test_result <- -1
act_test_result <- day15_part2_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day15_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
