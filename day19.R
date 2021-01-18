library(magrittr)

test_input <- c(
)

real_input <- readLines("./inputs/day19-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  NULL
}

#- LOGIC ----------------------------------------------------------------------#


#- SOLUTION PART 1 ------------------------------------------------------------#

day19_part1_solution <- function(input) {
  NULL
}

exp_test_result <- -1
act_test_result <- day19_part1_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day19_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day19_part2_solution <- function(input) {
  NULL
}

exp_test_result <- -1
act_test_result <- day19_part2_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day19_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
