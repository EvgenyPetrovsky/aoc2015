library(magrittr)

test_input <- c(
  "Alice would gain 54 happiness units by sitting next to Bob.",
  "Alice would lose 79 happiness units by sitting next to Carol.",
  "Alice would lose 2 happiness units by sitting next to David.",
  "Bob would gain 83 happiness units by sitting next to Alice.",
  "Bob would lose 7 happiness units by sitting next to Carol.",
  "Bob would lose 63 happiness units by sitting next to David.",
  "Carol would lose 62 happiness units by sitting next to Alice.",
  "Carol would gain 60 happiness units by sitting next to Bob.",
  "Carol would gain 55 happiness units by sitting next to David.",
  "David would gain 46 happiness units by sitting next to Alice.",
  "David would lose 7 happiness units by sitting next to Bob.",
  "David would gain 41 happiness units by sitting next to Carol."
)

real_input <- readLines("./inputs/day13-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  NULL
}

#- LOGIC ----------------------------------------------------------------------#


#- SOLUTION PART 1 ------------------------------------------------------------#

day13_part1_solution <- function(input) {
  NULL
}

exp_test_result <- -1
act_test_result <- day13_part1_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day13_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day13_part2_solution <- function(input) {
  NULL
}

exp_test_result <- -1
act_test_result <- day13_part2_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day13_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
