library(magrittr)

test_input <- c("abcdef")

real_input <- readLines("./inputs/day04-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input
}

#- LOGIC ----------------------------------------------------------------------#

md5 <- function(x) digest::digest(x, algo = "md5", serialize = FALSE)

find_least_number <- function(prefix, hash_starts_with) {
  n <- 0
  condition_len <- nchar(hash_starts_with)
  while ({
    hash <- md5(paste(prefix, n, sep = ""))
    substring(hash, 1, condition_len) != hash_starts_with
  }) {
    n <- n + 1
    if (n %% 10^5 == 0)
      print(paste(
        "time", Sys.time(),
        "checking", format(n, width = 7, scientific = F)
      ))
  }
  n
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day04_part1_solution <- function(input) {
  find_least_number(input, "00000")
}

test_output_part1 <- 609043
test_result <- day04_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day04_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day04_part2_solution <- function(input) {
  find_least_number(input, "000000")
}

real_result_part2 <- day04_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
