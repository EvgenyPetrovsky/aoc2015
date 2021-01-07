library(magrittr)

test_input <- c(
  '{"a":{"b":4},"c":-1}'
)

real_input <- readLines("./inputs/day12-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% jsonlite::fromJSON()
}

#- LOGIC ----------------------------------------------------------------------#

sum_all_numbers <- function(x, check_for_red = FALSE) {
  iter <- function(x) {
    if (is.numeric(x)) {
      sum(x, na.rm = T)
    }
    else if (is.list(x)) {
      has_red <- 
        x %>% 
        Map(f = function(x) is.character(x) && length(x) == 1 && x == "red") %>% 
        Reduce(f = any, init = FALSE)
      if (has_red & check_for_red) 0 
      else x %>% Map(f = iter) %>% Reduce(f = `+`, init = 0)
    } else if (is.character(x)) {
      #x[grepl(pattern = "^-?\\d+$", x)] %>% as.numeric() %>% sum()
      suppressWarnings(as.numeric(x)) %>% sum(na.rm = T)
    } else if (is.null(x)) {
      0
    } else {
      print(paste("unrecognized type:", typeof(x), "value:", x))
      0
    }
  }
  iter(x)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day12_part1_solution <- function(input) {
  input %>% parse_input() %>% sum_all_numbers()
}

exp_test_result <- 3
act_test_result <- day12_part1_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day12_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day12_part2_solution <- function(input) {
  input %>% parse_input() %>% sum_all_numbers(check_for_red = T)
}

real_result_part2 <- day12_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
