library(magrittr)

test_input <- c(
  '{"a":{"b":4},"c":-1}'
)

real_input <- readLines("./inputs/day12-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% 
    # we must make array of one element distinguishable from just value
    gsub(pattern = "[\"red\"]", replacement = "[\"red\",\"\"]", fixed = T) %>%
    jsonlite::fromJSON(simplifyVector = F)
}

#- LOGIC ----------------------------------------------------------------------#

#' check whether x is an object. 
#' 
#' jsonlite::fromJSON implementation transforms arrays into lists in case they 
#' contain heterogeneous data, so we can't use is.vector() check.
#' instead we must check whether given list 
#'  - has names for elements (property of object)
#'  - or not (property of array)
is_object <- function(x) is.list(x) && !is.null(names(x))

#' check whether x is an array
is_array <- function(x) is.list(x) && is.null(names(x))

#' check if given list has element with value = "red"
has_red <- function(x)
  x %>% 
    Map(f = function(x) is.character(x) && length(x) == 1 && x == "red") %>% 
    Reduce(f = any)

#' traverse all elements of a given object and sum all numeric values
sum_all_numbers <- function(x, check_for_red = FALSE) {
  iter <- function(x) {
    if (is.numeric(x)) {
      sum(x, na.rm = T)
    } else if (is.list(x)) {
      if (is_object(x) && has_red(x) && check_for_red) 0 
      else x %>% Map(f = iter) %>% Reduce(f = `+`, init = 0)
    } else if (is_array(x)) {
      x %>% Map(f = iter) %>% Reduce(f = `+`, init = 0)
    } else if (is.character(x)) {
      0
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

exp_test_result <- 3
act_test_result <- day12_part2_solution('{"a":1,"b":{"ba":65, "bb":["red"], "bc":[1,2]},"c":"orange","d":["red", 10]}')
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day12_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
