library(magrittr)

test_input <- c(
  "H => HO",
  "H => OH",
  "O => HH",
  "",
  "HOHOHO"
)

real_input <- readLines("./inputs/day19-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  div_position <- which(input == "")
  replacements <-
    input[1:(div_position - 1)] %>%
    strsplit(split = " => ") %>%
    Map(f = function(x) list(from = x[1], to = x[2]))
  starting_value <- input[div_position + 1]
  list(replacements = replacements, starting_value = starting_value)
}

#- LOGIC ----------------------------------------------------------------------#

#' function takes position in initial string and makes all possible replacements
#' for substrings starting from that position
get_all_replacements_for_position <- function(string, position, replacements) {
  start_str <- substr(string, 1, position - 1)
  make_one_replacement <- function(from, to) {
    pattern <- paste0("^", start_str, from)
    replacement <- paste0(start_str, to)
    gsub(pattern, replacement, x = string)
  }

  replacements %>%
    Map(f = function(x) make_one_replacement(x$form, x$to)) %>%
    Reduce(f = c)
}

get_all_replacements_for_string <- function(string, replacements) {
  seq.int(nchar(string)) %>%
    Map(f = function(position) get_all_replacements_for_position(string, position, replacements)) %>%
    Reduce(f = c)
}
#- SOLUTION PART 1 ------------------------------------------------------------#

day19_part1_solution <- function(input) {
  i <- input %>% 
    parse_input()
  get_all_replacements_for_string(i$starting_value, i$replacements) %>%
    unique() %>%
    length()
}

exp_test_result <- 7
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
