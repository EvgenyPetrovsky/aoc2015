library(magrittr)

test_input <- c(
  "ugknbfddgicrmopn",
  "aaa",
  "jchzalrnumimnmhp",
  "haegwjzuvuyypxyu",
  "dvszwmarrgswjxmb"
)

test_input_2 <- c(
  "qjhvhtzxzqqjkmpb",
  "xxyxx",
  "uurcxstgmygtbstg",
  "ieodomkazucvgmuy"
)

real_input <- readLines("./inputs/day05-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input
}

#- LOGIC ----------------------------------------------------------------------#

is_nice_string <- function(x) {
  s <- strsplit(x, split = "") %>% unlist()
  len <- length(s)
  # It contains at least three vowels (aeiou only)
  vowels <- strsplit("aeiou", split = "") %>% unlist()
  c1 <- (s %in% vowels) %>% sum() >= 3
  # It contains at least one letter that appears twice in a row
  c2 <- 2:len %>%
    Reduce(
      f = function(z, x) any(s[x-1] == s[x], z),
      init = FALSE)
  # It does not contain the strings ab, cd, pq, or xy
  prohibited <- c("ab", "cd", "pq", "xy")
  c3 <- 2:len %>% Reduce(
    f = function(z, x) all(!(paste0(s[x-1],s[x]) %in% prohibited), z),
    init = TRUE)

  # all 3 conditions must be met
  all(c1, c2, c3)
}

is_nice_string_2 <- function(x) {
  c1 <- grepl(pattern = "(\\w{2})\\w*\\1", x, perl = T)
  c2 <- grepl(pattern = "(\\w)\\w\\1", x, perl = T)
  all(c1, c2)
}
#- SOLUTION PART 1 ------------------------------------------------------------#

day05_part1_solution <- function(input) {
  input %>%
    parse_input() %>%
    Filter(f = is_nice_string) %>%
    length()
}

test_output_part1 <- 2
test_result <- day05_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day05_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day05_part2_solution <- function(input) {
  input %>%
    parse_input() %>%
    Filter(f = is_nice_string_2) %>%
    length()
}

test_output_part2 <- 2
test_result <- day05_part2_solution(test_input_2)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day05_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
