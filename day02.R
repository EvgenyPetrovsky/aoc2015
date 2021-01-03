library(magrittr)

test_input <- c(
  "2x3x4",
  "1x1x10"
)

real_input <- readLines("./inputs/day02-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% strsplit(split = "x") %>% Map(f = as.integer)
}

#- LOGIC ----------------------------------------------------------------------#
paper_area <- function(l, h, w) {
  areas <- c(l*h, h*w, l*w)
  sum(areas*2)  + min(areas)
}

ribbon_length <- function(l, h, w) {
  dims <- c(l, h, w)
  # smallest perimeter     + bow
  sum(sort(dims)[1:2] * 2) + prod(dims)
}


#- SOLUTION PART 1 ------------------------------------------------------------#

day02_part1_solution <- function(input) {
  input %>% parse_input() %>%
    Map(f = function(x) paper_area(x[1], x[2], x[3])) %>%
    Reduce(f = sum)
}

test_output_part1 <- 58 + 43
test_result <- day02_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day02_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day02_part2_solution <- function(input) {
  input %>% parse_input() %>%
    Map(f = function(x) ribbon_length(x[1], x[2], x[3])) %>%
    Reduce(f = sum)
}

test_output_part2 <- 34 + 14
test_result <- day02_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day02_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
