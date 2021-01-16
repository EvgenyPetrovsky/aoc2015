library(magrittr)

test_input <- c(
  ".#.#.#",
  "...##.",
  "#....#",
  "..#...",
  "#.#..#",
  "####.."
)

test_input_part2 <- c(
  "##.#.#",
  "...##.",
  "#....#",
  "..#...",
  "#.#..#",
  "####.#"

)

real_input <- readLines("./inputs/day18-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

#' takes strings of values "." and "#" and returns matrix of 0 and 1
parse_input <- function(input) {
  rows <- length(input)
  cols <- nchar(input[1])
  vec <- input %>% strsplit(split = "") %>% Reduce(f = c) %>% equals("#") + 0L
  matrix(data = vec, nrow = rows, ncol = cols, byrow = TRUE)
}

switch_on_corner_lights <- function(mx) {
  r <- nrow(mx)
  c <- ncol(mx)
  mx[1, 1] <- mx[1, c] <- mx[r, 1] <- mx[r, c] <- 1
  mx
}

#- LOGIC ----------------------------------------------------------------------#

count_pos_neighbors <- function(mx, r, c) {
  rows <- nrow(mx)
  cols <- ncol(mx)
  nbr_val <- -mx[r, c] + sum(mx[
    (max(r-1, 1):min(r+1,rows)),
    (max(c-1, 1):min(c+1,cols))])
}

cell_new_state <- function(cell_val, neighbor_val) {
  if (neighbor_val == 2) cell_val
  else if (neighbor_val == 3) 1
  else 0
}

make_round <- function(mx) {
  new_mx <- mx
  for (r in seq.int(nrow(mx))) for (c in seq.int(ncol(mx))) {
    v <- mx[r,c]
    n <- count_pos_neighbors(mx, r, c)
    new_mx[r, c] <- cell_new_state(v, n)
  }
  new_mx
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day18_part1_solution <- function(input, n) {
  init_state <- input %>% parse_input()
  end_state <- 1:n %>% 
    Reduce(
      f = function(z, x) z %>% make_round(), 
      init = init_state)
  sum(end_state)
}

exp_test_result <- 4
act_test_result <- day18_part1_solution(test_input, 4)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day18_part1_solution(real_input, 100)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day18_part2_solution <- function(input, n) {
  init_state <- input %>% parse_input() %>% switch_on_corner_lights()
  end_state <- 1:n %>% 
    Reduce(f = function(z, x) {
      z %>% make_round() %>% switch_on_corner_lights()
    }, init = init_state)
  sum(end_state)
}

exp_test_result <- 17
act_test_result <- day18_part2_solution(test_input_part2, 5)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day18_part2_solution(real_input, 100)
print(format(real_result_part2, scientific = FALSE))
