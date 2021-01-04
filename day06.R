library(magrittr)

test_input <- c(
  "turn on 0,0 through 999,999",
  "toggle 0,0 through 999,0",
  "turn off 499,499 through 500,500"
)

test_input_part2 <- c(
  "turn on 0,0 through 0,3",
  "turn off 0,0 through 1,1",
  "toggle 0,0 through 999,999"
)

real_input <- readLines("./inputs/day06-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

#' structure of parsed input is a list of
#'   instruction: turn on / turn off / toggle
#'   start: start of the range as 2 number coordinate
#'   finish: end of the range as 2 number coordinate
parse_input <- function(input) {
  input %>% 
    Map(f = parse_line)
}

parse_line <- function(input_line) {
  instruction <- 
    input_line %>% 
    gregexpr(pattern = "^[a-z]+( [a-z]+)?") %>% 
    regmatches(x = input_line) %>% unlist()
  range <- 
    input_line %>% 
    gregexpr(pattern = "(\\d)+,(\\d)+") %>% 
    regmatches(x = input_line) %>%
    unlist() %>%
    strsplit(split = ",") %>%
    Map(f = as.integer)
    
  list(instruction = instruction, start = range[[1]], finish = range[[2]])
}

#- LOGIC ----------------------------------------------------------------------#

initialize_grid <- function() {
  matrix(0, nrow = 1000, ncol = 1000)
}

instruction_mapping_part1 <- list(
  `turn on` = function(x) (x | TRUE) + 0L,
  `turn off` = function(x) (x & FALSE) + 0L,
  `toggle` = function(x) !x + 0L
)

instruction_mapping_part2 <- list(
  # increase the brightness of those lights by 1
  `turn on` = function(x) x + 1L,
  # decrease the brightness of those lights by 1, to a minimum of zero
  `turn off` = function(x) {x <- x - 1L; x[x < 0] <- 0L; x},
  # increase the brightness of those lights by 2
  `toggle` = function(x) x + 2L
)

apply_instruction <- function(grid, instruction, instruction_mapping) {
  rows <- c(instruction$start[1]:instruction$finish[1]) + 1
  cols <- c(instruction$start[2]:instruction$finish[2]) + 1
  func <- instruction_mapping[[instruction$instruction]]
  
  # update grid
  grid[rows,cols] <- func(grid[rows,cols])
  grid
}

plot_grid <- function(grid) {
  par(mar=c(0, 0, 0, 0))
  d <- max(grid) %>% max(1)
  image(grid / d, useRaster=TRUE, axes=FALSE)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day06_part1_solution <- function(input) {
  grid <- 
    input %>% 
    parse_input() %>% 
    Reduce(
      f = function(z, x) apply_instruction(z, x, instruction_mapping_part1),
      init = initialize_grid())
  plot_grid(grid)
  sum(grid)
}

test_output_part1 <- 10^6 - 10^3 - 2*2
test_result <- day06_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day06_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day06_part2_solution <- function(input) {
  grid <- 
    input %>% 
    parse_input() %>% 
    Reduce(
      f = function(z, x) apply_instruction(z, x, instruction_mapping_part2),
      init = initialize_grid())
  plot_grid(grid)
  sum(grid)
}

test_output_part2 <- (+1 * 4) + (-1 * (4-2)) + (+2 * 10^6)
test_result <- day06_part2_solution(test_input_part2)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day06_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
