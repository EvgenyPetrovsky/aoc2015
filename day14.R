library(magrittr)

test_input <- c(
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
  "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
)

real_input <- readLines("./inputs/day14-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% Map(f = parse_input_line)
}

#' returns structure
#'  - name
#'  - speed
#'  - run_time
#'  - rest_time
parse_input_line <- function(input_line) {
  pattern <- "^(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds.$"
  name <- input_line %>% gsub(pattern = pattern, replacement = "\\1")
  speed <- input_line %>% gsub(pattern = pattern, replacement = "\\2") %>% as.integer()
  run_time <- input_line %>% gsub(pattern = pattern, replacement = "\\3") %>% as.integer()
  rest_time <- input_line %>% gsub(pattern = pattern, replacement = "\\4") %>% as.integer()
  list(name = name, speed = speed, run_time = run_time, rest_time = rest_time)
}

#- LOGIC ----------------------------------------------------------------------#

distance_gone <- function(reindeer, time) {
  with(reindeer, {
    period <- run_time + rest_time
    total_run_time <- time %/% period * run_time + min(time %% period, run_time)
    total_run_time * speed
  }) 
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day14_part1_solution <- function(input, time) {
  distance_fun <- function(reindeer) distance_gone(reindeer, time)
  input %>% parse_input() %>% Map(f = distance_fun) %>% Reduce(f = max)
}

exp_test_result <- 1120
act_test_result <- day14_part1_solution(test_input, 1000)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day14_part1_solution(real_input, 2503)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day14_part2_solution <- function(input, time) {
  distance_fun <- function(reindeer) distance_gone(reindeer, time)
  deers <- input %>% parse_input() 
  seq.int(from = 1, to = time) %>% 
    Reduce(f = function(bonus_points, t) {
      distances <- deers %>% Map(f = function(deer) distance_gone(deer, t)) %>% unlist()
      round_winners <- distances == max(distances)
      bonus_points + as.integer(round_winners)
    }, init = integer(length(deers))) %>%
    Reduce(f = max)
}

exp_test_result <- 689
act_test_result <- day14_part2_solution(test_input, 1000)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day14_part2_solution(real_input, 2503)
print(format(real_result_part2, scientific = FALSE))
