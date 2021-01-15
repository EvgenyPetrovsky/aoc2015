library(magrittr)

test_input <- c(
  "20", "15", "10", "5", "5"
)

real_input <- readLines("./inputs/day17-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  as.integer(input)
}

#- LOGIC ----------------------------------------------------------------------#

const_EGGNOG_VOLUME <- 150L

count_all_combinations <- function(volumes, goal) {
  len <- length(volumes)
  iter <- function(pos, acc) {
    if (acc == goal) 1
    else if (pos > len) 0
    else if (acc  > goal) 0
    else iter(pos + 1, acc + volumes[pos]) + iter(pos + 1, acc)
  }
  iter(pos = 1, acc = 0)
}

count_all_combinations_part2 <- function(volumes, goal) {
  len <- length(volumes)
  iter <- function(pos, acc) {
    if (sum(acc) == goal) list(acc)
    else if (pos > len) NULL
    else if (sum(acc)  > goal) NULL
    else c(iter(pos + 1, c(acc, volumes[pos])), iter(pos + 1, acc))
  }
  iter(pos = 1, acc = integer())
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day17_part1_solution <- function(input, goal = const_EGGNOG_VOLUME) {
  input %>% parse_input() %>% count_all_combinations(goal)
}

exp_test_result <- 4
act_test_result <- day17_part1_solution(test_input, 25)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day17_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day17_part2_solution <- function(input, goal = const_EGGNOG_VOLUME) {
  combinations <-
    input %>%
    parse_input() %>%
    #sort() %>%
    count_all_combinations_part2(goal) %>%
    Filter(f = function(x) !is.null(x))
  min_num <- combinations %>% Map(f = length) %>% Reduce(f = min)
  diff_ways <-
    combinations %>%
    Filter(f = function(x) length(x) == min_num) %>%
    #Reduce(f = function(z, x) {
    #  idx <- x %>% paste(collapse = "_")
    #  z[[idx]] <- 1 + if (is.null(z[[idx]])) 0 else z[[idx]]
    #  z
    #}, init = list()) %>%
    length()
}

exp_test_result <- 2
act_test_result <- day17_part2_solution(test_input, 25)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day17_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
