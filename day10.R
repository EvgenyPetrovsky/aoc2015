library(magrittr)

test_input <- "1"

real_input <- readLines("./inputs/day10-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% strsplit(split = "") %>% unlist()
}

#- LOGIC ----------------------------------------------------------------------#

divide_into_chunks <- function(digits) {
  len <- length(digits)
  levels <-
    if (len <= 1) 1
    else {
      inc <- (digits[-1] != digits[-len]) + 0L
      Reduce(inc, f = `+`, init = 1, accumulate = T)
    }
  split(digits, levels)
}

make_new_number <- function(chunks) {
  chunks %>%
    Map(f = function(x) c(length(x), x[1])) %>%
    Reduce(f = c)
}

make_new_number_opt <- function(digits) {
  len <- length(digits)
  idx <- which(c(TRUE, digits[-1] != digits[-len], TRUE))
  # frequency of digit
  nns <- idx[-1] - idx[-length(idx)]
  # digit itself (last element of idx is length(digits) + 1)
  dig <- digits[idx[-length(idx)]]

  # to quickly concatenate vectors - put them into matrix and convert to vector
  mx <- matrix(c(nns, dig), nrow = 2, byrow = T)
  as.vector(mx)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day10_part1_solution <- function(input, n) {
  1:n %>%
    Reduce(f = function(z, x) {
      val <- z %>%
        as.character() %>%
        divide_into_chunks() %>%
        make_new_number()
      print(paste(
        "time:", Sys.time(),
        "step:", format(x, width = 2),
        "size:", length(val)))
      #print(paste("result:", val))
      val
    }, init = parse_input(input))
}

exp_test_result <- "312211"
act_test_result <-
  day10_part1_solution(test_input, 5) %>%
  paste(collapse = "")
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day10_part1_solution(real_input, 40)
print(format(length(real_result_part1), scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day10_part2_solution <- function(input, steps) {
  iter <- function(x, n) {
    if (n <= 0) return(x)
    val <- make_new_number_opt(x)
    print(paste(
      "time:", Sys.time(),
      "step:", format(steps - n + 1, width = 2),
      "size:", length(val)))
    #print(paste("result:", val))
    iter(val, n - 1)
  }
  iter(parse_input(input), steps)
}

exp_test_result <- "312211"
act_test_result <-
  day10_part2_solution(test_input, 5) %>%
  paste(collapse = "")
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day10_part2_solution(real_input, 50)
print(format(length(real_result_part2), scientific = FALSE))
