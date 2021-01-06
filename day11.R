library(magrittr)

test_input <- c(
  "ghijklmn"
)

real_input <- readLines("./inputs/day11-input.txt")

#- CONSTANTS ------------------------------------------------------------------#

not_allowed_letter_indices <- which(letters %in% c("i", "l", "o"))

#- PARSE INPUT ----------------------------------------------------------------#

#' convert text into numeric vector
parse_input <- function(input) {
  input %>% 
    strsplit(split = "") %>%
    unlist() %>%
    Map(f = function(x) which(letters == x)) %>%
    Reduce(f = c)
}

# convert numeric vector to text
password_to_text <- function(x) letters[x] %>% paste(collapse = "")

#- LOGIC ----------------------------------------------------------------------#

next_password <- function(x) {
  d <- length(letters)
  l <- length(x)
  iter <- function(x, p) {
    if (p < 1) stop("password out of bounds")
    else {
      new_x <- x %>% inset(p, x[p] %% d + 1)
      if (new_x[p] == 1) iter(new_x, p - 1)
      else new_x
    }
  }
  iter(x, l)
}

is_valid_password <- function(x) {
  l <- length(x)
  c1 <- 
    3:l %>% 
    Reduce(f = function(z, p) {
      z | all(x[(p-2):p] - x[p-2] == c(0, 1, 2))
    }, init = FALSE)
  c2 <- all((x %in% not_allowed_letter_indices) == FALSE)
  c3 <- grepl(pattern = "(.)\\1.*(.)\\2", password_to_text(x))
  
  all(c1, c2, c3)
}

next_valid_password <- function(x) {
  i <- 0
  x <- next_password(x)
  while (!is_valid_password(x)) {
    x <- next_password(x)
    i <- i + 1
    if (i %% 10000 == 0) 
      print(paste(
        "time:", Sys.time(),
        "iteration:", format(i, width = 7)))
  }
  x
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day11_part1_solution <- function(input) {
  input %>% parse_input() %>% next_valid_password() %>% password_to_text()
}

exp_test_result <- "ghjaabcc"
act_test_result <- day11_part1_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day11_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day11_part2_solution <- day11_part1_solution

real_result_part2 <- day11_part2_solution(real_result_part1)
print(format(real_result_part2, scientific = FALSE))
