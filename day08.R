library(magrittr)

test_input <- c(
  "\"\"",
  "\"abc\"",
  "\"aaa\\\"aaa\"",
  "\"\\x27\""
)

real_input <- readLines("./inputs/day08-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input
}

#- LOGIC ----------------------------------------------------------------------#

total_length <- function(x) {
  x %>% Map(f = nchar) %>% Reduce(f = `+`)
}

eval_text <- function(text) eval(parse(text = text))

serialize_text <- function(text) capture.output({text}) %>% substring(5)

#- SOLUTION PART 1 ------------------------------------------------------------#

day08_part1_solution <- function(input) {
  txt_lines <- 
    input %>%
    parse_input()
  
  text_len <- txt_lines %>% total_length()
  mem_len  <- txt_lines %>% Map(f = eval_text) %>% total_length()
  
  text_len - mem_len
}

test_output_part1 <- (2 + 5 + 10 + 6) - (0 + 3 + 7 + 1)
test_result <- day08_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day08_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day08_part2_solution <- function(input) {
  txt_lines <- 
    input %>%
    parse_input()
  
  text_len <- txt_lines %>% total_length()
  srlz_len <- txt_lines %>% Map(f = serialize_text) %>% total_length()
  
  srlz_len - text_len
}

test_output_part2 <-  (6 + 9 + 16 + 11) - (2 + 5 + 10 + 6)
test_result <- day08_part2_solution(test_input)
print(paste(
  "actual test result:", test_result,
  "expected test result:", test_output_part2,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day08_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
