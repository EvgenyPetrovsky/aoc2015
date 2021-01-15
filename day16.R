library(magrittr)

test_input <- c(
  "Sue 1: goldfish: 9, cars: 0, samoyeds: 9",
  "Sue 2: perfumes: 5, trees: 8, goldfish: 8",
  "Sue 3: pomeranians: 2, akitas: 1, trees: 5",
  "Sue 777: children: 3, cats: 8, samoyeds: 2, pomeranians: 2, goldfish: 4",
  "Sue 555: children: 3, cats: 7, samoyeds: 2, pomeranians: 3, goldfish: 5"
)

real_input <- readLines("./inputs/day16-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>%
    Map(f = parse_input_line) %>%
    # lift up numbers into names of list elements
    magrittr::set_names(Map(., f = function(x) as.character(x$number)) %>% unlist()) %>%
    # remove number element from attributes of elements of list
    Map(f = function(x) inset(x, "number", NULL))
}

parse_input_line <- function(input_line) {
  number <- gsub("^Sue (\\d+).*", "\\1", input_line)
  const_ATTR_LIST %>%
    Map(f = function(attr) input_line %>% extract_attribute(attr)) %>%
    Filter(f = function(attr) length(attr) != 0) %>%
    inset2("number", number)
}

extract_attribute <- function(text, attribute) {
  text %>%
    gregexpr(pattern = paste0(attribute, ": (\\d+)")) %>%
    regmatches(x = text) %>%
    unlist() %>%
    gsub(pattern = "\\w+: (\\d+)$", replacement = "\\1") %>%
    as.integer()
}

#- LOGIC ----------------------------------------------------------------------#


const_ATTR_LIST <- c(
  "children",
  "cats",
  "samoyeds",
  "pomeranians",
  "akitas",
  "vizslas",
  "goldfish",
  "trees",
  "cars",
  "perfumes"
)

const_CRITERIA <- list(
  children = function(x) x == 3L,
  cats = function(x) x == 7L,
  samoyeds = function(x) x == 2L,
  pomeranians = function(x) x == 3L,
  akitas = function(x) x == 0L,
  vizslas = function(x) x == 0L,
  goldfish = function(x) x == 5L,
  trees = function(x) x == 3L,
  cars = function(x) x == 2L,
  perfumes = function(x) x == 1L
)

const_CRITERIA_part2 <- list(
  children = function(x) x == 3L,
  cats = function(x) x > 7L,
  samoyeds = function(x) x == 2L,
  pomeranians = function(x) x < 3L,
  akitas = function(x) x == 0L,
  vizslas = function(x) x == 0L,
  goldfish = function(x) x < 5L,
  trees = function(x) x > 3L,
  cars = function(x) x == 2L,
  perfumes = function(x) x == 1L
)

aunt_conforms <- function(aunt_attributes, criteria) {
  check_fun <- function(x) aunt_attributes[[x]] %>% criteria[[x]]()
  names(aunt_attributes) %>% Map(f = check_fun) %>% Reduce(f = all)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day16_part1_solution <- function(input) {
  filter_fun <- function(x) x %>% aunt_conforms(criteria = const_CRITERIA)
  input %>% parse_input() %>% Filter(f = filter_fun) %>% names() %>% as.integer()
}

exp_test_result <- 555
act_test_result <- day16_part1_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day16_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day16_part2_solution <- function(input) {
  filter_fun <- function(x) x %>% aunt_conforms(criteria = const_CRITERIA_part2)
  input %>% parse_input() %>% Filter(f = filter_fun) %>% names() %>% as.integer()
}

exp_test_result <- 777
act_test_result <- day16_part2_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day16_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
