library(magrittr)

test_input <- c(
"Sue 1: goldfish: 9, cars: 0, samoyeds: 9",
"Sue 2: perfumes: 5, trees: 8, goldfish: 8",
"Sue 3: pomeranians: 2, akitas: 1, trees: 5",
"Sue 555: children: 3, cats: 7, samoyeds: 2, pomeranians: 3, goldfish: 5"
)

real_input <- readLines("./inputs/day16-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>%
    Map(f = parse_line) %>%
    # lift up numbers into names of list elements
    magrittr::set_names(Map(., f = function(x) as.character(x$number)) %>% unlist()) %>%
    Map(f = function(x) x$number <- NULL)
}

parse_line <- function(input_line) {
  number <- gsub("^Sue (\\d+)", "\\1", input_line)
  l <- const_ATTR_LIST %>%
    Map(f = function(attr) input_line %>% extract_attribute(attr)) %>%
    inset2("number", number)
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
  children = 3L,
  cats = 7L,
  samoyeds = 2L,
  pomeranians = 3L,
  akitas = 0L,
  vizslas = 0L,
  goldfish = 5L,
  trees = 3L,
  cars = 2L,
  perfumes = 1L
)

extract_attribute <- function(text, attribute) {
  gsub(paste0(".* ", attribute, ": (\\d+).*"), "\\1", text) %>% as.integer()
}

aunt_conforms <- function(aunt_attributes, criteria) {
  names(aunt_attributes) %>%
    Map(f = function(attr) aunt_attributes[[attr]] == criteria[[attr]]) %>%
    Reduce(f = all)
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
  NULL
}

exp_test_result <- -1
act_test_result <- day16_part2_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day16_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
