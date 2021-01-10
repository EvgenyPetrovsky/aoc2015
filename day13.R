library(magrittr)

test_input <- c(
  "Alice would gain 54 happiness units by sitting next to Bob.",
  "Alice would lose 79 happiness units by sitting next to Carol.",
  "Alice would lose 2 happiness units by sitting next to David.",
  "Bob would gain 83 happiness units by sitting next to Alice.",
  "Bob would lose 7 happiness units by sitting next to Carol.",
  "Bob would lose 63 happiness units by sitting next to David.",
  "Carol would lose 62 happiness units by sitting next to Alice.",
  "Carol would gain 60 happiness units by sitting next to Bob.",
  "Carol would gain 55 happiness units by sitting next to David.",
  "David would gain 46 happiness units by sitting next to Alice.",
  "David would lose 7 happiness units by sitting next to Bob.",
  "David would gain 41 happiness units by sitting next to Carol."
)

real_input <- readLines("./inputs/day13-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

#' generate hashtable of (person -> person) -> score
parse_input <- function(input) {
  input %>% 
    Map(f = parse_input_line) %>%
    Reduce(f = function(z, x) {
      z[[x$who]][[x$neighbor]] <- x$score
      z
    }, init = list())
}

#' take a text line and parse it into list 
#'  - who is a person that goes first
#'  - neighbor is a person that mentioned last
#'  - score is a gain (positive) or lose (negative) value
parse_input_line <- function(input_line) {
  who <- input_line %>% 
    gregexpr(pattern = "^\\w+") %>% 
    regmatches(x = input_line) %>% 
    unlist()
  score <- input_line %>%
    gregexpr(pattern = "(lose|gain) \\d+") %>%
    regmatches(x = input_line) %>%
    unlist() %>%
    gsub(pattern = "lose ", replacement = "-") %>%
    gsub(pattern = "gain ", replacement = "+") %>%
    as.integer()
  neighbor <- input_line %>% 
    gregexpr(pattern = "\\w+\\.$") %>% 
    regmatches(x = input_line) %>% 
    gsub(pattern = "\\.$", replacement = "") %>%
    unlist()
  list(who = who, score = score, neighbor = neighbor)
}

add_santa <- function(tree) {
  tree %>% 
    Map(f = function(person) person %>% inset("Santa", 0)) %>%
    inset2("Santa", list() %>% inset(names(tree), 0))  
}

#- LOGIC ----------------------------------------------------------------------#

# find all permutations of set x of given length n
permutations <- function(x, n) {
  empty <- integer()
  x_num <- seq_along(x)
  iter <- function(x, n) {
    if (n < 1) empty
    else if (length(x) == 1) x
    else if (length(x) < 1) empty
    else {
      seq_along(x) %>% 
        Map(f = function(idx) {
          iter(x[-idx], n - 1) %>% Map(f = function(p) c(x[idx], p))
        }) %>%
        Reduce(f = c)
    }
  }
  perm <- iter(x_num,n)
  perm %>% Map(f = function(xx) x[xx])
}

person_happiness_level <- function(tree, who, left, right) {
  tree[[who]][[left]] + tree[[who]][[right]]
}

table_happiness_level <- function(tree, names) {
  names %>% 
    seq_along() %>%
    Map(f = function(pos) {
      pos_l <- (pos-2) %% length(names) + 1
      pos_r <- (pos) %% length(names) + 1
      person_happiness_level(tree, names[pos], names[pos_l], names[pos_r])
    }) %>%
    Reduce(f = `+`)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day13_part1_solution <- function(input) {
  tree <- input %>% parse_input()
  names(tree) %>% 
    permutations(length(tree)) %>%
    Map(f = function(x) table_happiness_level(tree, x)) %>%
    Reduce(f = max)
}

exp_test_result <- 330
act_test_result <- day13_part1_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day13_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day13_part2_solution <- function(input) {
  tree <- input %>% parse_input() %>% add_santa()
  names(tree) %>% 
    permutations(length(tree)) %>%
    Map(f = function(x) table_happiness_level(tree, x)) %>%
    Reduce(f = max)

}

exp_test_result <- 330 - (46 - 2)
act_test_result <- day13_part2_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))


real_result_part2 <- day13_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
