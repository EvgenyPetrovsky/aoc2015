library(magrittr)

test_input <- c(
  "123 -> x",
  "456 -> y",
  "x AND y -> d",
  "x OR y -> e",
  "x LSHIFT 2 -> f",
  "y RSHIFT 2 -> g",
  "NOT x -> h",
  "NOT y -> i"
)

real_input <- readLines("./inputs/day07-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% Map(f = parse_line)
}

parse_line <- function(text_line) {
  lhs <- text_line %>% strsplit(split = " -> ") %>% extract2(1) %>% extract(1)
  rhs <- text_line %>% strsplit(split = " -> ") %>% extract2(1) %>% extract(2)
  op <- "[A-Z]+" %>% gregexpr(lhs) %>% regmatches(x = lhs) %>% extract2(1) %>% extract(1)
  i1 <- "[0-9a-z]+" %>% gregexpr(lhs) %>% regmatches(x = lhs) %>% extract2(1) %>% extract(1)
  i2 <- "[0-9a-z]+" %>% gregexpr(lhs) %>% regmatches(x = lhs) %>% extract2(1) %>% extract(2)
  o <- rhs
  list(op = op, i1 = i1, i2 = i2, o = o)
}

#make_expression <- function(el) {
#  prepare_input <- function(i) if (operand_is_literal(i)) i else paste0("f_", i, "()")
#  if (is.na(el$op)) {
#    paste0(el$o, " <- function() ", prepare_input(i1))
#  } else {
#    paste0(
#      "f_", el$o, " <- function() ", 
#      prepare_input(el$i1), 
#      " %", el$op, "% ",
#      prepare_input(el$i2))
#  }
#}


#- LOGIC ----------------------------------------------------------------------#
cache <- list()

fun_NOT <- function(a, b) bitwNot(a) %% 2^16
fun_AND <- function(a, b) bitwAnd(a, b) %% 2^16
fun_OR  <- function(a, b) bitwOr(a, b) %% 2^16
fun_LSHIFT <- function(a, b) bitwShiftL(a, b) %% 2^16
fun_RSHIFT <- function(a, b) bitwShiftR(a, b) %% 2^16

operand_is_literal <- function(operand) is.na(operand) | grepl("^\\d+$", operand)

build_tree <- function(tree, el) {
  tree %>% inset2(el$o, el)
}

evaluate_tree_element <- function(tree, element) {
  sub_tree <- tree[[element]]
  if (is.null(sub_tree)) 
    stop(paste("tree element", element, "doesn't exist in a tree"))
  
  # if element was calculated before - take it out of cache
  if (!is.null(cache[[element]])) return(cache[[element]])
  
  print(paste(sub_tree$o, "<-", sub_tree$op, sub_tree$i1, sub_tree$i2))
  fun_mapping <- list(
    NOT = fun_NOT, AND = fun_AND, OR = fun_OR, 
    LSHIFT = fun_LSHIFT, RSHIFT = fun_RSHIFT
  )
  
  eval_input <- function(i)
    if (operand_is_literal(i)) as.integer(i)
    else evaluate_tree_element(tree, i)
  operand_1 <- eval_input(sub_tree$i1)
  operand_2 <- eval_input(sub_tree$i2)
  
  val <- if (is.na(sub_tree$op)) operand_1
  else fun_mapping[[sub_tree$op]](operand_1, operand_2)
  
  # store result into cache
  cache[[element]] <<- val
  
  val
}


#- SOLUTION PART 1 ------------------------------------------------------------#

day07_part1_solution <- function(input, what = "a") {
  cache <<- list()
  input %>% 
    parse_input() %>% 
    Reduce(f = build_tree, init = list()) %>%
    evaluate_tree_element(what)
}

test_output_part1 <- 507
test_result <- day07_part1_solution(test_input, "e")
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day07_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day07_part2_solution <- function(input) {
  cache <<- list()
  tree <- input %>% 
    parse_input() %>% 
    Reduce(f = build_tree, init = list())
  a <- tree %>%
    evaluate_tree_element("a")
  
  # the signal you got on wire a, override wire b to that signal
  new_tree <- build_tree(tree, list(o = "b", op = NA, i1 = a, i2 = NA))
  # reset the other wires (including wire a)
  cache <<- list()
  new_tree %>% evaluate_tree_element("a")
}

real_result_part2 <- day07_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
