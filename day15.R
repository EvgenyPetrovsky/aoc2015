library(magrittr)

test_input <- c(
  "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
  "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
)

real_input <- readLines("./inputs/day15-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% 
    Map(f = parse_input_line) %>% 
    magrittr::set_names(Map(., f = function(x) x$ingredient) %>% unlist()) %>%
    Map(f = function(x) x$numbers)
}

parse_input_line <- function(input_line) {
  numbers <- input_line %>% gregexpr(pattern = "-?\\d+") %>% regmatches(x = input_line) %>% unlist()
  ingredient <- input_line %>% gsub(pattern = "^(\\w+): .*$", replacement = "\\1")
  #list(
  #  ingredient = ingredient,
  #  capacity = numbers[1],
  #  durability = numbers[2],
  #  flavor = numbers[3],
  #  texture = numbers[4],
  #  calories = numbers[5])
  list(ingredient = ingredient, numbers = as.integer(numbers))
}

#- LOGIC ----------------------------------------------------------------------#

count_score <- function(ingredients_value, proportions) {
  names(proportions) %>%
    Reduce(
      f = function(total_value, ingredient) {
        value <- proportions[[ingredient]] %o% ingredients_value[[ingredient]]
        total_value + value
      }, 
      init = matrix(
        0, 
        nrow = length(proportions[[1]]),
        ncol = length(ingredients_value[[1]])
      )
    ) %>%
    apply(MARGIN = 1, FUN = function(x) if(any(x < 0)) 0 else prod(x))
}

generate_proportions <- function(ingredients, total_quantity) {
  ingredients %>% 
    Map(f = function(x) 1:total_quantity) %>%
    expand.grid() %>%
    #inset2("total_quantity", rowSums(.)) %>%
    subset(rowSums(.) == total_quantity)
}

count_calories <- function(calories, proportions) {
  names(proportions) %>%
    Reduce(
      f = function(total_value, ingredient) {
        value <- proportions[[ingredient]] * calories[[ingredient]]
        total_value + value
      }, init = 0)
}



#- SOLUTION PART 1 ------------------------------------------------------------#

day15_part1_solution <- function(input) {
  quantity <- 100
  ingredients <- 
    input %>% 
    parse_input() %>%
    #ignore calories
    Map(f = function(x) x[-5])
  ingredients %>% 
    generate_proportions(total_quantity = 100) %>% 
    count_score(ingredients_value = ingredients) %>% 
    max()
}

exp_test_result <- 62842880
act_test_result <- day15_part1_solution(test_input)
print(paste(
  "actual test result:", format(act_test_result, scientific = F),
  "expected result:", format(exp_test_result, scientific = F),
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day15_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day15_part2_solution <- function(input) {
  quantity <- 100
  ingredients <- input %>% parse_input() %>% Map(f = function(x) x[-5])
  calories <- input %>% parse_input() %>% Map(f = function(x) x[5])

  ingredients %>% 
    generate_proportions(total_quantity = 100) %>% 
    subset(count_calories(calories, .) == 500) %>%
    count_score(ingredients_value = ingredients) %>% 
    max()
}

exp_test_result <- 57600000
act_test_result <- day15_part2_solution(test_input)
print(paste(
  "actual test result:", format(act_test_result, scientific = F),
  "expected result:", format(exp_test_result, scientific = F),
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day15_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
