library(magrittr)

test_input <- c("^>v<")

real_input <- readLines("./inputs/day03-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

parse_input <- function(input) {
  input %>% strsplit(split = "") %>% unlist()
}

#- LOGIC ----------------------------------------------------------------------#

starting_addess <- c(0L,0L)

#' state of the game is captured as 
#'  - address list: hashmap of addresses / coordinates and number of visits
#'  - position: current position of santa
init_game_state <- function() {
  list(
    address_list = list(), 
    position = starting_addess)
}

#' to before starting to process any instructions there must be visited position 
#' in the initial state
start_game_state <- function(init_state) {

  # in the beginning, santa visits starting address once
  start_game_state <- 
    log_visit(init_state$address_list, init_state$position) %>% 
    update_game_state(
      state = init_state,
      address_list = .,
      position = init_state$position)
}
#' update current game state using new address list and new position
update_game_state <- function(state, address_list, position) {
  state %>%
    inset2("address_list", address_list) %>%
    inset2("position", position)
}

#' find new position of santa based on current position and 
#' move instruction (>, ^, <, or v)
new_position <- function(position, move) {
  delta_position <- list(
    ">" = c( 1, 0), 
    "^" = c( 0, 1), 
    "<" = c(-1, 0), 
    "v" = c( 0,-1))

  position + delta_position[[move]]
}

#' take address list, visited address, log this visit and return updated address_list
log_visit <- function(address_list, address) {
  address_txt <- paste("(", address[1], ", ", address[2], ")", sep = "")
  nr_former_visits <- 
    if (is.null(address_list[[address_txt]])) 0 
    else address_list[[address_txt]]

  address_list %>%
    magrittr::inset2(address_txt, nr_former_visits + 1)
}

deliver_gifs <- function(state, move_instructions) {
  move_instructions %>%
    Reduce(f = function(state, move) {
      position <- new_position(state$position, move)
      address_list <- log_visit(state$address_list, position)
      update_game_state(state, address_list, position)
    }, init = state)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day03_part1_solution <- function(input) {
  instructions <- input %>% parse_input()

  game_state <- 
    init_game_state() %>%
    # deliver gifts to starting position
    start_game_state() %>%
    # deliver all gifts
    deliver_gifs(instructions)

  # count number of addresses in a list
  length(game_state$address_list)

}

test_output_part1 <- 2
test_result <- day03_part1_solution(">")
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

test_output_part1 <- 4
test_result <- day03_part1_solution("^>v<")
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

test_output_part1 <- 2
test_result <- day03_part1_solution("^v^v^v^v^v")
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))


real_result_part1 <- day03_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day03_part2_solution <- function(input) {
  instructions <- input %>% parse_input()
  instructions_split <- seq_along(instructions) %% 2 == 1
  instructions_for_santa <- instructions[instructions_split == TRUE]
  instructions_for_robot <- instructions[instructions_split == FALSE]
  
  # deliver all gifts
  game_state <- 
    init_game_state() %>%
    # deliver gifts to starting position
    start_game_state() %>%
    # santa delivers gifts
    deliver_gifs(instructions_for_santa) %>%
    # reset starting position to process input instructions of robot
    update_game_state(
      state = ., 
      address_list = .$address_list, 
      position = starting_addess
    ) %>%
    # deliver gifts to starting position
    start_game_state() %>%
    # robot delivers gifts
    deliver_gifs(instructions_for_robot)

  # count number of addresses in a list
  length(game_state$address_list)
}

test_output_part2 <- 3
test_result <- day03_part2_solution("^v")
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

test_output_part2 <- 3
test_result <- day03_part2_solution("^>v<")
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

test_output_part2 <- 11
test_result <- day03_part2_solution("^v^v^v^v^v")
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day03_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
