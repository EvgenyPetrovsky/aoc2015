library(magrittr)

test_input <- c(
  "London to Dublin = 464",
  "London to Belfast = 518",
  "Dublin to Belfast = 141"
)

real_input <- readLines("./inputs/day09-input.txt")

#- PARSE INPUT ----------------------------------------------------------------#

#' what is proper data structure to store the graph?
#' let's start with hash-table
parse_input <- function(input) {
  input %>% 
    Map(f = parse_input_line) %>%
    Reduce(f = function(z, x) {
      z[[x$from]][[x$to]] <- x$distance
      z[[x$to]][[x$from]] <- x$distance
      z
    }, init = list())
}

#' take line of format <from> to <to> = <distance> and parse it into list
parse_input_line <- function(input_line) {
  itinerary_distance <- input_line %>% strsplit(split = " = ") %>% unlist()
  from_to <- itinerary_distance %>% strsplit(split = " to ") %>% unlist()
  list(
    from = from_to[1], to = from_to[2], 
    distance = as.integer(itinerary_distance[2]))
}

#- LOGIC ----------------------------------------------------------------------#

# find all permutations of set x of given length n
permutations <- function(x, n) {
  empty <- list(x[0])
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
  iter(x,n)
}

#' given hash-table of distances (from -> to -> distance)
#' and way points of itinerary
#' calculate total distance
calculate_intinerary_distance <- function(distances, way_points) {
  seq_len(length(way_points)-1) %>%
    Map(f = function(idx) {
      from <- way_points[idx]
      to <- way_points[idx+1]
      distance <- distances[[from]][[to]]
      distance
    }) %>% 
    Reduce(f = sum)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day09_part1_solution <- function(input) {
  distances_tree <- input %>% parse_input()
  
  all_cities <- names(distances_tree)
  
  all_cities %>%
    # build all possible ways to visit all cities
    permutations(length(all_cities)) %>%
    # calculate distances for each way
    Map(f = function(x) calculate_intinerary_distance(distances_tree, x)) %>%
    # chose least
    Reduce(f = min)
}

exp_test_result <- 605
act_test_result <- day09_part1_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part1 <- day09_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day09_part2_solution <- function(input) {
  distances_tree <- input %>% parse_input()
  
  all_cities <- names(distances_tree)
  
  all_cities %>%
    # build all possible ways to visit all cities
    permutations(length(all_cities)) %>%
    # calculate distances for each way
    Map(f = function(x) calculate_intinerary_distance(distances_tree, x)) %>%
    # chose least
    Reduce(f = max)
}

exp_test_result <- 982
act_test_result <- day09_part2_solution(test_input)
print(paste(
  "actual test result:", act_test_result,
  "expected result:", exp_test_result,
  "valid:", act_test_result == exp_test_result))

real_result_part2 <- day09_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))
