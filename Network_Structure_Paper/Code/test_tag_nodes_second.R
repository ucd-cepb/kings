# Test script for tag_nodes_second function
# This script tests the AI-powered node tagging function

# Load required libraries
library(tidyverse)
library(ellmer)

# Load the main script (assuming it's in the same directory)
source("Network_Structure_Paper/Code/1a_generate_networks_functions.R")

nantbt <- read_csv('Network_Structure_Paper/Out/na_nodes_to_be_tagged.csv')

test_nl <- slice_sample(nantbt, n=20) %>% 
   mutate(entity_type = NA_character_) %>% 
   rename(entity_name = name)

tu_before <- token_usage()


# Test the function with a small batch
cat("Testing tag_nodes_second function...\n")
cat("Input nodelist:\n")
print(test_nl)

result <- tag_nodes_second(test_nl, gsp_id = "test_001", batch_size = 10)

cat("\nOutput nodelist:\n")
print(result)

# Check how many entities were tagged
untagged_before <- sum(is.na(test_nl$entity_type))
untagged_after <- sum(is.na(result$entity_type))

cat(sprintf("\nUntagged entities: %d -> %d\n", untagged_before, untagged_after)) 

tu_after <- token_usage()

print(rbind(tu_before, tu_after))
