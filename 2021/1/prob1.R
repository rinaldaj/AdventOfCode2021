library(stringr)
library(tidyverse)

input_path <- "input.txt"

input_raw <- read_file(input_path)

input_proc <- str_split(input_raw, pattern = "\n") %>%
	flatten_chr() %>%
	parse_number() %>%
	compact() 

input_table <- tibble(A = input_proc, B = lead(input_proc), C = lead(input_proc, 2)) %>%
	filter(!is.na(C)) %>%
	pmap_dbl(sum, na.rm = TRUE)

input_counted <- input_table %>%
	map2_lgl(lag(.), ~.x > .y) %>%
	sum(na.rm = TRUE)
print(input_counted)
