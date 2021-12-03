library(tidyverse)

input_path <- "input.txt"

input_table <- tibble(A = read_lines(input_path)) %>%
	separate(A, c("DIRECTION", "MAGNITUDE"), sep = "[[:space:]]+") %>%
	mutate(
		MAGNITUDE = parse_number(MAGNITUDE)
	) 

input_total <- input_table %>%
	group_by(DIRECTION) %>%
	summarise(
		TOTAL = sum(MAGNITUDE, na.rm = TRUE)
	) %>%
	pivot_wider(names_from = "DIRECTION", values_from = "TOTAL") %>%
	mutate(
		FPOS = forward,
		DEPTH = down - up,
		RESULTS = FPOS * DEPTH
	)

print(input_total$RESULTS)
	
input_second <- input_table %>%
	pmap(tibble) %>%
	accumulate(~tibble(
					FPOS = .x$FPOS + if_else(.y$DIRECTION == "forward", .y$MAGNITUDE, 0),
					DEPTH = .x$DEPTH + if_else(.y$DIRECTION == "forward", .x$AIM * .y$MAGNITUDE, 0),
					AIM = .x$AIM + if_else(.y$DIRECTION == "forward", 0, if_else(.y$DIRECTION == "up", -1, 1)) * .y$MAGNITUDE
				),
	.init = tibble(FPOS = 0, DEPTH = 0, AIM = 0)
	)
	
