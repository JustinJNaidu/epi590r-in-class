new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

x <- c(10, 15, 20, 25, 30)

new_mean(x = c(34645, 23423, 5234))
mean(c(346345, 23423, 5234))



prop <- function(x, multiplier = 1) {
	n <- length(x)
	proportion_val <- sum(x) / n
	multiplied_val <- multiplier * proportion_val
	return(multiplied_val)
}

prop(c(1,1,1,0,0))
prop(c(1,1,1,0,0), multiplier = 100)



x <- 3



# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2
square <- function(x) {
	squared_value <- (x^2)
	return(squared_value)
}

square(3)

# test it out
square(x)
square(53)
53^2 # does this match?






raise <- function(x, power) {
	power_value <- (x^power)
	return(power_value)
}

# test with
raise(x = 2, power = 4)
# should give you
2^4



raise <- function(x, multiplier = 2) {
	power_value <- (x^multiplier)
	return(power_value)
}

# test
raise(x = 5)
# should give you
5^2



raise <- function(x, multiplier = 2) {
	power_value <- (x^multiplier)
	return(power_value)
}

# test
raise(x = 5, multiplier = 6)
# should give you
5^6





square <- function(x) {
	squared_value <- x*x
	return(squared_value)
}
square(9)

x <- 3
y <- 4
x^y
3*3*3*3

#base number is x, power is y
raise <- function(base_number, power = 2) {
	answer <- base_number^power
	return(answer)
}

raise(base_number = 2, power = 5)
raise(2,5)




raise <- function(x, multiplier = 2) {
	power_value <- (x^multiplier)
	return(power_value)
}

# test
raise(x = 5)
# should give you
5^2


################################################################################



library(tidyverse)
library(gtsummary)
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))



logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat,
										 data = nlsy, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
												 data = nlsy, family = binomial(link = "log")
)

new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"),
		tidy_fun = partial(tidy_robust, vcov = "HC1")
		)
}

new_table_function(logistic_model)
new_table_function(poisson_model)
new_table_function(logbinomial_model)



