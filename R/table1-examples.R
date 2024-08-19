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


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")



# In-class Exercise
# Make a tbl_summary(). Include categorical region, race/ethnicity,
# income, and the sleep variables (use a helper function to select those) and
# make sure they are nicely labeled.


tbl_summary(
	nlsy,
	include = c(sex_cat, race_eth_cat,
							region_cat, income, starts_with("sleep")),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		sleep_wkdy ~ "Sleep Weekdays",
		sleep_wknd ~ "Sleep Weekends",
		income ~ "Income"
	),
	statistic = list(
		variable_name ~ "{stat1 in parents",
		starts_with("sleep") ~ ""
	),
	digits = list(
		variable_name ~ c(5),
		starts_with("sleep") ~ c()
	)






# Create a helper function to select sleep variables
select_sleep_vars <- function(data) {
	data |>
		select(starts_with("sleep"))
}



tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							region_cat, income, starts_with("sleep")),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		sleep_wkdy ~ "Sleep on Weekdays",
		sleep_wknd ~ "Sleep on Weekends",
		income ~ "Income"
	),
	statistic = list(
		income ~ "{p10}; ({p90})",    # 10th and 90th percentiles with 3 digits
		starts_with("sleep") ~ "{min} ({max})"    # Min and max for sleep variables with 1 digit
	),
	digits = list(
		income ~ 3,    # 3 digits for income percentiles
		starts_with("sleep") ~ 1    # 1 digit for sleep min and max
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total** N = {N}") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")



