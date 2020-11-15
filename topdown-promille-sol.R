# Goal: calculate a measure of blood alcohol concentration
# source: https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/

# Inputs:
# age in years
# sex = c("male", "female"),
# height in cm
# weight in kg
# drinking_time: start and end time
# drinks: 4 types are accepted -  “massn”, “hoibe”, “wein” und “schnaps”
# and the amount of each drink

# Output:
# bloog concentration measurement aftwr Whatson

tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight, drinking_time, drinks) {
  sex <- match.arg(tolower(sex), c("male", "female"))

  checkmate::assert_int(age, lower = 1, upper = 150)
  checkmate::assert_numeric(height, lower = 1, finite = TRUE)
  checkmate::assert_numeric(weight, lower = 1, finite = TRUE)
  checkmate::assert(checkmate::check_list(drinks), checkmate::check_atomic_vector(drinks), combine = "or")
  checkmate::assert_posixct(drinking_time)
  checkmate::assert_names(names(drinks), subset.of = c("massn", "hoibe", "wein", "schnaps"))

  if (diff(drinking_time) < 0) {
    stop("start time lies after the end time")
  }

  if (age < 16 | (age >= 16 & age < 18 & any(names(drinks) == "schnaps"))) {
    warning("Illegal use of alcohol.")
  }

  drinks <- as.list(drinks)

  alcohol_mass <- 0
  for (drink in names(drinks)) {
    add_alcohol_mass <- get_alcohol_mass(type = drink, count = drinks[[drink]])
    alcohol_mass <- alcohol_mass + add_alcohol_mass
  }

  avg_blood_density <- 1.055

  total_body_water <- get_total_body_water(sex, age, height, weight)

  kombination_WW <- (0.8 * alcohol_mass) / (avg_blood_density * total_body_water)

  kombination_WW_final <- kombination_WW - 0.15 * (as.numeric(diff(drinking_time)) - 1)
  kombination_WW_final

  # print(paste0("The value of Combination after Widmark and Whatson for ", drink, " is: ", kombination_WW_final))
}



# Used alcohol mass ({A})
get_alcohol_mass <- function(type = c("massn", "hoibe", "wein", "schnaps"), count) {
  checkmate::assert_int(count, lower = 1)

  if (type == "massn") {
    portion <- 1000
    volume_percentage <- 0.06
  }

  if (type == "hoibe") {
    portion <- 500
    volume_percentage <- 0.06
  }

  if (type == "wein") {
    portion <- 200
    volume_percentage <- 0.11
  }

  if (type == "schnaps") {
    portion <- 40
    volume_percentage <- 0.4
  }

  alcohol_density <- 0.8
  volume_drink <- portion * count * volume_percentage * alcohol_density
  volume_drink
}

# total body water ({GKW}) after Whatson
get_total_body_water <- function(sex, age, height, weight) {
  if (sex == "male") {
    value <- 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight
    return(value)
  }

  value <- 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  value
}


# Test --------------------------------------------------------------------

test_file("topdown-promille-tests.R")

tell_me_how_drunk(
  age = 18,
  sex = "male",
  height = 180,
  weight = 80,
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = list("wein" = 2, "schnaps" = 3)
)
