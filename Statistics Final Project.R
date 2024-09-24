library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

all_files <- list.files("C:/Statistics/", "pbp-", all.files = TRUE, 
                        full.names = TRUE)

all_read <- lapply(all_files, function(x) {
  output <- read.csv(x)
  year <- stringr::str_extract(x, "[0-9]{4}")
  output$year <- year
  output
})

complete_stats <- do.call(rbind, all_read)

nfl2023 = read.csv("c:/Statistics/pbp-2023.csv")

nfl2022 = read.csv("c:/Statistics/pbp-2022.csv")

nfl2021 = read.csv("c:/Statistics/pbp-2021.csv")

nfl2020 = read.csv("c:/Statistics/pbp-2020.csv")

nfl2019 = read.csv("c:/Statistics/pbp-2019.csv")


nfl = complete_stats%>%
  filter(PlayType == "EXTRA POINT"| PlayType == "TWO-POINT CONVERSION")

glimpse(nfl)

View(nfl)

nfl_extra_point = nfl%>%
  filter(PlayType == "EXTRA POINT")

nfl_two_point = nfl%>%
  filter(PlayType == "TWO-POINT CONVERSION")

extra_point_outcome = ifelse(
  stringr::str_detect(nfl_extra_point$Description, "IS GOOD"),
  1,
  0
)

summary(extra_point_outcome)

nfl_two_point$two_point_outcome = ifelse(
  stringr::str_detect(nfl_two_point$Description, "ATTEMPT SUCCEEDS"),
  1,
  0
)

nfl_extra_point$outcome = ifelse(
  stringr::str_detect(nfl_extra_point$Description, "IS GOOD"),
  1,
  0
)

prop.table(table(nfl_extra_point$outcome))

prop.table(table(nfl_two_point$PlayType, 
                 nfl_two_point$IsTwoPointConversionSuccessful), margin = 1)

#Expected Value of a Two Point Conversion: 
(.5008319*2)
#  1.001664

#Expected Value of an Extra Point Attempt
(.93866274*1)
#  .9386627

#How much does yardage impact extra points?

extra_point_model_basic = lm(nfl_extra_point$outcome~ YardLineFixed, 
                         data = nfl_extra_point)

summary(extra_point_model_basic)

#For every additional yard back, your odds of making an extra point attempt decrease by
#2.88%

extra_point_glm = glm(extra_point_outcome~ YardLineFixed, data = nfl_extra_point, 
                      family = binomial)

summary(extra_point_glm)

exp(extra_point_glm$coefficients)

margins::margins_summary(extra_point_glm)

library(sjPlot)

sjPlot::plot_model(extra_point_glm, type = "pred")

#How does yardage impact your ability to successfully make a 2 point conversion 

two_point_model_basic = lm(nfl_two_point$IsTwoPointConversionSuccessful~ YardLineFixed, 
                         data = nfl_two_point)
summary(two_point_model_basic)

#For every yard additional yard back, 
#the odds of successfully making a two point conversion decrease by 3.14%

two_point_glm = glm(nfl_two_point$IsTwoPointConversionSuccessful~ YardLineFixed, 
                    data = nfl_two_point, family = binomial)

summary(two_point_glm)

exp(two_point_glm$coefficients)

margins::margins_summary(two_point_glm)
#For every yard back your odds of converting a two point attempt decrease by 


two_point_model = lmer(nfl_two_point$IsTwoPointConversionSuccessful ~ 
                         nfl_two_point$YardLineFixed + (1 | YardLineFixed), 
                       data = nfl_two_point)

summary(two_point_model)


