Table
================

# Load packages

``` r
library(tidyverse)
library(ggplot2)
library(shiny)
library(infer)
```

# Load data

``` r
pre_empower <- read.csv("data/PRE_Empowerment.csv",
                    sep = ";")
post_empower <- read.csv("data/POST_Empowerment.csv",
                    sep = ";")
pre_2019 <- read.csv("data/PRE2019.csv",
                    sep = ";")
post_2019 <- read.csv("data/POST2019.csv",
                    sep = ";")
```

## Include columns that indicate whether responses are from pre- or post-implementation surveys, and what year they were collected

``` r
pre_empower <- pre_empower %>%
  mutate(pre_or_post = "Pre") %>%
  mutate(year = "2018")
post_empower <- post_empower %>%
  mutate(pre_or_post = "Post") %>%
  mutate(year = "2018")
pre_2019 <- pre_2019 %>%
  mutate(pre_or_post = "Pre") %>%
  mutate(year = "2019")
post_2019 <- post_2019 %>%
  mutate(pre_or_post = "Post") %>%
  mutate(year = "2019")
```

## Count how many students took the pre- and post-implementation surveys in 2018 and 2019

``` r
count(pre_empower)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    79

``` r
count(post_empower)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    64

``` r
count(pre_2019)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    39

``` r
count(post_2019)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    39

``` r
pre_empower %>%
  group_by(grade) %>%
  count
```

    ## # A tibble: 5 x 2
    ## # Groups:   grade [5]
    ##   grade      n
    ##   <fct>  <int>
    ## 1 1         14
    ## 2 2         11
    ## 3 3         19
    ## 4 5 bach    18
    ## 5 5 mag     17

``` r
post_empower %>%
  group_by(grade) %>%
  count
```

    ## # A tibble: 5 x 2
    ## # Groups:   grade [5]
    ##   grade      n
    ##   <fct>  <int>
    ## 1 1          9
    ## 2 2         10
    ## 3 3         16
    ## 4 5 bach    18
    ## 5 5 mag     11

``` r
pre_2019 %>%
  group_by(grade) %>%
  count
```

    ## # A tibble: 4 x 2
    ## # Groups:   grade [4]
    ##   grade     n
    ##   <int> <int>
    ## 1     1     7
    ## 2     2    11
    ## 3     3    20
    ## 4    NA     1

``` r
post_2019 %>%
  group_by(grade) %>%
  count
```

    ## # A tibble: 4 x 2
    ## # Groups:   grade [4]
    ##   grade     n
    ##   <int> <int>
    ## 1     1     7
    ## 2     2    11
    ## 3     3    20
    ## 4    NA     1

## Join pre- and post-implementation data into one dataset. Then join data from 2018 and 2019 into one dataset

``` r
empower_2018 <- full_join(pre_empower, post_empower) %>%
  select(-c(other_plan_afterschool))
```

    ## Joining, by = c("grade", "city_birth", "parents_stem", "which_parent_stem", "parent_profession", "number_science_classes", "number_math_classes", "number_tech_classes", "number_egr_classes", "math_worst", "math_career", "not_enjoy_math", "good_at_math", "decentschool_badmath", "higherlevel_math", "good_math_grades", "interesting_math", "future_math", "math_courses", "sure_science", "science_career", "science_outsideofschool", "science_pay", "science_job", "good_science", "decentschool_badscience", "higherlevel_science", "science_courses", "new_products", "engineering_everyday", "enjoy_building", "interested_machines", "career_design", "curiosity_tech", "future_innovation", "mathscience_useful", "success_engineering", "i_can_build", "opportunity_engineering", "prediction_literature", "prediction_math", "prediction_science", "future_math_classes", "future_science_classes", "future_egr_classes", "college", "what_major", "why_not_college", "plan_after_school", "other_plan_afterschool", "frequency_learning_stem", "why_no_chances", "opportunities_women_stem", "reason_opportunities_stem", "contribute_community", "resolve_problems_community", "external_help", "leader_community", "contribute_community.1", "pre_or_post", "year")

    ## Warning: Column `parent_profession` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `math_courses` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `science_job` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `good_science` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `new_products` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `engineering_everyday` joining factors with different
    ## levels, coercing to character vector

    ## Warning: Column `enjoy_building` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `curiosity_tech` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `future_innovation` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `mathscience_useful` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `prediction_math` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `college` joining factors with different levels, coercing
    ## to character vector

    ## Warning: Column `what_major` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `why_not_college` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `plan_after_school` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `other_plan_afterschool` joining factors with different
    ## levels, coercing to character vector

    ## Warning: Column `why_no_chances` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `opportunities_women_stem` joining factors with different
    ## levels, coercing to character vector

    ## Warning: Column `reason_opportunities_stem` joining factors with different
    ## levels, coercing to character vector

    ## Warning: Column `resolve_problems_community` joining factors with different
    ## levels, coercing to character vector

    ## Warning: Column `leader_community` joining factors with different levels,
    ## coercing to character vector

``` r
empower_2019 <- full_join(pre_2019, post_2019) %>%
  select(-c(other_plan_afterschool))
```

    ## Joining, by = c("grade", "city_birth", "parents_stem", "which_parent_stem", "parent_profession", "number_science_classes", "number_math_classes", "number_tech_classes", "number_egr_classes", "math_worst", "math_career", "not_enjoy_math", "good_at_math", "decentschool_badmath", "higherlevel_math", "good_math_grades", "interesting_math", "future_math", "math_courses", "sure_science", "science_career", "science_outsideofschool", "science_pay", "science_job", "good_science", "decentschool_badscience", "higherlevel_science", "science_courses", "new_products", "engineering_everyday", "enjoy_building", "interested_machines", "career_design", "curiosity_tech", "future_innovation", "mathscience_useful", "success_engineering", "i_can_build", "opportunity_engineering", "prediction_literature", "prediction_math", "prediction_science", "future_math_classes", "future_science_classes", "future_egr_classes", "college", "what_major", "why_not_college", "plan_after_school", "other_plan_afterschool", "frequency_learning_stem", "why_no_chances", "opportunities_women_stem", "reason_opportunities_stem", "contribute_community", "resolve_problems_community", "external_help", "leader_community", "contribute_community.1", "pre_or_post", "year")

``` r
empower_2019$grade <- as.factor(empower_2019$grade)
empower <- full_join(empower_2018, empower_2019)
```

    ## Joining, by = c("grade", "city_birth", "parents_stem", "which_parent_stem", "parent_profession", "number_science_classes", "number_math_classes", "number_tech_classes", "number_egr_classes", "math_worst", "math_career", "not_enjoy_math", "good_at_math", "decentschool_badmath", "higherlevel_math", "good_math_grades", "interesting_math", "future_math", "math_courses", "sure_science", "science_career", "science_outsideofschool", "science_pay", "science_job", "good_science", "decentschool_badscience", "higherlevel_science", "science_courses", "new_products", "engineering_everyday", "enjoy_building", "interested_machines", "career_design", "curiosity_tech", "future_innovation", "mathscience_useful", "success_engineering", "i_can_build", "opportunity_engineering", "prediction_literature", "prediction_math", "prediction_science", "future_math_classes", "future_science_classes", "future_egr_classes", "college", "what_major", "why_not_college", "plan_after_school", "frequency_learning_stem", "why_no_chances", "opportunities_women_stem", "reason_opportunities_stem", "contribute_community", "resolve_problems_community", "external_help", "leader_community", "contribute_community.1", "pre_or_post", "year")

    ## Warning: Column `grade` joining factors with different levels, coercing to
    ## character vector

    ## Warning: Column `city_birth` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `which_parent_stem` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `parent_profession` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `math_courses` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `science_job` joining character vector and factor, coercing
    ## into character vector

    ## Warning: Column `good_science` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `new_products` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `engineering_everyday` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `enjoy_building` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `curiosity_tech` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `future_innovation` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `mathscience_useful` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `prediction_literature` joining factors with different
    ## levels, coercing to character vector

    ## Warning: Column `prediction_math` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `future_math_classes` joining factors with different
    ## levels, coercing to character vector

    ## Warning: Column `future_science_classes` joining factors with different
    ## levels, coercing to character vector

    ## Warning: Column `future_egr_classes` joining factors with different levels,
    ## coercing to character vector

    ## Warning: Column `college` joining character vector and factor, coercing
    ## into character vector

    ## Warning: Column `what_major` joining character vector and factor, coercing
    ## into character vector

    ## Warning: Column `why_not_college` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `plan_after_school` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `why_no_chances` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `opportunities_women_stem` joining character vector and
    ## factor, coercing into character vector

    ## Warning: Column `reason_opportunities_stem` joining character vector and
    ## factor, coercing into character vector

    ## Warning: Column `resolve_problems_community` joining character vector and
    ## factor, coercing into character vector

    ## Warning: Column `leader_community` joining character vector and factor,
    ## coercing into character vector

## Rename to positive vs. negative feelings from A, B, C, D, E (as they were on the survey) to 1, 2, 3, 4, 5

``` r
change_names <- function(naming){
naming %>%
  str_replace("A", "1") %>%
  str_replace("B", "2") %>%
  str_replace("C", "3") %>%
  str_replace("D", "4") %>%
  str_replace("E", "5")
}

empower$math_worst <- change_names(empower$math_worst)
empower$math_career <- change_names(empower$math_career)
empower$not_enjoy_math <- change_names(empower$not_enjoy_math)
empower$good_at_math <- change_names(empower$good_at_math)
empower$decentschool_badmath <- change_names(empower$decentschool_badmath)
empower$higherlevel_math <- change_names(empower$higherlevel_math)
empower$good_math_grades <- change_names(empower$good_math_grades)
empower$interesting_math <- change_names(empower$interesting_math)
empower$future_math <- change_names(empower$future_math)
empower$math_courses <- change_names(empower$math_courses)
empower$sure_science <- change_names(empower$sure_science)
empower$science_career <- change_names(empower$science_career)
empower$science_outsideofschool <- change_names(empower$science_outsideofschool)
empower$science_pay <- change_names(empower$science_pay)
empower$science_job <- change_names(empower$science_job)
empower$good_science <- change_names(empower$good_science)
empower$decentschool_badscience <- change_names(empower$decentschool_badscience)
empower$higherlevel_science <- change_names(empower$higherlevel_science)
empower$science_courses <- change_names(empower$science_courses)
empower$new_products <- change_names(empower$new_products)
empower$engineering_everyday <- change_names(empower$engineering_everyday)
empower$enjoy_building <- change_names(empower$enjoy_building)
empower$interested_machines <- change_names(empower$interested_machines)
empower$career_design <- change_names(empower$career_design)
empower$curiosity_tech <- change_names(empower$curiosity_tech)
empower$future_innovation <- change_names(empower$future_innovation)
empower$mathscience_useful <- change_names(empower$mathscience_useful)
empower$success_engineering <- change_names(empower$success_engineering)
empower$i_can_build <- change_names(empower$i_can_build)
empower$opportunity_engineering <- change_names(empower$opportunity_engineering)
```

## Create different datasets for each question that will be analyzed. Each dataset should have the variables: “pre\_or\_post”, “all” (the response - whether students feel positively or negatively towards the question), “grade” (the respondent’s grade level), and “class” (which classifies which course the question refers to - EGR, math, or science)

### EGR

``` r
new_products <- empower %>%
  select(pre_or_post, new_products, grade) %>%
  mutate(class = "Engineering and Technology")
names(new_products) <- c("pre_or_post", "all", "grade", "class")

engineering_everyday <- empower %>%
  select(pre_or_post, engineering_everyday, grade) %>%
  mutate(class = "Engineering and Technology")
names(engineering_everyday) <- c("pre_or_post", "all", "grade", "class")

enjoy_building <- empower %>%
  select(pre_or_post, enjoy_building, grade) %>%
  mutate(class = "Engineering and Technology")
names(enjoy_building) <- c("pre_or_post", "all", "grade", "class")

interested_machines <- empower %>%
  select(pre_or_post, interested_machines, grade) %>%
  mutate(class = "Engineering and Technology")
names(interested_machines) <- c("pre_or_post", "all", "grade", "class")

career_design <- empower %>%
  select(pre_or_post, career_design, grade) %>%
  mutate(class = "Engineering and Technology")
names(career_design) <- c("pre_or_post", "all", "grade", "class")

curiosity_tech <- empower %>%
  select(pre_or_post, curiosity_tech, grade) %>%
  mutate(class = "Engineering and Technology")
names(curiosity_tech) <- c("pre_or_post", "all", "grade", "class")

future_innovation <- empower %>%
  select(pre_or_post, future_innovation, grade) %>%
  mutate(class = "Engineering and Technology")
names(future_innovation) <- c("pre_or_post", "all", "grade", "class")

mathscience_useful <- empower %>%
  select(pre_or_post, mathscience_useful, grade) %>%
  mutate(class = "Engineering and Technology")
names(mathscience_useful) <- c("pre_or_post", "all", "grade", "class")

success_engineering <- empower %>%
  select(pre_or_post, success_engineering, grade) %>%
  mutate(class = "Engineering and Technology")
names(success_engineering) <- c("pre_or_post", "all", "grade", "class")

i_can_build <- empower %>%
  select(pre_or_post, i_can_build, grade) %>%
  mutate(class = "Engineering and Technology")
names(i_can_build) <- c("pre_or_post", "all", "grade", "class")

opportunity_engineering <- empower %>%
  select(pre_or_post, opportunity_engineering, grade) %>%
  mutate(class = "Engineering and Technology")
names(opportunity_engineering) <- c("pre_or_post", "all", "grade", "class")
```

### Science

``` r
sure_science <- empower %>%
  select(pre_or_post, sure_science, grade) %>%
  mutate(class = "Science")
names(sure_science) <- c("pre_or_post", "all", "grade", "class")

science_career <- empower %>%
  select(pre_or_post, science_career, grade) %>%
  mutate(class = "Science")
names(science_career) <- c("pre_or_post", "all", "grade", "class")

science_outsideofschool <- empower %>%
  select(pre_or_post, science_outsideofschool, grade) %>%
  mutate(class = "Science")
names(science_outsideofschool) <- c("pre_or_post", "all", "grade", "class")

science_pay <- empower %>%
  select(pre_or_post, science_pay, grade) %>%
  mutate(class = "Science")
names(science_pay) <- c("pre_or_post", "all", "grade", "class")

science_job <- empower %>%
  select(pre_or_post, science_job, grade) %>%
  mutate(class = "Science")
names(science_job) <- c("pre_or_post", "all", "grade", "class")

good_science <- empower %>%
  select(pre_or_post, good_science, grade) %>%
  mutate(class = "Science")
names(good_science) <- c("pre_or_post", "all", "grade", "class")

higherlevel_science <- empower %>%
  select(pre_or_post, higherlevel_science, grade) %>%
  mutate(class = "Science")
names(higherlevel_science) <- c("pre_or_post", "all", "grade", "class")

science_courses <- empower %>%
  select(pre_or_post, science_courses, grade) %>%
  mutate(class = "Science")
names(science_courses) <- c("pre_or_post", "all", "grade", "class")
```

### Math

``` r
math_career <- empower %>%
  select(pre_or_post, math_career, grade) %>%
  mutate(class = "Math")
names(math_career) <- c("pre_or_post", "all", "grade", "class")

good_at_math <- empower %>%
  select(pre_or_post, good_at_math, grade) %>%
  mutate(class = "Math")
names(good_at_math) <- c("pre_or_post", "all", "grade", "class")

higherlevel_math <- empower %>%
  select(pre_or_post, higherlevel_math, grade) %>%
  mutate(class = "Math")
names(higherlevel_math) <- c("pre_or_post", "all", "grade", "class")

good_math_grades <- empower %>%
  select(pre_or_post, good_math_grades, grade) %>%
  mutate(class = "Math")
names(good_math_grades) <- c("pre_or_post", "all", "grade", "class")

interesting_math <- empower %>%
  select(pre_or_post, interesting_math, grade) %>%
  mutate(class = "Math")
names(interesting_math) <- c("pre_or_post", "all", "grade", "class")

future_math <- empower %>%
  select(pre_or_post, future_math, grade) %>%
  mutate(class = "Math")
names(future_math) <- c("pre_or_post", "all", "grade", "class")

math_courses <- empower %>%
  select(pre_or_post, math_courses, grade) %>%
  mutate(class = "Math")
names(math_courses) <- c("pre_or_post", "all", "grade", "class")
```

## Binding all the variables to one data set, in order to conduct simulations

``` r
together <- rbind(new_products,
             engineering_everyday,
             enjoy_building,
             interested_machines,
             career_design,
             curiosity_tech,
             future_innovation,
             mathscience_useful,
             success_engineering,
             i_can_build,
             opportunity_engineering, 
             sure_science,
             science_career, 
             science_outsideofschool,
             science_pay,
             science_job,
             good_science,
             higherlevel_science,
             science_courses,
             math_career,
             good_at_math,
             higherlevel_math,
             good_math_grades,
             interesting_math,
             future_math,
             math_courses)
```

``` r
together$grade <- together$grade %>%
  str_replace("5 bach", "5") %>%
  str_replace("5 mag", "5")
together <- transform(together, all = as.numeric(all))
```

## Grade 1 Math

``` r
grade1_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(class == "Math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.18        0.199
    ## 2 Pre          3.38        0.199

``` r
set.seed(2019)
perm_grade1_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(class == "Math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.326       0.310

## Grade 1 Science

``` r
grade1_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(class == "Science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.52        0.151
    ## 2 Pre          3.67        0.151

``` r
set.seed(2019)
perm_grade1_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(class == "Science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.277       0.268

## Grade 1 EGR

``` r
grade1_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(class == "Engineering and Technology") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_egr
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.14       -0.293
    ## 2 Pre          3.85       -0.293

``` r
set.seed(2019)
perm_grade1_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(class == "Engineering and Technology") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_egr %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.224       0.222

``` r
perm_grade1_egr %>%
  filter(stat >= 0.292801) %>%
  summarise(p_value = (n()/1000) * 2)
```

    ## # A tibble: 1 x 1
    ##   p_value
    ##     <dbl>
    ## 1    0.01

## Grade 1 Overall

``` r
grade1 <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.69      -0.0232
    ## 2 Pre          3.67      -0.0232

``` r
set.seed(2019)
perm_grade1 <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1 %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.161       0.160

## Grade 2 Math

``` r
grade2_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(class == "Math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.22        0.122
    ## 2 Pre          3.34        0.122

``` r
set.seed(2019)
perm_grade2_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(class == "Math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.316       0.319

## Grade 2 Science

``` r
grade2_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(class == "Science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.73      -0.0478
    ## 2 Pre          3.68      -0.0478

``` r
set.seed(2019)
perm_grade2_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(class == "Science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.277       0.260

## Grade 2 EGR

``` r
grade2_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(class == "Engineering and Technology") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_egr
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.07      -0.0740
    ## 2 Pre          4.00      -0.0740

``` r
set.seed(2019)
perm_grade2_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(class == "Engineering and Technology") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_egr %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.191       0.197

## Grade 2 all

``` r
grade2 <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.73      -0.0244
    ## 2 Pre          3.71      -0.0244

``` r
set.seed(2019)
perm_grade2 <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2 %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.161       0.144

## Grade 3 Math

``` r
grade3_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(class == "Math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.73      -0.0584
    ## 2 Pre          3.67      -0.0584

``` r
set.seed(2019)
perm_grade3_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(class == "Math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.217       0.204

## Grade 3 Science

``` r
grade3_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(class == "Science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.80      -0.0595
    ## 2 Pre          3.74      -0.0595

``` r
set.seed(2019)
perm_grade3_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(class == "Science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.189       0.187

## Grade 3 EGR

``` r
grade3_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(class == "Engineering and Technology") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_egr
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.02      -0.0152
    ## 2 Pre          4         -0.0152

``` r
set.seed(2019)
perm_grade3_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(class == "Engineering and Technology") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_egr %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.156       0.152

## Grade 3 all

``` r
grade3 <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.87      -0.0409
    ## 2 Pre          3.83      -0.0409

``` r
set.seed(2019)
perm_grade3 <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3 %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.106       0.105

## Grade 5 Math

``` r
grade5_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(class == "Math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.66       -0.338
    ## 2 Pre          3.33       -0.338

``` r
set.seed(2019)
perm_grade5_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(class == "Math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.233       0.229

## Grade 5 science

``` r
grade5_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(class == "Science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.94       -0.153
    ## 2 Pre          3.79       -0.153

``` r
set.seed(2019)
perm_grade5_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(class == "Science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.175       0.176

## Grade 5 EGR

``` r
grade5_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(class == "Engineering and Technology") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_egr
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.03       -0.230
    ## 2 Pre          3.80       -0.230

``` r
set.seed(2019)
perm_grade5_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(class == "Engineering and Technology") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_egr %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.144       0.143

## Grade 5 All

``` r
grade5 <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.90       -0.237
    ## 2 Pre          3.67       -0.237

``` r
set.seed(2019)
perm_grade5 <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5 %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.107       0.105

## Math all

``` r
all_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(class == "Math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
all_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.52      -0.0666
    ## 2 Pre          3.45      -0.0666

``` r
set.seed(2019)
perm_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(class == "Math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.136       0.138

## Science all

``` r
all_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(class == "Science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
all_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.78      -0.0497
    ## 2 Pre          3.73      -0.0497

``` r
set.seed(2019)
perm_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(class == "Science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.106       0.108

## EGR all

``` r
all_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(class == "Engineering and Technology") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
all_egr
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.04       -0.137
    ## 2 Pre          3.91       -0.137

``` r
set.seed(2019)
perm_egr <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(class == "Engineering and Technology") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_egr %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     -0.0790      0.0895

## All

## EGR all

``` r
all_together <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
all_together
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.82      -0.0934
    ## 2 Pre          3.73      -0.0934

``` r
set.seed(2019)
perm <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  specify(response = all, explanatory = pre_or_post) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     -0.0611      0.0634

``` r
perm %>%
  filter(stat >= 0.09340795) %>%
  summarise(p_value = (n()/1000) * 2)
```

    ## # A tibble: 1 x 1
    ##   p_value
    ##     <dbl>
    ## 1   0.002
