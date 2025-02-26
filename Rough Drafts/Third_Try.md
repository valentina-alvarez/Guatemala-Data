Third\_Iteration
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

``` r
first <- together %>%
  filter(grade == "1")
mean_first <- first %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  group_by(pre_or_post, class) %>%
  summarize(mean = mean(all))
mean_first
```

    ## # A tibble: 6 x 3
    ## # Groups:   pre_or_post [2]
    ##   pre_or_post class                       mean
    ##   <chr>       <chr>                      <dbl>
    ## 1 Post        Engineering and Technology  4.14
    ## 2 Post        Math                        3.18
    ## 3 Post        Science                     3.52
    ## 4 Pre         Engineering and Technology  3.85
    ## 5 Pre         Math                        3.38
    ## 6 Pre         Science                     3.67

``` r
mean_first$class <- factor(mean_first$class, levels = c("Math", "Science", "Engineering and Technology"))
mean_first$pre_or_post <- factor(mean_first$pre_or_post, levels = c("Pre", "Post"))

tiff("first.tiff", units="in", width=10, height=6, res=300)
ggplot(data = mean_first, mapping = aes(x = pre_or_post, y = mean, fill = class)) +
  geom_bar(stat = "identity") +
  facet_grid("class") +
    labs(title = "First Graders' Positivity Towards STEM Before and After 
the Implementation of Ignite", subtitle = "Stratified by Subject", x = "", y = "Positivity Towards STEM") +
    scale_fill_manual(values=c("slategray4", "steelblue", "lightblue4")) +
  theme(plot.title = element_text(size = 22),
  plot.subtitle = element_text(size = 18),
  axis.text=element_text(size=16),
  axis.title=element_text(size=20),
  legend.title=element_text(size=18),
  legend.text=element_text(size=16),
  strip.text=element_text(size=14),
  legend.position ="none")
dev.off()
```

    ## png 
    ##   2

``` r
second <- together %>%
  filter(grade == "2")
mean_second <- second %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  group_by(pre_or_post, class) %>%
  summarize(mean = mean(all))
mean_second
```

    ## # A tibble: 6 x 3
    ## # Groups:   pre_or_post [2]
    ##   pre_or_post class                       mean
    ##   <chr>       <chr>                      <dbl>
    ## 1 Post        Engineering and Technology  4.07
    ## 2 Post        Math                        3.22
    ## 3 Post        Science                     3.73
    ## 4 Pre         Engineering and Technology  4.00
    ## 5 Pre         Math                        3.34
    ## 6 Pre         Science                     3.68

``` r
mean_second$class <- factor(mean_second$class, levels = c("Math", "Science", "Engineering and Technology"))
mean_second$pre_or_post <- factor(mean_second$pre_or_post, levels = c("Pre", "Post"))

tiff("second.tiff", units="in", width=10, height=6, res=300)
ggplot(data = mean_second, mapping = aes(x = pre_or_post, y = mean, fill = class)) +
  geom_bar(stat = "identity") +
  facet_grid("class") +
    labs(title = "Second Graders' Positivity Towards STEM Before and After 
the Implementation of Ignite", subtitle = "Stratified by Subject", x = "", y = "Positivity Towards STEM") +
    scale_fill_manual(values=c("slategray4", "steelblue", "lightblue4")) +
  theme(plot.title = element_text(size = 22),
  plot.subtitle = element_text(size = 18),
  axis.text=element_text(size=16),
  axis.title=element_text(size=20),
  legend.title=element_text(size=18),
  legend.text=element_text(size=16),
  strip.text=element_text(size=14),
  legend.position ="none")
dev.off()
```

    ## png 
    ##   2

``` r
third <- together %>%
  filter(grade == "3")
mean_third <- third %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  group_by(pre_or_post, class) %>%
  summarize(mean = mean(all))
mean_third
```

    ## # A tibble: 6 x 3
    ## # Groups:   pre_or_post [2]
    ##   pre_or_post class                       mean
    ##   <chr>       <chr>                      <dbl>
    ## 1 Post        Engineering and Technology  4.02
    ## 2 Post        Math                        3.73
    ## 3 Post        Science                     3.80
    ## 4 Pre         Engineering and Technology  4   
    ## 5 Pre         Math                        3.67
    ## 6 Pre         Science                     3.74

``` r
mean_third$class <- factor(mean_third$class, levels = c("Math", "Science", "Engineering and Technology"))
mean_third$pre_or_post <- factor(mean_third$pre_or_post, levels = c("Pre", "Post"))

tiff("third.tiff", units="in", width=10, height=6, res=300)
ggplot(data = mean_third, mapping = aes(x = pre_or_post, y = mean, fill = class)) +
  geom_bar(stat = "identity") +
  facet_grid("class") +
    labs(title = "Third Graders' Positivity Towards STEM Before and After 
the Implementation of Ignite", subtitle = "Stratified by Subject", x = "", y = "Positivity Towards STEM") +
    scale_fill_manual(values=c("slategray4", "steelblue", "lightblue4")) +
  theme(plot.title = element_text(size = 22),
  plot.subtitle = element_text(size = 18),
  axis.text=element_text(size=16),
  axis.title=element_text(size=20),
  legend.title=element_text(size=18),
  legend.text=element_text(size=16),
  strip.text=element_text(size=14),
  legend.position ="none")
dev.off()
```

    ## png 
    ##   2

``` r
fifth <- together %>%
  filter(grade == "5")
mean_fifth <- fifth %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  group_by(pre_or_post, class) %>%
  summarize(mean = mean(all))
mean_fifth
```

    ## # A tibble: 6 x 3
    ## # Groups:   pre_or_post [2]
    ##   pre_or_post class                       mean
    ##   <chr>       <chr>                      <dbl>
    ## 1 Post        Engineering and Technology  4.03
    ## 2 Post        Math                        3.66
    ## 3 Post        Science                     3.94
    ## 4 Pre         Engineering and Technology  3.80
    ## 5 Pre         Math                        3.33
    ## 6 Pre         Science                     3.79

``` r
mean_fifth$class <- factor(mean_fifth$class, levels = c("Math", "Science", "Engineering and Technology"))
mean_fifth$pre_or_post <- factor(mean_fifth$pre_or_post, levels = c("Pre", "Post"))

tiff("fifth.tiff", units="in", width=10, height=6, res=300)
ggplot(data = mean_fifth, mapping = aes(x = pre_or_post, y = mean, fill = class)) +
  geom_bar(stat = "identity") +
  facet_grid("class") +
    labs(title = "Fifth Graders' Positivity Towards STEM Before and After 
the Implementation of Ignite", subtitle = "Stratified by Subject", x = "", y = "Positivity Towards STEM") +
    scale_fill_manual(values=c("slategray4", "steelblue", "lightblue4")) +
  theme(plot.title = element_text(size = 22),
  plot.subtitle = element_text(size = 18),
  axis.text=element_text(size=16),
  axis.title=element_text(size=20),
  legend.title=element_text(size=18),
  legend.text=element_text(size=16),
  strip.text=element_text(size=14),
  legend.position ="none")
dev.off()
```

    ## png 
    ##   2
