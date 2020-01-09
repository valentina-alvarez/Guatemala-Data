Data Exploration
================

# Load packages

``` r
library(tidyverse)
library(ggplot2)
library(shiny)
library(infer)
library(data.table)
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
empower_2019 <- full_join(pre_2019, post_2019) %>%
  select(-c(other_plan_afterschool))
empower_2019$grade <- as.factor(empower_2019$grade)
empower <- full_join(empower_2018, empower_2019)
```

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
empower$contribute_community <- change_names(empower$contribute_community)
empower$resolve_problems_community <- change_names(empower$resolve_problems_community)
empower$external_help <- change_names(empower$external_help)
empower$leader_community <- change_names(empower$leader_community)
empower$contribute_community.1 <- change_names(empower$contribute_community.1)
```

## Create different datasets for each question that will be analyzed. Each dataset should have the variables: “pre\_or\_post”, “all” (the response - whether students feel positively or negatively towards the question), “grade” (the respondent’s grade level), and “class” (which classifies which course the question refers to - EGR, math, or science)

### EGR

``` r
new_products <- empower %>%
  select(pre_or_post, new_products, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "new_products")
names(new_products) <- c("pre_or_post", "all", "grade", "class", "question")

engineering_everyday <- empower %>%
  select(pre_or_post, engineering_everyday, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "engineering_everyday")
names(engineering_everyday) <- c("pre_or_post", "all", "grade", "class", "question")

enjoy_building <- empower %>%
  select(pre_or_post, enjoy_building, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "enjoy_building")
names(enjoy_building) <- c("pre_or_post", "all", "grade", "class", "question")

interested_machines <- empower %>%
  select(pre_or_post, interested_machines, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "interested_machines")
names(interested_machines) <- c("pre_or_post", "all", "grade", "class", "question")

career_design <- empower %>%
  select(pre_or_post, career_design, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "career_design")
names(career_design) <- c("pre_or_post", "all", "grade", "class", "question")

curiosity_tech <- empower %>%
  select(pre_or_post, curiosity_tech, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "curiosity_tech")
names(curiosity_tech) <- c("pre_or_post", "all", "grade", "class", "question")

future_innovation <- empower %>%
  select(pre_or_post, future_innovation, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "future_innovation")
names(future_innovation) <- c("pre_or_post", "all", "grade", "class", "question")

mathscience_useful <- empower %>%
  select(pre_or_post, mathscience_useful, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "mathscience_useful")
names(mathscience_useful) <- c("pre_or_post", "all", "grade", "class", "question")

success_engineering <- empower %>%
  select(pre_or_post, success_engineering, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "success_engineering")
names(success_engineering) <- c("pre_or_post", "all", "grade", "class", "question")

i_can_build <- empower %>%
  select(pre_or_post, i_can_build, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "i_can_build")
names(i_can_build) <- c("pre_or_post", "all", "grade", "class", "question")

opportunity_engineering <- empower %>%
  select(pre_or_post, opportunity_engineering, grade) %>%
  mutate(class = "Engineering and Technology") %>%
  mutate(question = "opportunity")
names(opportunity_engineering) <- c("pre_or_post", "all", "grade", "class", "question")
```

### Science

``` r
sure_science <- empower %>%
  select(pre_or_post, sure_science, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "sure_science")
names(sure_science) <- c("pre_or_post", "all", "grade", "class", "question")

science_career <- empower %>%
  select(pre_or_post, science_career, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "science_career")
names(science_career) <- c("pre_or_post", "all", "grade", "class", "question")

science_outsideofschool <- empower %>%
  select(pre_or_post, science_outsideofschool, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "science_outsideofschool")
names(science_outsideofschool) <- c("pre_or_post", "all", "grade", "class", "question")

science_pay <- empower %>%
  select(pre_or_post, science_pay, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "science_pay")
names(science_pay) <- c("pre_or_post", "all", "grade", "class", "question")

science_job <- empower %>%
  select(pre_or_post, science_job, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "science_job")
names(science_job) <- c("pre_or_post", "all", "grade", "class", "question")

good_science <- empower %>%
  select(pre_or_post, good_science, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "good_science")
names(good_science) <- c("pre_or_post", "all", "grade", "class", "question")

higherlevel_science <- empower %>%
  select(pre_or_post, higherlevel_science, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "higherlevel_science")
names(higherlevel_science) <- c("pre_or_post", "all", "grade", "class", "question")

science_courses <- empower %>%
  select(pre_or_post, science_courses, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "science_courses")
names(science_courses) <- c("pre_or_post", "all", "grade", "class", "question")

decentschool_badscience <- empower %>%
  select(pre_or_post, decentschool_badscience, grade) %>%
  mutate(class = "Science") %>%
  mutate(question = "decentschool_badscience")
names(decentschool_badscience) <- c("pre_or_post", "all", "grade", "class", "question")
```

### Math

``` r
math_career <- empower %>%
  select(pre_or_post, math_career, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "math_career")
names(math_career) <- c("pre_or_post", "all", "grade", "class", "question")

good_at_math <- empower %>%
  select(pre_or_post, good_at_math, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "good_at_math")
names(good_at_math) <- c("pre_or_post", "all", "grade", "class", "question")

higherlevel_math <- empower %>%
  select(pre_or_post, higherlevel_math, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "higherlevel_math")
names(higherlevel_math) <- c("pre_or_post", "all", "grade", "class", "question")

good_math_grades <- empower %>%
  select(pre_or_post, good_math_grades, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "good_math_grades")
names(good_math_grades) <- c("pre_or_post", "all", "grade", "class", "question")

interesting_math <- empower %>%
  select(pre_or_post, interesting_math, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "interesting_math")
names(interesting_math) <- c("pre_or_post", "all", "grade", "class", "question")

future_math <- empower %>%
  select(pre_or_post, future_math, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "future_math")
names(future_math) <- c("pre_or_post", "all", "grade", "class", "question")

math_courses <- empower %>%
  select(pre_or_post, math_courses, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "math_courses")
names(math_courses) <- c("pre_or_post", "all", "grade", "class", "question")

math_worst <- empower %>%
  select(pre_or_post, math_worst, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "math_worst")
names(math_worst) <- c("pre_or_post", "all", "grade", "class", "question")

not_enjoy_math <- empower %>%
  select(pre_or_post, not_enjoy_math, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "not_enjoy_math")
names(not_enjoy_math) <- c("pre_or_post", "all", "grade", "class", "question")

decentschool_badmath <- empower %>%
  select(pre_or_post, decentschool_badmath, grade) %>%
  mutate(class = "Math") %>%
  mutate(question = "decentschool_badmath")
names(decentschool_badmath) <- c("pre_or_post", "all", "grade", "class", "question")
```

### Helping community

``` r
contribute_community <- empower %>%
  select(pre_or_post, contribute_community, grade) %>%
  mutate(class = "Others") %>%
  mutate(question = "contribute_community")
names(contribute_community) <- c("pre_or_post", "all", "grade", "class", "question")

resolve_problems_community <- empower %>%
  select(pre_or_post, resolve_problems_community, grade) %>%
  mutate(class = "Others") %>%
  mutate(question = "resolve_problems_community")
names(resolve_problems_community) <- c("pre_or_post", "all", "grade", "class", "question")

external_help <- empower %>%
  select(pre_or_post, external_help, grade) %>%
  mutate(class = "Others") %>%
  mutate(question = "external_help")
names(external_help) <- c("pre_or_post", "all", "grade", "class", "question")

leader_community <- empower %>%
  select(pre_or_post, leader_community, grade) %>%
  mutate(class = "Others") %>%
  mutate(question = "leader_community")
names(leader_community) <- c("pre_or_post", "all", "grade", "class", "question")

contribute_community.1 <- empower %>%
  select(pre_or_post, contribute_community.1, grade) %>%
  mutate(class = "Others") %>%
  mutate(question = "contribute_community.1")
names(contribute_community.1) <- c("pre_or_post", "all", "grade", "class", "question")
```

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
others <- rbind(math_worst, 
                not_enjoy_math,
                decentschool_badmath,
                decentschool_badscience,
                contribute_community,
                resolve_problems_community,
                external_help,
                leader_community,
                contribute_community.1)
```

``` r
together$grade <- together$grade %>%
  str_replace("5 bach", "5") %>%
  str_replace("5 mag", "5")
together <- transform(together, all = as.numeric(all))
```

``` r
together %>%
  filter(!is.na(all)) %>%
  group_by(pre_or_post) %>%
  count(question)
```

    ## # A tibble: 52 x 3
    ## # Groups:   pre_or_post [2]
    ##    pre_or_post question                 n
    ##    <chr>       <chr>                <int>
    ##  1 Post        career_design          101
    ##  2 Post        curiosity_tech         101
    ##  3 Post        engineering_everyday   102
    ##  4 Post        enjoy_building         102
    ##  5 Post        future_innovation      100
    ##  6 Post        future_math            102
    ##  7 Post        good_at_math           102
    ##  8 Post        good_math_grades       101
    ##  9 Post        good_science           101
    ## 10 Post        higherlevel_math       101
    ## # … with 42 more rows

``` r
observations1 <- together %>%
  group_by(question, pre_or_post, grade) %>%
  filter(!is.na(all)) %>%
  count()
observations1
```

    ## # A tibble: 258 x 4
    ## # Groups:   question, pre_or_post, grade [258]
    ##    question      pre_or_post grade     n
    ##    <chr>         <chr>       <chr> <int>
    ##  1 career_design Post        <NA>      1
    ##  2 career_design Post        1        16
    ##  3 career_design Post        2        19
    ##  4 career_design Post        3        36
    ##  5 career_design Post        5        29
    ##  6 career_design Pre         <NA>      1
    ##  7 career_design Pre         1        21
    ##  8 career_design Pre         2        18
    ##  9 career_design Pre         3        39
    ## 10 career_design Pre         5        34
    ## # … with 248 more rows

``` r
others$grade <- others$grade %>%
  str_replace("5 bach", "5") %>%
  str_replace("5 mag", "5")
others <- transform(others, all = as.numeric(all))
```

``` r
observations2 <- others %>%
  group_by(question, pre_or_post, grade) %>%
  filter(!is.na(all)) %>%
  count()
observations2
```

    ## # A tibble: 88 x 4
    ## # Groups:   question, pre_or_post, grade [88]
    ##    question             pre_or_post grade     n
    ##    <chr>                <chr>       <chr> <int>
    ##  1 contribute_community Post        <NA>      1
    ##  2 contribute_community Post        1        15
    ##  3 contribute_community Post        2        20
    ##  4 contribute_community Post        3        36
    ##  5 contribute_community Post        5        29
    ##  6 contribute_community Pre         <NA>      1
    ##  7 contribute_community Pre         1        20
    ##  8 contribute_community Pre         2        21
    ##  9 contribute_community Pre         3        38
    ## 10 contribute_community Pre         5        35
    ## # … with 78 more rows

``` r
first_others <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
first_others
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.41       0.0160
    ## 2 Pre          3.43       0.0160

``` r
second_others <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
second_others
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.24       0.0559
    ## 2 Pre          3.30       0.0559

``` r
third_others <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
third_others
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.15      -0.0713
    ## 2 Pre          3.08      -0.0713

``` r
set.seed(2019)
perm_third_others <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_third_others %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.145       0.273

``` r
fifth_others <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
fifth_others
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.39     -0.00877
    ## 2 Pre          3.38     -0.00877

``` r
set.seed(2019)
perm_fifth_others <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_fifth_others %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.186       0.197

# MATH

### Math career

2.  I consider choosing a career related to mathematics.

YES

``` r
grade1_math_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "math_career") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_math_career
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         2.81      -0.0125
    ## 2 Pre          2.8       -0.0125

``` r
set.seed(2019)
perm_grade1_math_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "math_career") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_math_career %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.844       0.830

``` r
grade2_math_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "math_career") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_math_career
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         2.05        0.316
    ## 2 Pre          2.36        0.316

YES

``` r
grade3_math_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "math_career") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_math_career
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post          3.5         -0.5
    ## 2 Pre           3           -0.5

``` r
set.seed(2019)
perm_grade3_math_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "math_career") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_math_career %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     -0.0691        1.09

YES

``` r
grade5_math_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "math_career") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_math_career
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         2.82       -0.292
    ## 2 Pre          2.53       -0.292

``` r
set.seed(2019)
perm_grade5_math_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "math_career") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_math_career %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.279       0.846

### Good at math

4.  I’m the kind of student who usually does well in math.

<!-- end list -->

``` r
grade1_good_at_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "good_at_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_good_at_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         2.44        0.467
    ## 2 Pre          2.90        0.467

``` r
grade2_good_at_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "good_at_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_good_at_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         2.85        0.377
    ## 2 Pre          3.23        0.377

YES

``` r
grade3_good_at_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "good_at_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_good_at_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.25       -0.147
    ## 2 Pre          3.10       -0.147

``` r
set.seed(2019)
perm_grade3_good_at_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "good_at_math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_good_at_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.471       0.700

YES

``` r
grade5_good_at_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "good_at_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_good_at_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.14       -0.309
    ## 2 Pre          2.83       -0.309

``` r
set.seed(2019)
perm_grade5_good_at_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "good_at_math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_good_at_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.258       0.820

### Higher level math

6.  I consider taking higher level math courses.

<!-- end list -->

``` r
grade1_higherlevel_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "higherlevel_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_higherlevel_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.19        0.241
    ## 2 Pre          3.43        0.241

YES

``` r
grade2_higherlevel_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "higherlevel_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_higherlevel_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.45       -0.260
    ## 2 Pre          3.19       -0.260

``` r
set.seed(2019)
perm_grade2_higherlevel_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "higherlevel_math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_higherlevel_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.628        1.05

YES

``` r
grade3_higherlevel_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "higherlevel_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_higherlevel_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.91      -0.0425
    ## 2 Pre          3.87      -0.0425

``` r
set.seed(2019)
perm_grade3_higherlevel_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "higherlevel_math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_higherlevel_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.450       0.541

YES

``` r
grade5_higherlevel_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "higherlevel_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_higherlevel_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.59       -0.174
    ## 2 Pre          3.41       -0.174

``` r
set.seed(2019)
perm_grade5_higherlevel_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "higherlevel_math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_higherlevel_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.462       0.787

### Good math grades

7.  I got good grades in math

YES

``` r
grade1_good_math_grades <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "good_math_grades") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_good_math_grades
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.27      -0.0286
    ## 2 Pre          3.24      -0.0286

``` r
set.seed(2019)
perm_grade1_good_math_grades <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "good_math_grades") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_good_math_grades %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.725       0.788

``` r
grade2_good_math_grades <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "good_math_grades") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_good_math_grades
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         2.9         0.195
    ## 2 Pre          3.10        0.195

``` r
grade3_good_math_grades <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "good_math_grades") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_good_math_grades
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.25        0.160
    ## 2 Pre          3.41        0.160

YES

``` r
grade5_good_math_grades <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "good_math_grades") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_good_math_grades
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.41       -0.214
    ## 2 Pre          3.2        -0.214

``` r
set.seed(2019)
perm_grade5_good_math_grades <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "good_math_grades") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_good_math_grades %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.329       0.767

### Interesting math

8.  I think mathematics is interesting

<!-- end list -->

``` r
grade1_interesting_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "interesting_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_interesting_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.75        0.107
    ## 2 Pre          3.86        0.107

``` r
grade2_interesting_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "interesting_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_interesting_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.95        0.145
    ## 2 Pre          4.10        0.145

``` r
grade3_interesting_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "interesting_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_interesting_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.42       0.0192
    ## 2 Pre          4.44       0.0192

YES

``` r
grade5_interesting_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "interesting_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_interesting_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.17       -0.401
    ## 2 Pre          3.77       -0.401

``` r
set.seed(2019)
perm_grade5_interesting_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "interesting_math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_interesting_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     -0.0369       0.845

### Future Math

9.  I plan to use mathematics in my future career.

<!-- end list -->

``` r
grade1_future_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "future_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all))
grade1_future_math
```

    ## # A tibble: 2 x 2
    ##   pre_or_post  mean
    ##   <chr>       <dbl>
    ## 1 Post         3.25
    ## 2 Pre          3.48

``` r
grade2_future_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "future_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_future_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.9         0.148
    ## 2 Pre          4.05        0.148

YES

``` r
grade3_future_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "future_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_future_math
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.86      -0.0150
    ## 2 Pre          3.85      -0.0150

``` r
set.seed(2019)
perm_grade3_future_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "future_math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_future_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.544       0.550

YES

``` r
grade5_future_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "future_math") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all))
grade5_future_math
```

    ## # A tibble: 2 x 2
    ##   pre_or_post  mean
    ##   <chr>       <dbl>
    ## 1 Post         4.31
    ## 2 Pre          3.74

``` r
grade5_future_math$pre_or_post <- factor(grade5_future_math$pre_or_post, levels = c("Pre", "Post"))

tiff("mathematics_future_career.tiff", units="in", width=7, height=9, res=300)
ggplot(data = grade5_future_math, mapping = aes(x = pre_or_post, y = mean)) +
  geom_bar(stat = "identity", fill = "slategray4") +
    labs(title = "I plan to use mathematics in my future career", subtitle = "11th graders", x = "", y = "Average Positivity Towards STEM") +
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
set.seed(2019)
perm_grade5_future_math <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "future_math") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_future_math %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1       0.111        1.04

### Math courses

10. I would like the opportunity to take more math courses.

<!-- end list -->

``` r
grade1_math_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "math_courses") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_math_courses
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.56        0.388
    ## 2 Pre          3.95        0.388

YES

``` r
grade2_math_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "math_courses") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_math_courses
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.5       -0.0714
    ## 2 Pre          3.43      -0.0714

``` r
set.seed(2019)
perm_grade2_math_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "math_courses") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_math_courses %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.815       0.881

``` r
grade3_math_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "math_courses") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_math_courses
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.89        0.111
    ## 2 Pre          4           0.111

YES

``` r
grade5_math_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "math_courses") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_math_courses
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.17       -0.401
    ## 2 Pre          3.77       -0.401

``` r
set.seed(2019)
perm_grade5_math_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "math_courses") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_math_courses %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.172       0.931

# SCIENCE

### Sure science

11. I feel safe in science classes.

<!-- end list -->

``` r
grade1_sure_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "sure_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_sure_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.62        0.280
    ## 2 Pre          3.90        0.280

``` r
grade2_sure_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "sure_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_sure_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.9         0.195
    ## 2 Pre          4.10        0.195

``` r
grade3_sure_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "sure_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_sure_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.56      0.00855
    ## 2 Pre          3.56      0.00855

YES

``` r
grade5_sure_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "sure_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_sure_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.45       -0.277
    ## 2 Pre          3.17       -0.277

``` r
set.seed(2019)
perm_grade5_sure_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "sure_science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_sure_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.251       0.833

### Science career

12. I consider a career related to science.

<!-- end list -->

``` r
grade1_science_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "science_career") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_science_career
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.31        0.188
    ## 2 Pre          3.5         0.188

``` r
grade2_science_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "science_career") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_science_career
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.47        0.276
    ## 2 Pre          3.75        0.276

YES

``` r
grade3_science_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_career") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_science_career
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.61      -0.0470
    ## 2 Pre          3.56      -0.0470

``` r
set.seed(2019)
perm_grade3_science_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_career") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_science_career %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.511       0.618

YES

``` r
grade5_science_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "science_career") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_science_career
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.93       -0.417
    ## 2 Pre          3.51       -0.417

``` r
set.seed(2019)
perm_grade5_science_career <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "science_career") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_science_career %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     -0.0779       0.955

### Science outside of school

13. I think I will use science when I leave school.

<!-- end list -->

``` r
grade1_science_outsideofschool <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "science_outsideofschool") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_science_outsideofschool
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.62        0.327
    ## 2 Pre          3.95        0.327

YES

``` r
grade2_science_outsideofschool <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "science_outsideofschool") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_science_outsideofschool
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.9       -0.0429
    ## 2 Pre          3.86      -0.0429

``` r
set.seed(2019)
perm_grade2_science_outsideofschool <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "science_outsideofschool") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_science_outsideofschool %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.809       0.870

``` r
grade3_science_outsideofschool <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_outsideofschool") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_science_outsideofschool
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.91      0.00879
    ## 2 Pre          3.92      0.00879

YES

``` r
grade5_science_outsideofschool <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "science_outsideofschool") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_science_outsideofschool
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.17       -0.115
    ## 2 Pre          4.06       -0.115

``` r
set.seed(2019)
perm_grade5_science_outsideofschool <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "science_outsideofschool") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_science_outsideofschool %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.263       0.514

### Science pay

14. Knowledge of science will help me earn a living.

YES

``` r
grade1_science_pay <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "science_pay") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_science_pay
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.69       -0.116
    ## 2 Pre          3.57       -0.116

``` r
set.seed(2019)
perm_grade1_science_pay <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "science_pay") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_science_pay %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.718       0.859

YES

``` r
grade2_science_pay <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "science_pay") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_science_pay
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.8        -0.181
    ## 2 Pre          3.62       -0.181

``` r
set.seed(2019)
perm_grade2_science_pay <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "science_pay") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_science_pay %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.556       0.911

YES

``` r
grade3_science_pay <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_pay") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_science_pay
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.11      -0.0598
    ## 2 Pre          4.05      -0.0598

``` r
set.seed(2019)
perm_grade3_science_pay <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_pay") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_science_pay %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.357       0.501

``` r
grade5_science_pay <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "science_pay") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_science_pay
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.14       0.0680
    ## 2 Pre          4.21       0.0680

### Science job

15. I need science for my future work.

<!-- end list -->

``` r
grade1_science_job <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "science_job") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_science_job
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.47        0.133
    ## 2 Pre          3.6         0.133

``` r
grade2_science_job <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "science_job") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_science_job
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.7        0.0619
    ## 2 Pre          3.76       0.0619

YES

``` r
grade3_science_job <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_job") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_science_job
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.89      -0.0684
    ## 2 Pre          3.82      -0.0684

``` r
set.seed(2019)
perm_grade3_science_job <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_job") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_science_job %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.470       0.594

``` r
grade5_science_job <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "science_job") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_science_job
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4           0.273
    ## 2 Pre          4.27        0.273

### Good science

16. I know I can do well in science classes

<!-- end list -->

``` r
grade1_good_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "good_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_good_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.81        0.287
    ## 2 Pre          4.1         0.287

YES

``` r
grade2_good_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "good_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_good_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.95      -0.0452
    ## 2 Pre          3.90      -0.0452

``` r
set.seed(2019)
perm_grade2_good_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "good_science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_good_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.645       0.788

YES

``` r
grade3_good_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "good_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_good_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.83       -0.123
    ## 2 Pre          3.71       -0.123

``` r
set.seed(2019)
perm_grade3_good_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "good_science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_good_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.393       0.625

YES

``` r
grade5_good_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "good_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_good_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.04       -0.183
    ## 2 Pre          3.85       -0.183

``` r
set.seed(2019)
perm_grade5_good_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "good_science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_good_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.257       0.619

### Higher level science

18. I am considering taking higher level courses in science.

YES

``` r
grade1_higherlevel_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "higherlevel_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_higherlevel_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.31       -0.263
    ## 2 Pre          3.05       -0.263

``` r
set.seed(2019)
perm_grade1_higherlevel_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "higherlevel_science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_higherlevel_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.502        1.00

YES

``` r
grade2_higherlevel_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "higherlevel_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_higherlevel_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.32       -0.316
    ## 2 Pre          3          -0.316

``` r
set.seed(2019)
perm_grade2_higherlevel_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "higherlevel_science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_higherlevel_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.453         1.1

YES

``` r
grade3_higherlevel_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "higherlevel_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_higherlevel_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.56       -0.171
    ## 2 Pre          3.38       -0.171

``` r
set.seed(2019)
perm_grade3_higherlevel_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "higherlevel_science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_higherlevel_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.409       0.767

YES

``` r
grade5_higherlevel_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "higherlevel_science") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_higherlevel_science
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.55       -0.218
    ## 2 Pre          3.33       -0.218

``` r
set.seed(2019)
perm_grade5_higherlevel_science <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "higherlevel_science") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_higherlevel_science %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.326       0.774

### Science courses

19. I would like the opportunity to take more science courses.

<!-- end list -->

``` r
grade1_science_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "science_courses") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_science_courses
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.31        0.354
    ## 2 Pre          3.67        0.354

YES

``` r
grade2_science_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "science_courses") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_science_courses
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.78       -0.357
    ## 2 Pre          3.42       -0.357

``` r
set.seed(2019)
perm_grade2_science_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "science_courses") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_science_courses %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.500        1.18

YES

``` r
grade3_science_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_courses") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_science_courses
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.94      -0.0234
    ## 2 Pre          3.92      -0.0234

``` r
set.seed(2019)
perm_grade3_science_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "science_courses") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_science_courses %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.509       0.561

YES

``` r
grade5_science_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "science_courses") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_science_courses
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.28       -0.335
    ## 2 Pre          3.94       -0.335

``` r
set.seed(2019)
perm_grade5_science_courses <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "science_courses") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_science_courses %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     -0.0998       0.791

# EGR

### New Products

20. I Iike to imagine myself creating new products.

YES

``` r
grade1_new_products <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "new_products") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_new_products
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.31       -0.265
    ## 2 Pre          4.05       -0.265

``` r
set.seed(2019)
perm_grade1_new_products <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "new_products") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_new_products %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.515       0.989

``` r
grade2_new_products <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "new_products") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_new_products
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.05       0.0585
    ## 2 Pre          4.11       0.0585

``` r
grade3_new_products <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "new_products") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_new_products
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.11       0.0427
    ## 2 Pre          4.15       0.0427

YES

``` r
grade5_new_products <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "new_products") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_new_products
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.38       -0.203
    ## 2 Pre          4.18       -0.203

``` r
set.seed(2019)
perm_grade5_new_products <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "new_products") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_new_products %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.170       0.593

### Engineering Everyday

21. If I learn engineering, then I can improve the things that people
    use every day.

YES

``` r
grade1_engineering_everyday <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "engineering_everyday") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_engineering_everyday
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.31      -0.0125
    ## 2 Pre          4.3       -0.0125

``` r
set.seed(2019)
perm_grade1_engineering_everyday <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "engineering_everyday") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_engineering_everyday %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.517       0.502

YES

``` r
grade2_engineering_everyday <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "engineering_everyday") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_engineering_everyday
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.2        -0.253
    ## 2 Pre          3.95       -0.253

``` r
set.seed(2019)
perm_grade2_engineering_everyday <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "engineering_everyday") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_engineering_everyday %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.365       0.841

YES

``` r
grade3_engineering_everyday <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "engineering_everyday") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_engineering_everyday
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.31       -0.100
    ## 2 Pre          4.21       -0.100

``` r
set.seed(2019)
perm_grade3_engineering_everyday <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "engineering_everyday") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_engineering_everyday %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.332       0.554

YES

``` r
grade5_engineering_everyday <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "engineering_everyday") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all))
grade5_engineering_everyday
```

    ## # A tibble: 2 x 2
    ##   pre_or_post  mean
    ##   <chr>       <dbl>
    ## 1 Post         4.28
    ## 2 Pre          3.88

``` r
grade5_engineering_everyday$pre_or_post <- factor(grade5_engineering_everyday$pre_or_post, levels = c("Pre", "Post"))

tiff("engineering_everyday.tiff", units="in", width=7, height=9, res=300)
ggplot(data = grade5_engineering_everyday, mapping = aes(x = pre_or_post, y = mean)) +
  geom_bar(stat = "identity", fill = "slategray4") +
    labs(title = "If I learn engineering, then I can 
improve the things that people use every day.", subtitle = "11th graders", x = "", y = "Average Positivity Towards STEM") +
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
set.seed(2019)
perm_grade5_engineering_everyday <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "engineering_everyday") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_engineering_everyday %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     0.00921       0.769

### Enjoy Building

22. I enjoy building and fixing things.

YES

``` r
grade1_enjoy_building <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "enjoy_building") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_enjoy_building
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.44       -0.533
    ## 2 Pre          3.90       -0.533

``` r
set.seed(2019)
perm_grade1_enjoy_building <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "enjoy_building") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_enjoy_building %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     -0.0706        1.16

``` r
grade2_enjoy_building <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "enjoy_building") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_enjoy_building
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.9         0.153
    ## 2 Pre          4.05        0.153

YES

``` r
grade3_enjoy_building <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "enjoy_building") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_enjoy_building
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4          -0.132
    ## 2 Pre          3.87       -0.132

``` r
set.seed(2019)
perm_grade3_enjoy_building <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "enjoy_building") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_enjoy_building %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.351       0.620

YES

``` r
grade5_enjoy_building <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "enjoy_building") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_enjoy_building
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.03       -0.459
    ## 2 Pre          3.58       -0.459

``` r
grade5_enjoy_building$pre_or_post <- factor(grade5_enjoy_building$pre_or_post, levels = c("Pre", "Post"))

tiff("enjoy_building.tiff", units="in", width=7, height=9, res=300)
ggplot(data = grade5_enjoy_building, mapping = aes(x = pre_or_post, y = mean)) +
  geom_bar(stat = "identity", fill = "slategray4") +
    labs(title = "I enjoy building and fixing things", subtitle = "11th graders", x = "", y = "Average Positivity Towards STEM") +
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
set.seed(2019)
perm_grade5_enjoy_building <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "enjoy_building") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_enjoy_building %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      0.0315       0.893

### Interested machines

23. I am interested in how certain machines work.

YES

``` r
grade1_interested_machines <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "interested_machines") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_interested_machines
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.44       -0.676
    ## 2 Pre          3.76       -0.676

``` r
set.seed(2019)
perm_grade1_interested_machines <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "interested_machines") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_interested_machines %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1     -0.0412        1.34

YES

``` r
grade2_interested_machines <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "interested_machines") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_interested_machines
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.45       -0.239
    ## 2 Pre          4.21       -0.239

``` r
set.seed(2019)
perm_grade2_interested_machines <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "interested_machines") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_interested_machines %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.308       0.823

YES

``` r
grade3_interested_machines <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "interested_machines") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_interested_machines
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.25      -0.0192
    ## 2 Pre          4.23      -0.0192

``` r
set.seed(2019)
perm_grade3_interested_machines <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "interested_machines") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_interested_machines %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.472       0.551

``` r
grade5_interested_machines<- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "interested_machines") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_interested_machines
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4          0.0882
    ## 2 Pre          4.09       0.0882

### Career Design

24. Designing products or structures will be important for my future
    career.

YES

``` r
grade1_careerdesign <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "career_design") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_careerdesign
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4          -0.143
    ## 2 Pre          3.86       -0.143

``` r
set.seed(2019)
perm_grade1_careerdesign <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "career_design") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_careerdesign %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.485       0.764

YES

``` r
grade2_careerdesign <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "career_design") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_careerdesign
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.63       -0.243
    ## 2 Pre          3.39       -0.243

``` r
set.seed(2019)
perm_grade2_careerdesign <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "career_design") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_careerdesign %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.656        1.10

``` r
grade3_careerdesign <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "career_design") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_careerdesign
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.25        0.186
    ## 2 Pre          3.44        0.186

``` r
grade5_careerdesign <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "career_design") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_careerdesign
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.62       0.0852
    ## 2 Pre          3.71       0.0852

### Curiosity Tech

25. I’m curious about how electronics work.

YES

``` r
grade1_curiosity_tech <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "curiosity_tech") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_curiosity_tech
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.25        -0.25
    ## 2 Pre          4           -0.25

``` r
set.seed(2019)
perm_grade1_curiosity_tech <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "curiosity_tech") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_curiosity_tech %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.516        1.06

YES

``` r
grade2_curiosity_tech <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "curiosity_tech") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_curiosity_tech
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.45       -0.292
    ## 2 Pre          4.16       -0.292

``` r
set.seed(2019)
perm_grade2_curiosity_tech <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "curiosity_tech") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_curiosity_tech %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1        -0.5        1.09

YES

``` r
grade3_curiosity_tech <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "curiosity_tech") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_curiosity_tech
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.17       -0.120
    ## 2 Pre          4.05       -0.120

``` r
set.seed(2019)
perm_grade3_curiosity_tech <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "curiosity_tech") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_curiosity_tech %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.390       0.676

YES

``` r
grade5_curiosity_tech <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "curiosity_tech") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_curiosity_tech
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.28       -0.188
    ## 2 Pre          4.09       -0.188

``` r
set.seed(2019)
perm_grade5_curiosity_tech <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "curiosity_tech") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_curiosity_tech %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.201       0.625

### Future Innovation

26. I would like to use creativity and innovation in my future work.

<!-- end list -->

``` r
grade1_future_innovation <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "future_innovation") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_future_innovation
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.86       0.0902
    ## 2 Pre          3.95       0.0902

YES

``` r
grade2_future_innovation <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "future_innovation") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_future_innovation
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.85     -0.00789
    ## 2 Pre          3.84     -0.00789

``` r
set.seed(2019)
perm_grade2_future_innovation <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "future_innovation") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_future_innovation %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.669       0.671

``` r
grade3_future_innovation <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "future_innovation") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_future_innovation
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.94        0.107
    ## 2 Pre          4.05        0.107

YES

``` r
grade5_future_innovation <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "future_innovation") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_future_innovation
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.41       -0.232
    ## 2 Pre          4.18       -0.232

``` r
set.seed(2019)
perm_grade5_future_innovation <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "future_innovation") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_future_innovation %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.249       0.724

### Math science useful

27. Knowing how to use mathematics and science together will allow me to
    invent useful things.

<!-- end list -->

``` r
grade1_mathscience_useful <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "mathscience_useful") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_mathscience_useful
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.75         0.25
    ## 2 Pre          4            0.25

YES

``` r
grade2_mathscience_useful <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "mathscience_useful") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_mathscience_useful
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.15      -0.0974
    ## 2 Pre          4.05      -0.0974

``` r
set.seed(2019)
perm_grade2_mathscience_useful <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "mathscience_useful") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_mathscience_useful %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.508       0.675

``` r
grade3_mathscience_useful <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "mathscience_useful") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_mathscience_useful
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.11       0.0427
    ## 2 Pre          4.15       0.0427

YES

``` r
grade5_mathscience_useful <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "mathscience_useful") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_mathscience_useful
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.21       -0.207
    ## 2 Pre          4          -0.207

``` r
set.seed(2019)
perm_grade5_mathscience_useful <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "mathscience_useful") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_mathscience_useful %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.186       0.633

### Success engineering

28. I think I can succeed in an engineering career.

YES

``` r
grade1_success_engineering <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "success_engineering") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_success_engineering
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.94       -0.414
    ## 2 Pre          3.52       -0.414

``` r
set.seed(2019)
perm_grade1_success_engineering <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "success_engineering") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_success_engineering %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.462        1.17

YES

``` r
grade2_success_engineering <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "success_engineering") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_success_engineering
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.25       -0.145
    ## 2 Pre          4.11       -0.145

``` r
set.seed(2019)
perm_grade2_success_engineering <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "success_engineering") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_success_engineering %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.321       0.606

YES

``` r
grade3_success_engineering <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "success_engineering") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_success_engineering
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.14       -0.190
    ## 2 Pre          3.95       -0.190

``` r
set.seed(2019)
perm_grade3_success_engineering <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "success_engineering") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_success_engineering %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.239       0.625

YES

``` r
grade5_success_engineering <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "success_engineering") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_success_engineering
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.54      -0.0945
    ## 2 Pre          3.44      -0.0945

``` r
set.seed(2019)
perm_grade5_success_engineering <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "success_engineering") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_success_engineering %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.429       0.652

# I can build

29. I am good at building and fixing things.

YES

``` r
grade1_i_can_build <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "i_can_build") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_i_can_build
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.19       -0.568
    ## 2 Pre          3.62       -0.568

``` r
set.seed(2019)
perm_grade1_i_can_build <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "i_can_build") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_i_can_build %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.158        1.34

``` r
grade2_i_can_build <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "i_can_build") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_i_can_build
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.47        0.415
    ## 2 Pre          3.89        0.415

``` r
grade3_i_can_build <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "i_can_build") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_i_can_build
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.56       0.0234
    ## 2 Pre          3.58       0.0234

YES

``` r
grade5_i_can_build <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "i_can_build") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_i_can_build
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.69       -0.631
    ## 2 Pre          3.06       -0.631

``` r
grade5_i_can_build$pre_or_post <- factor(grade5_i_can_build$pre_or_post, levels = c("Pre", "Post"))

tiff("i_can_build.tiff", units="in", width=7, height=9, res=300)
ggplot(data = grade5_i_can_build, mapping = aes(x = pre_or_post, y = mean)) +
  geom_bar(stat = "identity", fill = "slategray4") +
    labs(title = "I am good at building and fixing things", subtitle = "11th graders", x = "", y = "Average Positivity Towards STEM") +
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
set.seed(2019)
perm_grade5_i_can_build <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "i_can_build") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_i_can_build %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1       0.180        1.05

### Opportunity egr

30. I would like the opportunity to take more engineering courses.

YES

``` r
grade1_opportunity <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "opportunity") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_opportunity
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.06       -0.634
    ## 2 Pre          3.43       -0.634

``` r
set.seed(2019)
perm_grade1_opportunity <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "opportunity") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_opportunity %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.219        1.48

YES

``` r
grade2_opportunity <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "opportunity") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_opportunity
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.3        -0.142
    ## 2 Pre          4.16       -0.142

``` r
set.seed(2019)
perm_grade2_opportunity <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "opportunity") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_opportunity %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.420       0.724

YES

``` r
grade3_opportunity <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "opportunity") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_opportunity
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.33      -0.0175
    ## 2 Pre          4.32      -0.0175

``` r
set.seed(2019)
perm_grade3_opportunity <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "opportunity") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_opportunity %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.421       0.470

YES

``` r
grade5_opportunity <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "opportunity") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_opportunity
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.86       -0.274
    ## 2 Pre          3.59       -0.274

``` r
set.seed(2019)
perm_grade5_opportunity <- together %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "opportunity") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_opportunity %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.291       0.867

# OTHERS

# Grade 1

## Contribute Community

``` r
grade1_contribute_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "contribute_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_contribute_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post          4         0.1000
    ## 2 Pre           4.1       0.1000

## Resolve Problems Community

``` r
grade1_resolve_problems_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "resolve_problems_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_resolve_problems_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.8       0.00952
    ## 2 Pre          3.81      0.00952

## External Help

``` r
grade1_external_help <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "external_help") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_external_help
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         2.71        0.286
    ## 2 Pre          3           0.286

## Leader Community

``` r
grade1_leader_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "leader_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_leader_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.53     -0.00952
    ## 2 Pre          3.52     -0.00952

``` r
set.seed(2019)
perm_grade1_leader_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "leader_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade1_leader_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.647       0.625

## Contribute Community .1

``` r
grade1_contribute_community.1 <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "1") %>%
  filter(question == "contribute_community.1") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade1_contribute_community.1
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.73       0.0762
    ## 2 Pre          3.81       0.0762

## Grade 2

## Contribute Community

``` r
grade2_contribute_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "contribute_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_contribute_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.95       -0.188
    ## 2 Pre          3.76       -0.188

``` r
set.seed(2019)
perm_grade2_contribute_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "contribute_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_contribute_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.628       0.931

## Resolve Problems Community

``` r
grade2_resolve_problems_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "resolve_problems_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_resolve_problems_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.95       -0.331
    ## 2 Pre          3.62       -0.331

``` r
set.seed(2019)
perm_grade2_resolve_problems_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "resolve_problems_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_resolve_problems_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.443       0.981

## External Help

``` r
grade2_external_help <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "external_help") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_external_help
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         2.95        0.193
    ## 2 Pre          3.14        0.193

## Leader Community

``` r
grade2_leader_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "leader_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_leader_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.48       -0.143
    ## 2 Pre          3.33       -0.143

``` r
set.seed(2019)
perm_grade2_leader_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "leader_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_leader_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.655       0.929

## Contribute Community .1

``` r
grade2_contribute_community.1 <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "contribute_community.1") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade2_contribute_community.1
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.86      -0.0476
    ## 2 Pre          3.81      -0.0476

``` r
set.seed(2019)
perm_grade2_contribute_community.1 <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "2") %>%
  filter(question == "contribute_community.1") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade2_contribute_community.1 %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.712       0.788

## Grade 3

## Contribute Community

``` r
grade3_contribute_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "contribute_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_contribute_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.97      -0.0512
    ## 2 Pre          3.92      -0.0512

``` r
set.seed(2019)
perm_grade3_contribute_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "contribute_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_contribute_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.424       0.542

## Resolve Problems Community

``` r
grade3_resolve_problems_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "resolve_problems_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_resolve_problems_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.89      -0.0468
    ## 2 Pre          3.84      -0.0468

``` r
set.seed(2019)
perm_grade3_resolve_problems_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "resolve_problems_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_resolve_problems_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.410       0.555

## External Help

``` r
grade3_external_help <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "external_help") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_external_help
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.41       -0.141
    ## 2 Pre          3.27       -0.141

``` r
set.seed(2019)
perm_grade3_external_help <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "external_help") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_external_help %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.428       0.708

## Leader Community

``` r
grade3_leader_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "leader_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_leader_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.63      -0.0233
    ## 2 Pre          3.61      -0.0233

``` r
set.seed(2019)
perm_grade3_leader_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "leader_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_leader_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.520       0.537

## Contribute Community .1

``` r
grade3_contribute_community.1 <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "contribute_community.1") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade3_contribute_community.1
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4         -0.0263
    ## 2 Pre          3.97      -0.0263

``` r
set.seed(2019)
perm_grade3_contribute_community.1 <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "3") %>%
  filter(question == "contribute_community.1") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade3_contribute_community.1 %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.509       0.573

## Grade 5

## Contribute Community

``` r
grade5_contribute_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "contribute_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_contribute_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4           0.171
    ## 2 Pre          4.17        0.171

## Resolve Problems Community

``` r
grade5_resolve_problems_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "resolve_problems_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_resolve_problems_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.10       -0.103
    ## 2 Pre          4          -0.103

``` r
set.seed(2019)
perm_grade5_resolve_problems_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "resolve_problems_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_resolve_problems_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.226       0.453

## External Help

``` r
grade5_external_help <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "external_help") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_external_help
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.24        0.101
    ## 2 Pre          3.34        0.101

## Leader Community

``` r
grade5_leader_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "leader_community") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_leader_community
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         3.90       -0.211
    ## 2 Pre          3.69       -0.211

``` r
set.seed(2019)
perm_grade5_leader_community <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "leader_community") %>%
  specify(response = all, explanatory = pre_or_post) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Post", "Pre"))
```

``` r
perm_grade5_leader_community %>%
  summarize(lower_bound = quantile(stat, 0.025),
            upper_bound = quantile(stat, 0.975))
```

    ## # A tibble: 1 x 2
    ##   lower_bound upper_bound
    ##         <dbl>       <dbl>
    ## 1      -0.267       0.668

## Contribute Community .1

``` r
grade5_contribute_community.1 <- others %>%
  filter(!is.na(all)) %>%
  filter(!is.na(pre_or_post)) %>%
  filter(grade == "5") %>%
  filter(question == "contribute_community.1") %>%
  group_by(pre_or_post) %>%
  summarize(mean = mean(all)) %>%
  mutate(diff(mean))
grade5_contribute_community.1
```

    ## # A tibble: 2 x 3
    ##   pre_or_post  mean `diff(mean)`
    ##   <chr>       <dbl>        <dbl>
    ## 1 Post         4.07        0.131
    ## 2 Pre          4.2         0.131
