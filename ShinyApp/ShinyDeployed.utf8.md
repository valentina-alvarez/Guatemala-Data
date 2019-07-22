---
title: "Shiny App Deployed"
output: html_document
---



### Load packages

```r
library(tidyverse)
library(ggplot2)
library(shiny)
```

### Load data

```r
pre_empower <- read.csv("data/PRE_Empowerment.csv",
                    sep = ";")
post_empower <- read.csv("data/POST_Empowerment.csv",
                    sep = ";")
```

# SECTION 1: ORGANIZING DATA
### Classifying and joining pre and post responses 

```r
pre_empower <- pre_empower %>%
  mutate(pre_or_post = "pre")
post_empower <- post_empower %>%
  mutate(pre_or_post = "post")
```


```r
empower <- full_join(pre_empower, post_empower)
```

```
## Joining, by = c("grade", "city_birth", "parents_stem", "which_parent_stem", "parent_profession", "number_science_classes", "number_math_classes", "number_tech_classes", "number_egr_classes", "math_worst", "math_career", "not_enjoy_math", "good_at_math", "decentschool_badmath", "higherlevel_math", "good_math_grades", "interesting_math", "future_math", "math_courses", "sure_science", "science_career", "science_outsideofschool", "science_pay", "science_job", "good_science", "decentschool_badscience", "higherlevel_science", "science_courses", "new_products", "engineering_everyday", "enjoy_building", "interested_machines", "career_design", "curiosity_tech", "future_innovation", "mathscience_useful", "success_engineering", "i_can_build", "opportunity_engineering", "prediction_literature", "prediction_math", "prediction_science", "future_math_classes", "future_science_classes", "future_egr_classes", "college", "what_major", "why_not_college", "plan_after_school", "other_plan_afterschool", "frequency_learning_stem", "why_no_chances", "opportunities_women_stem", "reason_opportunities_stem", "contribute_community", "resolve_problems_community", "external_help", "leader_community", "contribute_community.1", "pre_or_post")
```

```
## Warning: Column `parent_profession` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `math_courses` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `science_job` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `good_science` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `new_products` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `engineering_everyday` joining factors with different
## levels, coercing to character vector
```

```
## Warning: Column `enjoy_building` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `curiosity_tech` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `future_innovation` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `mathscience_useful` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `prediction_math` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `college` joining factors with different levels, coercing
## to character vector
```

```
## Warning: Column `what_major` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `why_not_college` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `plan_after_school` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `other_plan_afterschool` joining factors with different
## levels, coercing to character vector
```

```
## Warning: Column `why_no_chances` joining factors with different levels,
## coercing to character vector
```

```
## Warning: Column `opportunities_women_stem` joining factors with different
## levels, coercing to character vector
```

```
## Warning: Column `reason_opportunities_stem` joining factors with different
## levels, coercing to character vector
```

```
## Warning: Column `resolve_problems_community` joining factors with different
## levels, coercing to character vector
```

```
## Warning: Column `leader_community` joining factors with different levels,
## coercing to character vector
```

### Rank

```r
empower$ID <- seq.int(nrow(empower))
```

### Changing responses from letters to words and characterizing them as "Positive" or "Negative" 

To reduce the variability of the data, I joined the answers "Absolutely" with "Yes" to define someone who feels positively about a certain question. I joined "No" with "Absolutely Not" to define someone who feels negatively about a certain question. 


```r
change_names <- function(naming){
naming %>%
  str_replace("A", "no") %>%
  str_replace("B", "no") %>%
  str_replace("C", "neutral") %>%
  str_replace("D", "yes") %>%
  str_replace("E", "yes")
}

change_names2 <- function(naming){
naming %>%
  str_replace("A", "very good") %>%
  str_replace("B", "okay") %>%
  str_replace("C", "bad")
}

change_names3 <- function(naming){
naming %>%
  str_replace("A", "yes") %>%
  str_replace("B", "no") %>%
  str_replace("C", "not sure")
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

empower$prediction_literature <- change_names2(empower$prediction_literature)
empower$prediction_math <- change_names2(empower$prediction_math)
empower$prediction_science <- change_names2(empower$prediction_science)

empower$future_math_classes <- change_names3(empower$future_math_classes)
empower$future_science_classes <- change_names3(empower$future_science_classes)
empower$future_egr_classes <- change_names3(empower$future_egr_classes)
```

## Selecting data

I excluded variables that showed extreme variance or are not relevant to the study. For example, when they were asked whether their parents had STEM-related careers (parents_stem), many girls classified farming or other agricultural/manual labor as STEM-related careers.

I created a new data set that looks at the grades by age group ("Younger" vs. "Older")

```r
empower_new <- empower %>%
  select(-c(parents_stem, 
            which_parent_stem, 
            parent_profession, 
            number_science_classes, 
            number_math_classes, 
            number_tech_classes, 
            number_egr_classes))
empower_byage <- empower_new %>%
  mutate(age_groups = case_when(
    grade == "1" ~ "younger",
    grade == "2" ~ "younger",
    grade == "3" ~ "younger",
    grade == "5 bach" ~ "older",
    grade == "5 mag" ~ "older"
  ))
```


```r
save(empower_new, file = "data/empower_new.RData")
save(empower_byage, file = "data/empower_byage.RData")
```




```r
# Define UI

ui <- fluidPage(
    
    # App title
    titlePanel("Questions"),
    
    # Sidebar layout with a input and output definitions
    sidebarLayout(
        
        # Inputs: Select variables to plot
        sidebarPanel(
            
            # Select variable for y-axis
            selectInput(inputId = "y", 
                        label = "Question",
                        choices = c("math_career", "good_at_math", 
                                    "math_courses", "science_career", 
                                    "good_science", "science_courses",
                                    "mathscience_useful", "success_engineering",
                                    "opportunity_engineering"), 
                        selected = "math_career"),
            
            # Select variable for x-axis
            selectInput(inputId = "x", 
                        label = "Group",
                        choices = c("all_together", "age_groups", "grade"), 
                        selected = "all_together")
        ),
        
        # Output: Show graph
        mainPanel(
            plotOutput(outputId = "graph1"), 
            textOutput(outputId = "influential")
        )
    )
)

# Define server function --------------------------------------------
server <- function(input, output) 
    
    #Top Graph
    output$graph1 <- renderPlot({
        if(input$y == "math_career" && input$x == "all_together"){
            empower_new$math_career <- factor(empower_new$math_career, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = math_career), position = "fill") +
                labs(title = "Proportion of People Who Think of a Career in Math",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Would you consider a career in math?")
        }
        else if(input$y == "math_career" && input$x == "age_groups"){
            empower_byage$math_career <- factor(empower_byage$math_career, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post"))
            empower_byage$age_groups <- factor(empower_byage$age_groups, levels = c("younger", "older"))
            ggplot(data = empower_byage) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = math_career), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Think of a Career in Math",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Career in math?")
        }
        else if(input$y == "math_career" && input$x == "grade"){
            empower_new$math_career <- factor(empower_new$math_career, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = math_career), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Think of a Career in Math",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Career in math?")
        }
        else if(input$y == "good_at_math" && input$x == "all_together"){
            empower_new$good_at_math <- factor(empower_new$good_at_math, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                    fill = good_at_math), position = "fill") +
                labs(title = "Proportion of People Who Think They Are Good at Math",
                    x = "Pre or Post",
                    y = "Proportions", 
                    fill = "Good at math?")
        }
        else if(input$y == "good_at_math" && input$x == "age_groups"){
            empower_byage$good_at_math <- factor(empower_byage$good_at_math, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post"))
            ggplot(data = empower_byage) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = good_at_math), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Think They are Good at Math",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Good at math?")
        }
        else if(input$y == "good_at_math" && input$x == "grade"){
            empower_new$good_at_math <- factor(empower_new$good_at_math, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = good_at_math), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Think They are Good at Math",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Good at math?")    
        }
        else if(input$y == "math_courses" && input$x == "all_together"){
            empower_new$math_courses <- factor(empower_new$math_courses, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))  
                ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = math_courses), position = "fill") +
                labs(title = "Proportion of People Who Want to Take More Math Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "More math courses?")
        }
        
        else if(input$y == "math_courses" && input$x == "age_groups"){
            empower_byage$math_courses <- factor(empower_byage$math_courses, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post")) 
               ggplot(data = empower_byage) +
                 geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = math_courses), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Want to Take More Math Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "More math courses?")
        }
        else if(input$y == "math_courses" && input$x == "grade"){
            empower_new$math_courses <- factor(empower_new$math_courses, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))
            ggplot(data = empower_new) +    
            geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = math_courses), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Want to Take More Math Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "More math courses?")
        }
        else if(input$y == "science_career" && input$x == "all_together"){
            empower_new$science_career <- factor(empower_new$science_career, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))                
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = science_career), position = "fill") +
                labs(title = "Proportion of People Who Think of a Career in Science",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Career in science?")
        }
        else if(input$y == "science_career" && input$x == "age_groups"){
            empower_byage$science_career <- factor(empower_byage$science_career, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post"))                 
            ggplot(data = empower_byage) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = science_career), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Think of a Career in Science",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Career in science?")
        }
        else if(input$y == "science_career" && input$x == "grade"){
            empower_new$science_career <- factor(empower_new$science_career, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post")) 
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = science_career), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Think of a Career in Science",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Career in science?")
        }
        else if(input$y == "good_science" && input$x == "all_together"){
            empower_new$good_science <- factor(empower_new$good_science, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))              
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = good_science), position = "fill") +
                labs(title = "Proportion of People Who Think They are Good at Science",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Good at science?")
        }
        else if(input$y == "good_science" && input$x == "age_groups"){
            empower_byage$good_science <- factor(empower_byage$good_science, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post"))     
            ggplot(data = empower_byage) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = good_science), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Think They are Good at Science",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Good at science?")
        }
        else if(input$y == "good_science" && input$x == "grade"){
            empower_new$good_science <- factor(empower_new$good_science, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post")) 
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = good_science), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Think They are Good at Science",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Good at Science?")
        }
        else if(input$y == "science_courses" && input$x == "all_together"){
            empower_new$science_courses <- factor(empower_new$science_courses, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post")) 
            empower_new$science_courses <- factor(empower_new$science_courses, levels = c("yes", "neutral", "no"))
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = science_courses), position = "fill") +
                labs(title = "Proportion of People Who Want to take More Science Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "More Science Courses?")
        }
        else if(input$y == "science_courses" && input$x == "age_groups"){
            empower_byage$science_courses <- factor(empower_byage$science_courses, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post"))     
                ggplot(data = empower_byage) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = science_courses), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Want to take More Science Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "More Science Courses?")
        }
        else if(input$y == "science_courses" && input$x == "grade"){
            empower_new$science_courses <- factor(empower_new$science_courses, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post")) 
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = science_courses), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Want to take More Science Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "More Science Courses?")
        }
        else if(input$y == "mathscience_useful" && input$x == "all_together"){
            empower_new$mathscience_useful <- factor(empower_new$mathscience_useful, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post")) 
            empower_new$mathscience_useful <- factor(empower_new$mathscience_useful, levels = c("yes", "neutral", "no"))
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = mathscience_useful), position = "fill") +
                labs(title = "Proportion of People Who Think They Can Invent",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Can invent?")
        }
        else if(input$y == "mathscience_useful" && input$x == "age_groups"){
            empower_byage$mathscience_useful <- factor(empower_byage$mathscience_useful, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post"))            
            ggplot(data = empower_byage) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = mathscience_useful), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Think They Can Invent",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Can invent?")
        }
        else if(input$y == "mathscience_useful" && input$x == "grade"){
            empower_new$mathscience_useful <- factor(empower_new$mathscience_useful, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post")) 
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = mathscience_useful), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Think They Can Invent",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Can invent?")
        }
        else if(input$y == "success_engineering" && input$x == "all_together"){
            empower_new$success_engineering <- factor(empower_new$success_engineering, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))             
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = success_engineering), position = "fill") +
                labs(title = "Proportion of People Who Think They Can Succeed as Engineers",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Can succeed?")
        }
        else if(input$y == "success_engineering" && input$x == "age_groups"){
            empower_byage$success_engineering <- factor(empower_byage$success_engineering, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post"))            
            ggplot(data = empower_byage) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = success_engineering), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Think They Can Succeed as Engineers",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Can succeed?")
        }
        else if(input$y == "success_engineering" && input$x == "grade"){
            empower_new$success_engineering <- factor(empower_new$success_engineering, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post")) 
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = success_engineering), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Think They Can Succeed as Engineers",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "Can succeed?")
    }
        else if(input$y == "opportunity_engineering" && input$x == "all_together"){
            empower_new$opportunity_engineering <- factor(empower_new$opportunity_engineering, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post"))             
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = opportunity_engineering), position = "fill") +
                labs(title = "Proportion of People Who Want to Take More EGR Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "EGR courses?")
        }
        else if(input$y == "opportunity_engineering" && input$x == "age_groups"){
            empower_byage$opportunity_engineering <- factor(empower_byage$opportunity_engineering, levels = c("yes", "neutral", "no"))
            empower_byage$pre_or_post <- factor(empower_byage$pre_or_post, levels = c("pre", "post"))            
            ggplot(data = empower_byage) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = opportunity_engineering), position = "fill") +
                facet_grid(~ age_groups) +
                labs(title = "Proportion of People Who Want to Take More EGR Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "EGR Courses?")
        }
        else if(input$y == "opportunity_engineering" && input$x == "grade"){
            empower_new$opportunity_engineering <- factor(empower_new$opportunity_engineering, levels = c("yes", "neutral", "no"))
            empower_new$pre_or_post <- factor(empower_new$pre_or_post, levels = c("pre", "post")) 
            ggplot(data = empower_new) +
                geom_bar(mapping = aes(x = pre_or_post, 
                                       fill = opportunity_engineering), position = "fill") +
                facet_grid(~ grade) +
                labs(title = "Proportion of People Who Want to Take More EGR Courses",
                     x = "Pre or Post",
                     y = "Proportions", 
                     fill = "EGR Courses?")
        }
    }
            )
    

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
```

preserve7793a5b68e589eb9
