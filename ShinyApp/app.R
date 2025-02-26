# Load packages -----------------------------------------------------
library(tidyverse)
library(ggplot2)
library(shiny)

# Load data ---------------------------------------------------------
load("/cloud/project/data/empower_new.RData")
load("/cloud/project/data/empower_byage.RData")


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