library(shiny)
library(tidyverse)
library(DT)
library(tools)

bCancer <- read_csv("breast_cancer_wisconsin_diagnostic.csv")

all_choices <- colnames(bCancer)
all_choices <- all_choices[endsWith(all_choices, "_mean")]

all_choices_names <- toTitleCase(gsub("_", " ", all_choices))
all_choices_list <- as.list(all_choices)
names(all_choices_list) <- all_choices_names

# Additional features to add
# DONE 1) More aesthetic choices names, more aesthetic axis labels
# DONE 2) hist function
# DONE 3) Give an option to look at the table of data directly
# DONE 4) More customization - different themes, different colours of the plots

ui <- navbarPage("Breast Cancer",
                 tabPanel("Scatterplot",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select Variables"),
                              selectInput("indepVar", label = "Independent Variable", 
                                          choices = all_choices_list, 
                                          selected = all_choices_list[1]),
                              selectInput("depVar", label = "Dependent Variable", 
                                          choices = all_choices_list, 
                                          selected = all_choices_list[2]),
                              
                              h3("Select Visual Elements"),
                              textInput("lineColor", label = "Line Color", value = "green4"),
                              selectInput("theme", label = "Theme", 
                                          choices = list("Classic" = "theme_classic", 
                                                         "Black and White" = "theme_bw",
                                                         "Minimal" = "theme_minimal",
                                                         "Void" = "theme_void"), 
                                          selected = all_choices[1]),
                              sliderInput("textSize", label = "Text Size", min = 8, 
                                          max = 20, value = 11)
                              
                            ),
                            
                            mainPanel(
                              plotOutput("lmPlot")
                            )
                          )
                 ),
                 tabPanel("Histogram",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select Variables"),
                              selectInput("histVar", label = "Variable", 
                                          choices = all_choices_list, 
                                          selected = all_choices_list[1]),
                              sliderInput("histBins", label = "Number of bins",
                                          min = 10, max = 200, value = 30),
                              checkboxInput("histCheckbox", label = "Color by diagnosis", value = FALSE),
                        
                              h3("Select Visual Elements"),
                              selectInput("histTheme", label = "Theme",
                                          choices = list("Classic" = "theme_classic",
                                                         "Black and White" = "theme_bw",
                                                         "Minimal" = "theme_minimal",
                                                         "Void" = "theme_void"),
                                          selected = all_choices[1]),
                              sliderInput("histTextSize", label = "Text Size", min = 8,
                                          max = 20, value = 11)
                              
                            ),
                            
                            mainPanel(
                              plotOutput("histPlot"),
                              uiOutput("histText")
                            )
                          )
                 ),
                 tabPanel("Table",
                          dataTableOutput("table")
                 )
                 
)

server <- function(input, output) {

    output$lmPlot <- renderPlot({
      
      round_digits <- 4  
      selectedIndepVariable <- input$indepVar
      selectedDepVariable <- input$depVar
      
      # Run linear model with variables
      formulaLM <- as.formula(paste(selectedDepVariable, selectedIndepVariable, sep = " ~ "))
      fit_bc <- lm(formulaLM, data = bCancer)
      
      # Text outputs 
      textOfFormula <- paste0(selectedDepVariable, 
                              " = ", 
                              round(fit_bc$coefficients[1], round_digits), 
                              " + ", 
                              round(fit_bc$coefficients[2], round_digits), 
                              "*", 
                              names(fit_bc$coefficients[2])
      )
      message("The formula to model ", 
              names(fit_bc$coefficients[2]), 
              " is: ", 
              textOfFormula)
      message(paste0("The intercept is: ", round(fit_bc$coefficients[1], round_digits))) 

      message(paste0("The slope of ",
                     names(fit_bc$coefficients[2]), 
                     " is ", 
                     round(fit_bc$coefficients[2], round_digits)))
      
      # ggplot output
      ggplot(bCancer, aes(x = .data[[selectedIndepVariable]], y = .data[[selectedDepVariable]])) +
        geom_point(alpha = 0.4) +
        geom_abline(intercept = fit_bc$coefficients[1],
                    slope = fit_bc$coefficients[2],
                    colour = input$lineColor,
                    linewidth = 1) +
        ggtitle(textOfFormula) +
        labs(x = toTitleCase(gsub("_", " ", selectedIndepVariable)), 
             y = toTitleCase(gsub("_", " ", selectedDepVariable))) + 
        get(input$theme)(base_size = input$textSize) +
        theme(plot.title = element_text(colour = input$lineColor))
      
    })
    
    output$histPlot <- renderPlot( {
      if (input$histCheckbox) {
        hist_plot <- ggplot(bCancer, aes(x = .data[[input$histVar]], fill = diagnosis)) + 
          geom_histogram(bins = input$histBins) +
          scale_fill_manual(values = list(B = "grey", M = "coral"), labels = list(B = "Benign", M = "Malignant")) + 
          labs(y = "Count", x = toTitleCase(gsub("_", " ", input$histVar)), fill = "Diagnosis") + 
          get(input$histTheme)(base_size = input$histTextSize)
      } else {
        hist_plot <- ggplot(bCancer, aes(x = .data[[input$histVar]])) + 
          geom_histogram(bins = input$histBins) +
          labs(y = "Count", x = toTitleCase(gsub("_", " ", input$histVar)), fill = "Diagnosis") +
          get(input$histTheme)(base_size = input$histTextSize)   
      }
      hist_plot
    })
    
    
    output$histText <- renderUI( {
      fit <- shapiro.test(bCancer[[input$histVar]])

      text <- paste0("The p-value of the Shapiro-Wilk test for ", input$histVar, ": ", signif(fit$p.value, digits=3))
      if (fit$p.value < 0.05) {
        text <- paste0(text, "<br>", "We reject the null hypothesis that ", input$histVar,  " is normal")
      } else {
        text <- paste0(text, "<br>", "We accept the null hypothesis that ", input$histVar,  " is normal")
      }
      
      HTML(text)
    })
    
    output$table <- renderDataTable({
      bCancer[, c("id", "diagnosis", all_choices)]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
