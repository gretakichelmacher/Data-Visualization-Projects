# set the environment
library(tidyverse)
library(reshape2)
library(shiny)
library(rsconnect)
library(plotly)
library(DT)
library(forcats)

rsconnect::setAccountInfo(name='data-visualizations-by-greta-kichelmacher',
                          token='38B1B9A281FB93056A920E8FC44E1A63',
                          secret='NMPrSxoUcImW9+39nVjuFJRgvKnCKAmjwrlBEjh0')

# load the dataset 
df=read.csv("data.csv", sep=";")


### EDA ------------------------------------------------------------------------


## Get the number of unique categories per feature 
for (col_name in names(df)) {
  unique_values <- unique(df[[col_name]])
  num_unique <- length(unique_values)
  cat("Number of unique categories in", col_name, ":", num_unique, "\n")
}


names(df)


dim(df)
# we can see that our dataset has 4424 observations and 37 variables 

summary(df)


## check for missing values
nulls_per_column <- colSums(is.na(df)) # there are none


## check for duplicates
duplicate_rows <- sum(duplicated(df)) # there are none


# rename some variables for better visualization and interpretation
# data taken from (https://archive.ics.uci.edu/dataset
#/697/predict+students+dropout+and+academic+success)

student_data <- df %>%
  mutate(
    Marital.status = factor(Marital.status,
                            levels = 1:6,
                            labels = c("Single", "Married", "Widower", "Divorced", "Facto union", "Legally separated")),
    Mother.s.qualification = factor(Mother.s.qualification,
                                    levels = c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 14, 18, 19, 22, 26, 27, 29, 30, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44),
                                    labels = c("Secondary Education - 12th Year of Schooling or Eq.", "Higher Education - Bachelor's Degree", "Higher Education - Degree", "Higher Education - Master's", "Higher Education - Doctorate", "Frequency of Higher Education", "12th Year of Schooling - Not Completed", "11th Year of Schooling - Not Completed", "7th Year (Old)", "Other - 11th Year of Schooling", "10th Year of Schooling", "General commerce course", "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv.", "Technical-professional course", "7th year of schooling", "2nd cycle of the general high school course", "9th Year of Schooling - Not Completed", "8th year of schooling", "Unknown", "Can't read or write", "Can read without having a 4th year of schooling", "Basic education 1st cycle (4th/5th year) or equiv.", "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv.", "Technological specialization course", "Higher education - degree (1st cycle)", "Specialized higher studies course", "Professional higher technical course", "Higher Education - Master (2nd cycle)", "Higher Education - Doctorate (3rd cycle)")),
    Father.s.qualification = factor(Father.s.qualification,
                                    levels = c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14, 18, 19, 20, 22, 25, 26, 27, 29, 30, 31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44),
                                    labels = c("Secondary Education - 12th Year of Schooling or Eq.", "Higher Education - Bachelor's Degree", "Higher Education - Degree", "Higher Education - Master's", "Higher Education - Doctorate", "Frequency of Higher Education", "12th Year of Schooling - Not Completed", "11th Year of Schooling - Not Completed", "7th Year (Old)", "Other - 11th Year of Schooling", "2nd year complementary high school course", "10th Year of Schooling", "General commerce course", "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv.", "Complementary High School Course", "Technical-professional course", "Complementary High School Course - not concluded", "7th year of schooling", "2nd cycle of the general high school course", "9th Year of Schooling - Not Completed", "8th year of schooling", "General Course of Administration and Commerce", "Supplementary Accounting and Administration", "Unknown", "Can't read or write", "Can read without having a 4th year of schooling", "Basic education 1st cycle (4th/5th year) or equiv.", "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv.", "Technological specialization course", "Higher education - degree (1st cycle)", "Specialized higher studies course", "Professional higher technical course", "Higher Education - Master (2nd cycle)", "Higher Education - Doctorate (3rd cycle)")),
    Mother.s.occupation = factor(Mother.s.occupation,
                                 levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 90, 99, 122, 123, 125, 131, 132, 134, 141, 143, 144, 151, 152, 153, 171, 173, 175, 191, 192, 193, 194),
                                 labels = c("Student", "Representatives of the Legislative Power and Executive Bodies, Directors, Directors and Executive Managers", "Specialists in Intellectual and Scientific Activities", "Intermediate Level Technicians and Professions", "Administrative staff", "Personal Services, Security and Safety Workers and Sellers", "Farmers and Skilled Workers in Agriculture, Fisheries and Forestry", "Skilled Workers in Industry, Construction and Craftsmen", "Installation and Machine Operators and Assembly Workers", "Unskilled Workers", "Armed Forces Professions", "Other Situation", "(blank)", "Health professionals", "Teachers", "Specialists in information and communication technologies (ICT)", "Intermediate level science and engineering technicians and professions", "Technicians and professionals, of intermediate level of health", "Intermediate level technicians from legal, social, sports, cultural and similar services", "Office workers, secretaries in general and data processing operators", "Data, accounting, statistical, financial services and registry-related operators", "Other administrative support staff", "Personal service workers", "Sellers", "Personal care workers and the like", "Skilled construction workers and the like, except electricians", "Skilled workers in printing, precision instrument manufacturing, jewelers, artisans and the like", "Workers in food processing, woodworking, clothing and other industries and crafts", "Cleaning workers", "Unskilled workers in agriculture, animal production, fisheries and forestry", "Unskilled workers in extractive industry, construction, manufacturing and transport", "Meal preparation assistants")),
    Father.s.occupation = factor(Father.s.occupation,
                                 levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 90, 99, 101, 102, 103, 112, 114, 121, 122, 123, 124, 131, 132, 134, 135, 141, 143, 144, 151, 152, 153, 154, 161, 163, 171, 172, 174, 175, 181, 182, 183, 192, 193, 194, 195),
                                 labels = c("Student", "Representatives of the Legislative Power and Executive Bodies, Directors, Directors and Executive Managers", "Specialists in Intellectual and Scientific Activities", "Intermediate Level Technicians and Professions", "Administrative staff", "Personal Services, Security and Safety Workers and Sellers", "Farmers and Skilled Workers in Agriculture, Fisheries and Forestry", "Skilled Workers in Industry, Construction and Craftsmen", "Installation and Machine Operators and Assembly Workers", "Unskilled Workers", "Armed Forces Professions", "Other Situation", "(blank)", "Armed Forces Officers", "Armed Forces Sergeants", "Other Armed Forces personnel", "Directors of administrative and commercial services", "Hotel, catering, trade and other services directors", "Specialists in the physical sciences, mathematics, engineering and related techniques", "Health professionals", "Teachers", "Specialists in finance, accounting, administrative organization, public and commercial relations", "Intermediate level science and engineering technicians and professions", "Technicians and professionals, of intermediate level of health", "Intermediate level technicians from legal, social, sports, cultural and similar services", "Information and communication technology technicians", "Office workers, secretaries in general and data processing operators", "Data, accounting, statistical, financial services and registry-related operators", "Other administrative support staff", "Personal service workers", "Sellers", "Personal care workers and the like", "Protection and security services personnel", "Market-oriented farmers and skilled agricultural and animal production workers", "Farmers, livestock keepers, fishermen, hunters and gatherers, subsistence", "Skilled construction workers and the like, except electricians", "Skilled workers in metallurgy, metalworking and similar", "Skilled workers in electricity and electronics", "Workers in food processing, woodworking, clothing and other industries and crafts", "Fixed plant and machine operators", "Assembly workers", "Vehicle drivers and mobile equipment operators", "Unskilled workers in agriculture, animal production, fisheries and forestry", "Unskilled workers in extractive industry, construction, manufacturing and transport", "Meal preparation assistants", "Street vendors (except food) and street service providers")),
    Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
    Nationality = factor(Nacionality,
                         levels = c(1, 2, 6, 11, 13, 14, 17, 21, 22, 24, 25, 26, 32, 41, 62, 100, 101, 103, 105, 108, 109),
                         labels = c("Portuguese", "German", "Spanish", "Italian", "Dutch", "English", "Lithuanian", "Angolan", "Cape Verdean", "Guinean", "Mozambican", "Santomean", "Turkish", "Brazilian", "Romanian", "Moldova (Republic of)", "Mexican", "Ukrainian", "Russian", "Cuban", "Colombian")),
    International = factor(International, levels = c(0, 1), labels = c("No", "Yes")),
    
    Application.order = factor(Application.order, levels = 0:9),
    Application.mode = factor(Application.mode, 
                              levels = c(1, 2, 5, 7, 10, 15, 16, 17, 18, 26, 27, 39, 42, 43, 44, 51, 53, 57), 
                              labels = c("1st phase - general contingent", "Ordinance No. 612/93", "1st phase - special contingent (Azores Island)", "Holders of other higher courses", "Ordinance No. 854-B/99", "International student (bachelor)", "1st phase - special contingent (Madeira Island)", "2nd phase - general contingent", "3rd phase - general contingent", "Ordinance No. 533-A/99, item b2) (Different Plan)", "Ordinance No. 533-A/99, item b3 (Other Institution)", "Over 23 years old", "Transfer", "Change of course", "Technological specialization diploma holders", "Change of institution/course", "Short cycle diploma holders", "Change of institution/course (International)")),
    
    
    Scholarship.holder = factor(Scholarship.holder, levels = c(0, 1), labels = c("No", "Yes")),
    Debtor = factor(Debtor, levels = c(0, 1), labels = c("No", "Yes")),
    Tuition.fees.up.to.date = factor(Tuition.fees.up.to.date, levels = c(0, 1), labels = c("No", "Yes")),
    Educational.special.needs = factor(Educational.special.needs, levels = c(0, 1), labels = c("No", "Yes")),
    Course = factor(Course,
                    levels = c(33, 171, 8014, 9003, 9070, 9085, 9119, 9130, 9147, 9238, 9254, 9500, 9556, 9670, 9773, 9853, 9991),
                    labels = c("Biofuel Production Technologies", "Animation and Multimedia Design", "Social Service (evening attendance)", "Agronomy", "Communication Design", "Veterinary Nursing", "Informatics Engineering", "Equinculture", "Management", "Social Service", "Tourism", "Nursing", "Oral Hygiene", "Advertising and Marketing Management", "Journalism and Communication", "Basic Education", "Management (evening attendance)"))
  )




### VISUALIZATIONS -------------------------------------------------------------

# Function to prepare data for plotting to show both the count and 
# percentage of the target
prepare_plot_data <- function(data, group_by_column) {
  total_count <- nrow(data)
  data %>%
    group_by(!!sym(group_by_column)) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = sprintf("%.1f%%", Count / total_count * 100),
           TooltipInfo = paste("Count:", Count, "<br>Percentage:", 
                               Percentage)) %>%
    ungroup()
}


### DASHBOARD PREPARATION ------------------------------------------------------

# 1) DASHBOARD 1: Students Distribution Analysis


# Calculate class distribution
class_distribution <- table(df$Target)
class_df <- data.frame(Class = names(class_distribution),
                       Frequency = as.numeric(class_distribution))

# Create a color palette for consistency in both plots
colors <- rainbow(length(class_distribution))
class_colors <- setNames(colors, names(class_distribution))

# Create legends 
legends <- list()
for (i in 1:length(class_distribution)) {
  legends[[i]] <- paste(names(class_distribution)[i], "-", class_colors[i])
}






### DEFINE UI ------------------------------------------------------------------


# Define UI
ui <- navbarPage(
  title = "Students Dropout Analysis",
  tabPanel("Students Distribution Analysis",
           # UI for Dashboard 1
           titlePanel("Students Distribution Analysis"),
           mainPanel(
             fluidRow(
               column(6, plotOutput("barplot1")),
               column(6, plotlyOutput("piechart1")) 
             )
           )
  ),
  tabPanel("Demographics & Socio-Economic Background",
           # UI for Dashboard 2
           titlePanel("Student Demographics & Socio-Economic Background Analysis"),
           fluidPage(
             selectInput("targetFilter", "Select Target Category:", 
                         choices = c("All Students", "Graduate", "Enrolled", "Dropout"),
                         selected = "All Students"),
             plotlyOutput("maritalStatusPlot"),
             plotlyOutput("motherQualificationPlot"),
             plotlyOutput("fatherQualificationPlot"),
             sliderInput("ageSlider", "Age at Enrollment:",
                         min = min(student_data$Age.at.enrollment), 
                         max = max(student_data$Age.at.enrollment), 
                         value = c(min(student_data$Age.at.enrollment), 
                                   max(student_data$Age.at.enrollment))),
             plotlyOutput("ageDistributionPlot"),
             plotlyOutput("genderDistributionPlot"),
             plotlyOutput("internationalStudentsPlot")
           )
  ),
  tabPanel("Application Modes & Admission Insights",
           # UI for Dashboard 3
           titlePanel("Application Modes & Admission Insights"),
           fluidPage(
             selectInput("outcomeSelect", "Select Student Outcome:",
                         choices = c("All Students", "Graduate", "Enrolled", "Dropout"),
                         selected = "All Students"),
             plotlyOutput("appModePlot"),
             plotlyOutput("admissionGradePlot"),
             plotlyOutput("appOrderPlot")
           )
  ),
  tabPanel("Economic Factors & Educational Support",
           # UI for Dashboard 4
           titlePanel("Economic Indicators & Educational Support Services"),
           fluidPage(
             selectInput("filterTarget", "Filter by Student Outcome:", 
                         choices = c("All", unique(student_data$Target)), selected = "All"),
             div(plotlyOutput("scholarshipPlot"), style = "padding-bottom: 20px;"),
             div(plotlyOutput("financialStandingPlot"), style = "padding-bottom: 20px;"),
             div(plotlyOutput("BoxPlot1"), style = "padding-bottom: 20px;"),
             div(plotlyOutput("boxPlot2"), style = "padding-bottom: 20px;")
           )
  )
)




### DEFINE SERVER --------------------------------------------------------------


# Define Server
server <- function(input, output) {
  
  # Server logic for Dashboard 1
  output$barplot1 <- renderPlot({
    
    ggplot(class_df, aes(x = Class, y = Frequency, fill = Class)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Frequency), vjust = -0.3, color = "black") +
      labs(x = "Class", y = "Number of students") +
      theme_minimal() +
      theme(legend.position = "none", 
            axis.text.x = element_text(size=12, hjust = 1.5),
            axis.title.x = element_text(size = 15.5, margin = margin(t = 12)),
            axis.title.y = element_text(size = 15.5, margin = margin(t = 14)),
            plot.title = element_text(hjust = 0.5)) + 
      scale_fill_manual(values = class_colors)
  })
  
  output$piechart1 <- renderPlotly({
    
    pie_chart <- plot_ly(
      labels = names(class_distribution),
      values = class_distribution,
      type = "pie",
      textinfo = "percent+label",
      hoverinfo = "none", 
      marker = list(colors = colors),
      textposition = 'outside'
    )
    pie_chart
  })
  
  # Server logic for Dashboard 2
  
  # Server logic for Dashboard 2
  filtered_data_2 <- reactive({
    if (input$targetFilter == "All Students") {
      student_data
    } else {
      filter(student_data, Target == input$targetFilter)
    }
  })
  
  # Marital Status Distribution
  output$maritalStatusPlot <- renderPlotly({
    data <- prepare_plot_data(filtered_data_2(), "Marital.status")
    p <- ggplot(data, aes(x = reorder(Marital.status, -Count), y = Count, 
                          fill = Marital.status, text = TooltipInfo)) +
      geom_bar(stat = "identity") +
      scale_fill_discrete() +  
      theme_minimal() +
      labs(title = "Marital Status Distribution", x = NULL, y = "Count") +  
      theme(plot.title = element_text(size = 20), 
            axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # Parental Background - Mother's Qualification
  output$motherQualificationPlot <- renderPlotly({
    data <- filtered_data_2() %>%
      count(Mother.s.qualification) %>%
      top_n(10) %>%
      mutate(Qualification = reorder(Mother.s.qualification, -n),
             TooltipInfo = paste("Count:", n, "<br>Percentage:", 
                                 sprintf("%.1f%%", n / sum(n) * 100)))
    
    p <- ggplot(data, aes(x = Qualification, y = n, fill = Qualification, 
                          text = TooltipInfo)) +
      geom_bar(stat = "identity") +
      scale_fill_discrete() +  
      theme_minimal() +
      labs(title = "Top 10 Mother's Qualifications", x = NULL, y = "Count") +  
      theme(plot.title = element_text(size = 20), 
            axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # Parental Background - Father's Qualification
  output$fatherQualificationPlot <- renderPlotly({
    data <- filtered_data_2() %>%
      count(Father.s.qualification) %>%
      top_n(10) %>%
      mutate(Qualification = reorder(Father.s.qualification, -n),
             TooltipInfo = paste("Count:", n, "<br>Percentage:", 
                                 sprintf("%.1f%%", n / sum(n) * 100)))
    
    p <- ggplot(data, aes(x = Qualification, y = n, fill = Qualification, 
                          text = TooltipInfo)) +
      geom_bar(stat = "identity") +
      scale_fill_discrete() +  
      theme_minimal() +
      labs(title = "Top 10 Father's Qualifications", x = NULL, y = "Count") +  
      theme(plot.title = element_text(size = 20), axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # Age at Enrollment
  output$ageDistributionPlot <- renderPlotly({
    data <- filtered_data_2() %>%
      filter(Age.at.enrollment >= input$ageSlider[1] & 
               Age.at.enrollment <= input$ageSlider[2])
    age_counts <- data %>%
      group_by(Age.at.enrollment) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = sprintf("%.1f%%", Count / nrow(data) * 100),
             TooltipInfo = paste("Count:", Count, "<br>Percentage:", Percentage))
    
    p <- ggplot(age_counts, aes(x = Age.at.enrollment, y = Count, 
                                text = TooltipInfo)) +
      geom_bar(stat = "identity", fill = "blue") +  
      theme_minimal() +
      labs(title = "Age at Enrollment Distribution", x = "Age", y = "Count") +
      theme(plot.title = element_text(size = 20), axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gender Distribution
  output$genderDistributionPlot <- renderPlotly({
    data <- prepare_plot_data(filtered_data_2(), "Gender")
    p <- ggplot(data, aes(x = Gender, y = Count, fill = Gender, 
                          text = TooltipInfo)) +
      geom_bar(stat = "identity") +
      scale_fill_discrete() +  
      theme_minimal() +
      labs(title = "Gender Distribution", x = NULL, y = "Count") +  
      theme(plot.title = element_text(size = 20), axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # International Students
  output$internationalStudentsPlot <- renderPlotly({
    data <- prepare_plot_data(filtered_data_2(), "International")
    p <- ggplot(data, aes(x = International, y = Count, 
                          fill = International, text = TooltipInfo)) +
      geom_bar(stat = "identity") +
      scale_fill_discrete() +  
      theme_minimal() +
      labs(title = "International Students Distribution", x = NULL, 
           y = "Count") +  
      theme(plot.title = element_text(size = 20), axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })

    
  
  # Server logic for Dashboard 3
  
  filtered_data_3 <- reactive({
    if (input$outcomeSelect == "All Students") {
      return(student_data)
    } else {
      return(student_data %>% filter(Target == input$outcomeSelect))
    }
  })
  
  
  # Application Mode Analysis
  output$appModePlot <- renderPlotly({
    data <- prepare_plot_data(filtered_data_3(), "Application.mode")
    
    # Ordering by Count in descending order
    data$Application.mode <- 
      factor(data$Application.mode, 
             levels = data$Application.mode[order(-data$Count)])
    
    p <- ggplot(data, aes(x = Application.mode, y = Count, 
                          text = TooltipInfo)) +
      geom_bar(stat = "identity", fill = "blue") +  
      coord_flip() +
      theme_minimal() +
      labs(title = "Application Mode Analysis", x = NULL, y = "Count") +
      theme(plot.title = element_text(size = 20),
            legend.position = "none")  
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Admission Grade Analysis
  output$admissionGradePlot <- renderPlotly({
    data <- filtered_data_3()
    
    # if 'All Students' is selected 
    if (input$outcomeSelect == "All Students") {
      data$Target <- "All Students" # Unify all levels into a single level
    }
    
    # if other target category are selected
    p <- ggplot(data, aes(x = Admission.grade, y = Target, 
                          text = Admission.grade)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Admission Grade Analysis", x = "Admission Grade", 
           y = "Student Outcome") +
      theme(plot.title = element_text(size = 20))
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Application Order Preference
  output$appOrderPlot <- renderPlotly({
    data <- prepare_plot_data(filtered_data_3(), "Application.order")
    p <- ggplot(data, aes(x = fct_infreq(Application.order), y = Count, 
                          text = TooltipInfo)) +
      geom_bar(stat = "identity", fill = "blue") +  
      coord_flip() +
      theme_minimal() +
      labs(title = "Application Order Preference", x = "Application Order", 
           y = "Count") +
      theme(plot.title = element_text(size = 20),
            legend.position = "none")  
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Server logic for Dashboard 4
  
  filtered_data_4 <- reactive({
    data <- if (input$filterTarget == "All") {
      student_data
    } else {
      student_data %>% filter(Target == input$filterTarget)
    }
    data %>% 
      mutate(
        # If a student’s tuition fees are up to date and the student is not a
        # debtor, the student's financial status 
        # is considered "Good", otherwise is considered as 'Poor'.
        FinancialStatus = ifelse(Tuition.fees.up.to.date == "Yes" & 
                                   Debtor == "No", "Good", "Poor"),
        ScholarshipLabel = ifelse(Scholarship.holder == "Yes", 
                                  "With Scholarship", "Without Scholarship")
      )
  })
  
  # Scholarship Holders Plot
  output$scholarshipPlot <- renderPlotly({
    data <- prepare_plot_data(filtered_data_4(), "Scholarship.holder")
    p <- ggplot(data, aes(x = Scholarship.holder, y = Count, 
                          fill = Scholarship.holder, text = TooltipInfo)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Scholarship Distribution", x = "Scholarship Holder", 
           y = "Count") +
      theme(plot.title = element_text(size = 20))
    ggplotly(p, tooltip = "text")
  })
  
  # Financial Standing Plot
  output$financialStandingPlot <- renderPlotly({
    data <- prepare_plot_data(filtered_data_4(), "Debtor")
    p <- ggplot(data, aes(x = Debtor, y = Count, fill = Debtor, 
                          text = TooltipInfo)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Financial Standing", x = "Debtor Status", y = "Count") +
      theme(plot.title = element_text(size = 20))
    ggplotly(p, tooltip = "text")
  })
  
  # Box Plot for Grade Distribution by Financial Status 1st semester
  output$BoxPlot1 <- renderPlotly({
    data <- filtered_data_4()
    p <- ggplot(data, aes(x = FinancialStatus, 
                          y = Curricular.units.1st.sem..grade.)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "1st Semester Grades by Financial Status", 
           x = "Financial Status", y = "Grade") +
      theme(plot.title = element_text(size = 20))
    ggplotly(p)
  })
  
  # Box Plot for Grade Distribution by Financial Status 2nd semester
  output$boxPlot2 <- renderPlotly({
    data <- filtered_data_4()
    p <- ggplot(data, aes(x = FinancialStatus, 
                          y = Curricular.units.2nd.sem..grade.)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "2nd Semester Grades by Financial Status", 
           x = "Financial Status", y = "Grade") +
      theme(plot.title = element_text(size = 20))
    ggplotly(p)
  })
}


### RUN THE APPLICATION --------------------------------------------------------

# Run the application 
shinyApp(ui, server)







