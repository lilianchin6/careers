#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(wordcloud)

cpdc <- read_csv("data/clean.csv")
deps =  cpdc$department[! cpdc$department %in% c("Other")]
deps <- deps[!is.na(deps)]

jobs <- read_csv("data/clean.csv")
# jobs_employers <- c("Google", "Microsoft", "IBM", "Amazon", "Deloitte", 
#                    "Accenture", "PNC", "Palantir", "Schlumberger", 
#                    "Facebook", "Apple", "Citi", "PricewaterhouseCoopers", 
#                    "Teach for America", "ExxonMobil", "Salesforce.com", 
#                    "Deutsche Bank", "Dropbox", "Epic Systems", "Intel", 
#                    "National Institutes of Health", "Capitol One", "Oracle")
jobs_employers <- jobs$employer.school[-grep("niversity|ollege|CMU|Carnegie|Massachusetts Institute|Not Reported|unknown", 
                                             jobs$employer.school)]
jobs_employers <- names(head(sort(table(jobs_employers), decreasing = TRUE), n = 50))
jobs_employers <- jobs_employers [! jobs_employers %in% "J.P. Morgan Chase"]
jobs <- subset(jobs, jobs$employer.school %in% jobs_employers)


dashboardPage(
  dashboardHeader(title = "CMU Careers"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Where do Students Go?", tabName = "C", icon = icon("map-marker")),
      menuItem("Post-Grad Status", tabName = "A", icon = icon("mortar-board")),
      menuItem("Student Networks", tabName = "B", icon = icon("group")),
      menuItem("Employer Hiring", tabName = "D", icon = icon("user-o")),
      menuItem("Exploring Gender", tabName = "Salaries", icon = icon("female"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              column(width = 8,
                     h1("CMU Careers"),
                     h3("About the Data set"),
                     p("This data set was provided by the Career and Professional Development Center at Carnegie Mellon University. It includes around 4000 student responses about post-education plans between 2012-2014. Examples of some data that was collected:"),
                     tags$ul(
                       tags$li("Degree Earned"), 
                       tags$li("Post Graduation Status"), 
                       tags$li("Name of Graduate School or Employeer"),
                       tags$li("Annual Salary"),
                       tags$li("Job Title and Industry")
                     ),
                     p("This app was created by Andrew Afable, Lilian Chin, Kiersten Chuc and Eric Lee for 36-315: Statistical Graphics and Visualizations taught by Sam Ventura."),
                     #h3("CPDC"),
                     h3(a("CPDC", href = "http://www.cmu.edu/career/", target="_blank")),
                     p("The Career and Professional Development Center (CPDC) at Carnegie Mellon University (CMU) is a centralized career center staffed by a team of 24 enthusiastic, dedicated, and highly-educated professionals who orchestrate the career exploration, experiential learning, and career networking needs of students and alumni."),
                     #h3("Carnegie Mellon University"),
                     h3(a("Carnegie Mellon University", href = "http://www.cmu.edu", target="_blank")),
                     p("Carnegie Mellon is a private, internationally ranked research university with programs in areas ranging from science, technology and business, to public policy, the humanities and the arts.
More than 13,000 students in the university's seven schools and colleges benefit from a small student-to-faculty ratio and an education characterized by its focus on creating and implementing solutions for real problems, interdisciplinary collaboration and innovation.
                       Carnegie Mellon's main campus in the United States is in Pittsburgh, Pa. It has campuses in California's Silicon Valley and Qatar, and programs in Africa, Asia, Australia, Europe and Mexico.")),
              
              column(width = 4,
                # A static valueBox
                valueBox("61%", "Employed", icon = icon("group"), width = NULL),
                valueBox("$83,146","Avg. Salary", icon = icon("money"), color = "green", width = NULL),
                valueBox("#10","Best Value Colleges (Payscale ROI Report)", icon = icon("line-chart"), color = "orange", width = NULL),
                valueBox("#23", "World University Rankings", icon = icon("building"), color = "red", width = NULL),
                h6("2016 Statistics"),
                img(src = "full-color-seal-min.png", height = 72, width = 72),
                img(src = "stats_fifty.png", height = 72, width = 72)
          
              )),
      # First tab content
      tabItem(tabName = "A",
              titlePanel("Post-Graduation Status by Department"),
              fluidPage(
                column(3,wellPanel(selectizeInput("year", "Choose a Year:",
                                   choices = c(2012,2013,2014)),
                    selectizeInput(
                      'e5', 'Select a Major', choices = deps,
                      selected = c("Computer Science", "Electrical & Computer Engineering", 
                                   "Mechanical Engineering", "Business Administration"),
                      multiple = TRUE, options = list(maxItems = 4)),
                    textOutput("Error", inline = T))),
                
                column(9,box(plotlyOutput("main_plot"),width=12))
                
                #box(plotlyOutput("main_plot")),
                #h1("Departmental Comparision"),
                #tags$p("On the left select the year you want to comparison for. You can currently select up to 4 departments for comparision. If a major is not available for a particular year, an error message will show up."),
                #box(title = "Messages", textOutput("Error", inline = T))

              )),
      
      # Second tab content
      tabItem(tabName = "B",
              fluidPage(
                titlePanel("Network Graphs - 2014"),
                box(p("For CMU College to Industry: Blue = College, Yellow = Industry"),width = 12),
                box(p("For Undergraduate to Graduate School and Department: Yellow = Graduate School, Blue = Undergraduate Program, Red = Graduate Program"),width = 12),
                #box(p("Hover over any node to see details on how many students were employed from each individualized college or industry. You can also use the dropdown menus."),width = 12),
                column(6,box(title = "CMU College to Industry",visNetworkOutput("network"), status = "primary", width = 12)),
                column(6,box(title = "Undergraduate to Graduate School and Department",visNetworkOutput("network_tri"), status = "primary", width = 12))
              )
      ),
      tabItem(tabName = "C",
              tabsetPanel(
                tabPanel("Post-Graduation Location",
              titlePanel("Post-Graduation Location"),
              p("Hover over the map for a breakdown by college."),
                fluidPage(
                  # Copy the chunk below to make a group of checkboxes
                  column(2,wellPanel(checkboxGroupInput("Year", label = "Year", 
                                     choices = list("2012", "2013", "2014"),
                                     selected = "2012"), 
                  checkboxGroupInput("Status", label = "Student Status" , 
                                     choices = list("Employed" = "E", "Graduate School" = "G"), 
                                     selected = c("E","G")),
                  textOutput("ErrMessage", inline = T))), 
                  column(10,box(plotlyOutput("statemap"), width = 12))
                )
              ),
              
                tabPanel("Post-Grad US Cities",
                fluidPage(
                  column(2,checkboxGroupInput("Status", label = h3("Post-Grad Status"), 
                                     choices = list("Employed" = "E", "Graduate School" = "G"),
                                     selected = c("E","G"))),
                  column(10, box(plotlyOutput("mapplot"), width = 12))
                )
              ))),
      tabItem(tabName = "D",
              fluidPage(
                h1("Which Majors/Departments do Employeers Tend to Hire From?"),
                column(3,
                       wellPanel(
                         selectInput(inputId = "employers",
                                label = "Hiring Companies",
                                choices = jobs_employers,
                                selected = c("Google", "Microsoft", "IBM"), 
                                multiple = TRUE),
                         selectInput(inputId = "dept",
                                     label = "School Department", 
                                     choices = unique(jobs$department), 
                                     selected = c("Computer Science", "Electrical & Computer Engineering", 
                                                  "Mechanical Engineering", "Business Administration"), 
                                     multiple = TRUE),
                    hr(),
                
                checkboxInput(inputId = "is_prop", 
                                  label = "Show proportional bars", 
                                  value = FALSE), 
                    
                    checkboxInput(inputId = "all_comp", 
                                  label = "Show all companies", 
                                  value = FALSE), 
                    
                    checkboxInput(inputId = "all_dept", 
                                  label = "Show all departments", 
                                  value = FALSE)
                
                    )),
                
                #  checkboxInput(inputId = "individual_obs",
                #                label = strong("Show individual observations"),
                #                value = FALSE),
                #  
                #  checkboxInput(inputId = "density",
                #                label = strong("Show density estimate"),
                #                value = FALSE),
                #  
                column(9,box(plotOutput(outputId = "employerBar", height = "400px"), width = 12)))
              ),
      tabItem(tabName = "Salaries",
              tabsetPanel(
                tabPanel("Grad Schools and Employers",
                         titlePanel("Where Carnegie Mellon Students Ended Up in 2014"),
                         fluidRow(
                           column(3,
                                  wellPanel( 
                                    selectInput("selection", "Choose Grad Schools or Employers",
                                                choices = c("Employer", "Graduate School")),
                                    
                                    actionButton("updatewords", "Change"),
                                    hr(),
                                    sliderInput(inputId = "freq",
                                                "Minimum Frequency:",
                                                min = 1,  max = 20, value = 5),
                                    
                                    sliderInput(inputId = "max",
                                                "Maximum Number of Words:",
                                                min = 1,  max = 100,  value = 50),
                                    
                                    checkboxInput(inputId = "genderwordcloud", label = strong("Subset on Gender"), 
                                                  value = FALSE)
                                  )
                           ),
                           
                           column(9,
                                  conditionalPanel("input.genderwordcloud == false",
                                                   box(title = "Wordcloud", width = 12, 
                                                       plotOutput("wordcloud_obj"))
                                  ),
                                  conditionalPanel("input.genderwordcloud == true",
                                                   box(title = "Female Wordcloud", width = 6,
                                                       plotOutput("wordcloud_female")),
                                                   box(title = "Male Wordcloud", width = 6,
                                                       plotOutput("wordcloud_male"))
                                  )
                           )
                         )
                ),
                
                tabPanel("Salary by Industry and Gender",
                         titlePanel("Annual Salary by Industry in 2014"),
                         # Salary and Industry Comparison
                         sidebarLayout(
                           sidebarPanel(
                             fluidRow(
                               column(width = 12,
                                      box(
                                        title = "Number of People Represented", width = NULL, 
                                        solidHeader = TRUE, 
                                        htmlOutput("numpeople")
                                      )),
                               selectizeInput(inputId = "industry", label = "Industry", 
                                              choices = empopts, multiple = TRUE,
                                              options = list(maxItems = 26, 
                                                             placeholder = 'Select an industry')),
                               actionButton("updatesalary", "Change"),
                               hr(),
                               checkboxInput(inputId = "genderdensity",
                                             label = strong("Subset on Gender"),
                                             value = FALSE)
                             )
                           ),
                           
                           mainPanel(
                             box(width = 12,
                                 # Salary Density
                                 plotlyOutput("salary", height = "300px")
                             )
                           )
                         )
                )
              ))
    )
  )
)

