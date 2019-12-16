#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggforce)
library(ggmap)
library(MASS)
library(scales)
library(tidyverse)
library(visNetwork)

cpdc <- read_csv("data/clean.csv")

#Network 1: Bipartite
nodes1 = read.csv("data/NodesDF.csv")
edges1 = read.csv("data/edgesDF.csv")

#Network 2: Tripartite
nodes2 = read.csv("data/Nodes_tripartite.csv")
edges2 = read.csv("data/Edges_tripartite.csv")

# change categories to full name
levels(cpdc$status) <- c(levels(cpdc$status), "Employed","Freelance", "Graduate School",
                         "Military", "No Response", "Not Seeking", "Return to Home Country", 
                         "Seeking", "Volunteering")
cpdc$status[cpdc$status == 'E'] <- 'Employed'
cpdc$status[cpdc$status == 'F'] <- 'Freelance'
cpdc$status[cpdc$status == 'G'] <- 'Graduate School'
cpdc$status[cpdc$status == 'M'] <- 'Military'
cpdc$status[cpdc$status == 'NR'] <- 'Other'#No response
cpdc$status[cpdc$status == 'NS'] <- 'Other' #Not seeking
cpdc$status[cpdc$status == 'R'] <- 'Other' # return to home country
cpdc$status[cpdc$status == 'S'] <- 'Seeking'
cpdc$status[cpdc$status == 'V'] <- 'Volunteering'



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

  output$main_plot <- renderPlotly({
    
    #allInputs <- eventReactive(input$go, {
      output$txt2Test <- renderText({ input$year })
      output$yah <- renderText({length(input$e5)})
      years <-input$year
      e5s <- input$e5
      #print(length(e5s))
      #output$plot <- renderPlotly({
        # Create Bar Chart
        # Remove Other and NA columns
        cpdc <- cpdc[-(which(is.na(cpdc$department) | cpdc$department== "Other")),]
        dep_status_prev <- subset(cpdc, cpdc$year == years)
        dep_status <- subset(dep_status_prev, dep_status_prev$department == e5s[1]) %>% 
          group_by(status) %>% 
          summarize(count = n())
        sum_total <- sum(dep_status$count)
        # FIRST MAJOR
        p <- plot_ly(dep_status, x = ~status, y = ~count/sum_total*100, type = 'bar', name = e5s[1], 
                     marker = list(color = 'rgb(49,130,189)'))
        if (length(e5s) > 1){ # SECOND MAJOR
            print(length(e5s))
            dep_status_prev1 <- subset(cpdc, cpdc$year == years)
            dep_status1 <- subset(dep_status_prev1, dep_status_prev1$department == e5s[2]) %>% 
              group_by(status) %>% 
              summarize(count = n())
            sum_total1 <- sum(dep_status1$count)
            print(dep_status1$count/sum_total1*100)
            if(nrow(dep_status1) != 0){
              p <- p %>% add_trace(y=~dep_status1$count/sum_total1*100, x=~dep_status1$status, type="bar", name = e5s[2],
                                   marker = list(color = 'rgb(204,204,204)'))
            }
            else{
              output$Error <- renderText({paste("Not enough data for ",e5s[2])})
            }
            
        }
        
        if (length(e5s) > 2){ # THRID MAJOR
          print(length(e5s))
          print("bye")
          dep_status_prev1 <- subset(cpdc, cpdc$year == years)
          dep_status3 <- subset(dep_status_prev1, dep_status_prev1$department == e5s[3]) %>% 
            group_by(status) %>% 
            summarize(count = n())
          sum_total3 <- sum(dep_status3$count)
          print(dep_status3$count/sum_total3*100)
          if(nrow(dep_status3) != 0){
            p <- p %>% add_trace(y=~dep_status3$count/sum_total3*100, x=~dep_status3$status, type="bar", name = e5s[3],
                               marker = list(color = 'red'))
          }
          else{
            output$Error <- renderText({paste("Not enough data for ",e5s[3])})
          }
        }
        
        if (length(e5s) == 4){
          print(length(e5s))
          print("hi")
          dep_status_prev4 <- subset(cpdc, cpdc$year == input$year)
          dep_status4 <- subset(dep_status_prev4, dep_status_prev4$department == e5s[4]) %>% 
            group_by(status) %>% 
            summarize(count = n())
          sum_total4 <- sum(dep_status4$count)
          print(dep_status4$count/sum_total1*100)
          if(nrow(dep_status4) != 0){
            p <- p %>% add_trace(y=~dep_status4$count/sum_total4*100, x=~dep_status4$status, type="bar", name = e5s[4],
                               marker = list(color = 'orange'))
          }
          else{
            output$Error <- renderText({paste("Not enough data for ",e5s[4])})
          }
        
        }
        
          p %>% layout(title = paste("What are they Doing? - Class of ",years, sep = " "),
                       xaxis = list(title = "After Graduation Status", tickangle = -45),
                       yaxis = list(title = "Percentage (%)"),
                       margin = list(b = 100),
                       barmode = 'group')
          
      #})
      
      #})
    #allInputs()
})
  
  output$network <- renderVisNetwork({
    visNetwork(nodes1, edges1, height = "100%", width = "100%") %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 selectedBy = "group",nodesIdSelection = TRUE) 
    })
  
  output$network_tri <- renderVisNetwork({
    visNetwork(nodes2, edges2, height = "100%", width = "100%") %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 selectedBy = "group",nodesIdSelection = TRUE) 
  })
  
  ### ANDREW: MAP
  output$statemap <- renderPlotly({
    years <- input$Year
    status1 <- input$Status
    print(input$Year)
    # Subset by each year, 1 = 2012, 2 = 2013, 3 = 2014
    career.data.e <- cpdc
    
    output$ErrMessage = renderText("")
    if (length(years) == 0){
      output$ErrMessage = renderText("Select at least one year")
    }
    
    career.data.e <- subset(career.data.e, year %in% years)
    #career.data.e <-subset(career.data.e, status %in% status1)
    
    date = paste(years, collapse = ", ")
    
    
    # Employment count by state/country
    state.size.e <- career.data.e %>%
      group_by(state.country) %>%
      summarize(state_size = n())
    
    # Employment by school
    school.size.e <- career.data.e %>%
      group_by(college) %>%
      summarize(school_size = n())
    
    career.state <- left_join(career.data.e, state.size.e, by = c("state.country" = "state.country"))
    career.state <- left_join(career.state, school.size.e, by = c("college" = "college"))
    
    # Add 0 States
    missing_states <- c()
    print ("works")
    for (i in 1:length(state.abb)){
      
      if (state.abb[i] %in% unique(career.state$state.country) == FALSE)
        missing_states <- append(missing_states, state.abb[i])
    }
    
    
    state.ind <- which(colnames(career.state) == "state.country")
    size.ind <-  which(colnames(career.state) == "state_size")
    print(length(missing_states))
    for (i in 1:length(missing_states)){
      x <- c(rep(NA, size.ind))
      x[state.ind] <- missing_states[i]
      x[size.ind] <- 0
      print(x)
      career.state <- rbind(career.state, x)
    }
    career.state$state_size <- as.integer(career.state$state_size)
    print("still works")
    # Add state names
    names_states <- data.frame(state.abb, state.name)
    career.state <- left_join(career.state, names_states, by = c('state.country' = 'state.abb'))
    
    school_size <- as.data.frame(matrix(nrow = length(unique(career.data.e$state.country)), 
                                        ncol = 1))
    colnames(school_size) <- "state.country"
    school_size$state.country <- unique(career.data.e$state.country)
    schools <- c("BHA", "BXA", "CFA", "CIT", "Dietrich", 
                    "MCS", "SCS", "SHS", "Tepper") 
    print ("all good")
    
    for (i in 1:length(schools)){
      state_set <- subset(career.data.e, college == schools[i]) %>%
        group_by(state.country) %>%
        summarize(school_n = n())
      
      school_size <- left_join(school_size, state_set, by = c("state.country" = "state.country"))
    }
    
    # Rename columns of data frame
    colnames(school_size)[2:ncol(school_size)] <- schools
    print("hmmm")
    # Replace NAs with 0
    #for (i in 1:nrow(school_size)){
    #  print(i)
    #  for (j in 2:10){
    #    print(j)
    #    if (is.na(school_size[i,j]) == TRUE)
    #      school_size[i,j] <- 0
    #  }
    #}
    school_size[is.na(school_size)] <- 0
    print(school_size)
    print("here")
    
    career.state <- left_join(career.state, school_size, by = c("state.country" = "state.country"))
    print("afabs")
    print(career.state)
    # Replace NAs for missing states
    for (i in 1:nrow(career.state)){
      for (j in 26:ncol(career.state)){
        if (is.na(career.state[i,j] == TRUE))
          career.state[i,j] <- 0
      }
    }
    print("eric")
    
    
    
    career.state$hover <- with(career.state, paste(state.name, '<br>', 
                                                   "BHA:", BHA,'<br>',"BXA:", BXA, '<br>',
                                                   "CFA:", CFA,'<br>',"CIT:", CIT, '<br>',
                                                   "Dietrich:", Dietrich,'<br>',"MCS:", MCS, '<br>',
                                                   "SCS:", SCS,'<br>',"SHS:", SHS, '<br>',
                                                   "Tepper:", Tepper
    ))
    
    # career.state$hover <- with(career.state, c(schools, state.name))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    p <- plot_geo(career.state, locationmode = 'USA-states') %>%
      add_trace(
        z = ~state_size, text = ~hover,
        locations = ~state.country,
        color = ~state_size, colors = 'Reds'
      ) %>%
      colorbar(title = "Number of Students", 
               limits = c(min(career.state$state_size, na.rm = T), 
                          max(career.state$state_size, na.rm = T))) %>%
      layout(
        title = paste(date ,"Post-Grad States"),
        geo = g
      )
    p
  })
  
  #####
  ## ANDREW MAP 2
  #####
  output$mapplot <- renderPlotly({
    # Employer 2014 data
    statuses <- input$Status 
    # Check if E, G, or E and G
    car <- read_csv('data/employer2014_G_E.csv')
    car <- subset(car, status %in% statuses)
    # Get city coordinates
    
    city_size <- car %>%
      group_by(city) %>%
      summarize(city_size = n())
    
    
    
    
    # city_size <- left_join(city_size, city_coord, by = c("city" = "name"))
    
    career_city_e <- left_join(car, city_size, by = c("city" = "city"))
    
    
    # Find list of all the employers in a given city
    city_name <- unique(career_city_e$city)
    
    employer_df<- as.data.frame(matrix(nrow = length(unique(career_city_e$city)), ncol = 1))
    colnames(employer_df) <- "city"
    employer_df$city <- unique(career_city_e$city)
    employer <- unique(career_city_e$employer.school)
    
    
    # Find total of employers in each city
    employment_number <- career_city_e %>%
      group_by(city) %>%
      summarize(employer_tot = length(unique(employer.school)))
    
    career_city_e <- left_join(career_city_e, employment_number,
                               by = c("city" = "city"))
    
    
    # Find each individual employer
    for (i in 1:length(employer)){
      city_set <- subset(career_city_e, employer.school  == employer[i]) %>%
        group_by(city) %>%
        summarize(employer_n = n())
      
      employer_df <- left_join(employer_df, city_set, by = c("city" = "city"))
    }
    colnames(employer_df)[2:ncol(employer_df)] <- employer
    
    
    # Find list of every company name
    employers <- as.data.frame(employer_df$city)
    employers$company <- rep(0, length(employers))
    
    for (i in 1:nrow(employer_df)){
      sorted_employers <- sort(employer_df[i, 2:ncol(employer_df)], decreasing = TRUE)
      
      # Check if there are any companies at a given city
      if (length(sorted_employers) == 0){
        employers$company[i] = 0
      }
      
      # If less than 5, return the top x employers
      else if (length(sorted_employers) < 5){
        x <- colnames(sorted_employers)[1:ncol(sorted_employers)]
        employers$company[i] <- list(x)
      }
      
      # If greater or equal to 5, return the top 5 employers
      else if (length(sorted_employers) >= 5){
        x <- colnames(sorted_employers)[1:5]
        employers$company[i] <- list(x)
      }
      
      for (ii in 1:length(employers$company))
        employers$company[[ii]] <- paste(employers$company[[ii]], collapse = ", ")
    }
    
    
    
    career_city_e <- left_join(career_city_e, employers, by = c("city" = "employer_df$city"))
    
    # career_city_e <- left_join(career_city_e, state_coord, by = c("state.country" = "state.abb"))
    
    # geo styling
    g2 <- list(
      scope = 'usa',
      projection = list(type = 'Mercator'),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("black"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    p2 <- 
      plot_geo(career_city_e, lat = ~lat, lon = ~long) %>%
      add_markers(
        text = ~paste(city, state.country, city_size, company, sep = "<br />"),
        color = ~employer_tot, symbol = I("circle"), size =~city_size, 
        hoverinfo = "text"
      ) %>%
      colorbar(title = "Number of Companies/Schools") %>%
      layout(
        title = 'US Cities by Number of 2014 Post-Grads <br />(Hover for top companies)', geo = g2
      )
    p2
    
  })
  
  
  ######
  ## KIRESTEN BAR CHART
  ######
  jobs <- read_csv("data/clean.csv")
  no_school <- grep("niversity|ollege|CMU|Carnegie|Massachusetts Institute|Not Reported|unknown", 
                    jobs$employer.school)
  jobs <- jobs[-no_school, ]
  jobs_employers <- names(head(sort(table(jobs$employer.school), decreasing = TRUE), n = 50))
  jobs <- subset(jobs, jobs$employer.school %in% jobs_employers)
  jobs$employer.school[grep("Morgan Chase", jobs$employer.school)] <- "JP Morgan Chase"
  # Colors
  my_colors <- colors()[367:657]
  choose_colors <- function(n) {
    colors <- seq(from = 1, to = length(my_colors), length.out = n)
    return(my_colors[colors])
  }
  # Theme
  my_theme <- 
    theme_grey() + 
    theme(plot.title = element_text(size = 20, 
                                    colour = "black", 
                                    family = "Helvetica"),
          text = element_text(colour = "black", 
                              family = "Helvetica", 
                              size = 16),
          axis.line = element_line(colour = "black"), 
          panel.background = element_rect(fill = "white"))
  
  # first plot: bar chart of employers vs department
  output$employerBar <- renderPlot({
    
    # determine dataset
    jobs <- jobs[-(which(is.na(jobs$department) | jobs$department== "Other")),]
    jobs_1 <- subset(jobs, employer.school %in% input$employers & department %in% input$dept)
    if (input$all_comp) { # look at all available companies
      jobs_1 <- subset(jobs, department %in% input$dept)
    } else if (input$all_dept) { # look at all available depts
      jobs_1 <- subset(jobs, employer.school %in% input$employers)
    } 
    
    n_dept <- length(unique(jobs_1$department))
    
    # printing graphic
    if (input$all_comp & input$all_dept) {
      base <- 
        ggplot(jobs_1) + 
        labs(title = "Too much information! Please check only one box.")
    } else {
      base <-
        ggplot(jobs_1, aes(x = employer.school, fill = department)) + 
        geom_bar() + 
        labs(title = "Employers by Hired Departments", 
             x = "Employer", 
             y = "Number of Hired Students", 
             fill = "Department") + 
        my_theme + 
        scale_fill_manual(values = choose_colors(n_dept)) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    }
    
    # make a proportional plot
    if (input$is_prop) {
      base <- 
        ggplot(jobs_1, aes(x = employer.school, fill = department)) + 
        geom_bar(position = "fill") + 
        labs(title = "Employers by Hired Departments", 
             x = "Employer", 
             y = "Proportion of Hired Students", 
             fill = "Department") + 
        my_theme + 
        scale_fill_manual(values = choose_colors(n_dept)) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    }
    
    
    #    # Add rug
    #    if (input$individual_obs) {
    #      base <- base + geom_rug()
    #    }
    #    
    #    # Add density plot
    #    if (input$density) {
    #      base <- base + geom_density(adjust = input$bw_adjust, colour = "dark red")
    #    }
    print(base)
  })
  
  ######
  ## LILIAN GRAPHS
  ######
  
  # 391 people with a salary and with an industry listed 391 people
  salarydf = read.csv("data/salary2014.csv")
  salarydf$employerindustry <- as.character(salarydf$employerindustry)
  numsalary = nrow(salarydf)
  numrepresented = paste(as.character(numsalary))
  
  
  makesalarydf <- reactive({
    # Change when the "update" button is pressed...
    input$updatesalary
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing density plot...")
        if (is.null(input$industry)) {
          df = salarydf
        } else {
          industries = sapply(input$industry, function(x) {paste("'", x, "'", sep="")})
          textlogical = paste(paste("employerindustry ==", industries), collapse = " | ")
          smalldf = subset(salarydf, eval(parse(text = textlogical)))
          numsalary <- nrow(salarydf)
          numstud <- nrow(smalldf)
          df = smalldf
        }
      })
    })
  })
  
  makesalarytitle <- reactive({
    input$updatesalary
    isolate({
      if (is.null(input$industry)) {
        plottitle = "Annual Salary for All Industries"
      } else if (length(input$industry) == 1) {
        name <- simpleCap(input$industry)
        plottitle = paste("Annual Salary for", name, "Industry")
      } else {
        plottitle = "Annual Salary for Industries Listed"
      }
      
    })
  })
  
  # rug plot, but also change the hover to show people
  output$salary <- renderPlotly({
    df <- makesalarydf()
    plottitle <- makesalarytitle()
    if (input$genderdensity) {
      base <-
        ggplot(data = df, aes(x = annualsalary, fill = gender)) +
        geom_density(alpha = 0.7) + geom_rug(col="black",alpha=.2) + 
        scale_fill_manual(values=c("#800000", "#3f34a5")) + 
        labs(
          title = plottitle,
          x = "Annual Salary ($)",
          y = "Proportion of Students",
          fill = "Gender"
        ) 
      base <- ggplotly(base) %>% layout(legend = list(x = 0, y = 1), yaxis = list(title = ""))
    } else {
      base <- ggplot(df, aes(x = annualsalary)) +
        geom_density(fill = "darkred", alpha = 0.7) + geom_rug(col="black",alpha=.2) + 
        labs(
          title = plottitle,
          x = "Annual Salary ($)",
          y = "Proportion of Students"
        ) 
    }
  })
  
  output$numpeople <- renderText({
    df = makesalarydf()
    nrow(df)
    HTML(paste0("<b><center><font size='6'>", as.character(nrow(df)),"</font></center></b>"))
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  makewords <- reactive({
    # Change when the "update" button is pressed...
    input$updatewords
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        if (input$selection == "Employer") {
          wordmatrix <- companies
          wordmatrix
        } else if (input$selection == "Graduate School") {
          wordmatrix <- schools
          wordmatrix
        }
      })
    })
  })
  
  output$wordcloud_obj <- renderPlot({
    countword = makewords()
    # Make the original word cloud
    wordcloudmat <- countword %>% count(word)
    if (input$freq <= max(wordcloudmat$n)) {
      wordcloudmat %>%
        with(wordcloud_rep(word, n, scale = c(4, 0.5), 
                           min.freq = input$freq, max.words = input$max, 
                           random.order = FALSE, random.color = FALSE,
                           colors=brewer.pal(8, "Reds")[4:8]))
    }
  })
  
  output$wordcloud_female <- renderPlot({
    if (input$genderwordcloud) {
      # Make the male word cloud here
      countword = makewords()
      wordmat = countword %>% subset(gender == "Female") %>% count(word)
      if (input$freq <= max(wordmat$n)) {
        wordcloud_obj <-
          wordmat %>% with(wordcloud_rep(word, n, scale = c(4, 0.5), 
                                         min.freq = input$freq, max.words = input$max, 
                                         random.order = FALSE, random.color = FALSE,
                                         colors=brewer.pal(8, "YlOrRd")[4:8]))
      }
    }
  })
  
  output$wordcloud_male <- renderPlot({
    if (input$genderwordcloud) {
      # Make the male word cloud here
      countword = makewords()
      wordmat = countword %>% subset(gender == "Male") %>% count(word)
      if (input$freq <= max(wordmat$n)) {
        wordcloud_obj <-
          wordmat %>% with(wordcloud_rep(word, n, scale = c(4, 0.5), 
                                         min.freq = input$freq, max.words = input$max, 
                                         random.order = FALSE, random.color = FALSE,
                                         colors=brewer.pal(8, "PuBuGn")[4:8]))
      }
    }
  })
  })
  
  
  
  

