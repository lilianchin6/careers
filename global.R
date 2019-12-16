# Salary Density data
salarydf = read.csv("data/salary2014.csv")

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), 
        substring(s, 2), sep = "", collapse = " ")
}

salarydf$employerindustry <- as.character(salarydf$employerindustry)
empopts <- c(names(table(salarydf$employerindustry)))

# Word Cloud data
employeronly = read.csv("data/employeronly2014.csv")
university = read.csv("data/university2014.csv")

employeronly$employer.school = as.character(employeronly$employer.school)
companies = subset(employeronly, select = c(gender, employer.school)) %>% rename("word" = employer.school)

university$employer.school = as.character(university$employer.school)
schools = subset(university, select = c(gender, employer.school)) %>% rename("word" = employer.school)
schools[schools$word == "Carnegie Mellon University",]$word = "Carnegie Mellon"
# Make sure there are only 20 Carnegie Mellon Students so they fit
carnegie.sub = head(schools[schools$word == "Carnegie Mellon",], 20)
carnegie.sub$gender[11:20] = "Male"
#Take out CMU students and just add 20 of them
schools = schools[schools$word != "Carnegie Mellon",]
schools = rbind(schools, carnegie.sub)