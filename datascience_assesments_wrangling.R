setwd("C:/Users/Johannes/Documents/Johannes/Data Science/Wrangling")
library(dslabs)
library(tidyverse)
library(readxl)
library(readr)
library(rvest)
library(stringr)
##Chapter 1 Read in Data
#path to data set
path <- system.file("extdata", package="dslabs")
#names of files
list.files(path)
filename <- "murders.csv"
#path and file to copy
fullpath <- file.path(path, filename)
fullpath
file.copy(fullpath, getwd())
getwd()
??file.path
library(readxl)
library(readr)

##Section 1: Import Data Assesment 
#Q1: Which readr function should be used to import this file? https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data
?`readr-package`
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
data <- read_table(url, header=F)
data <- read.csv(url, header=F)
data <- read.csv2(url, header=F)
data <- read_tsv(url, col_names = F)
??read_tsv

##Section 2: Tidy Data
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

##seperate and unite
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

#alternate approach: separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

##Section 2.1:Reshaping Data Assesment
#Question 1: Examine the built-in dataset co2. This dataset comes with 
#base R, not dslabs - just type co2 to access the dataset.
#Is co2 tidy? Why or why not?

co2
head(co2)

#Question 2
#run
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
#Use the gather() function to make this dataset tidy. Call the column 
#with the CO2 measurements co2 and call the month column month. Name the
#resulting object co2_tidy.
#Which code would return the correct tidy format?

tidy_co2 <- gather(co2_wide, month, co2, -year)

#Question 3: Use co2_tidy to plot CO2 versus month with a different curve for each year
#What can be concluded from this plot?

tidy_co2 %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

#Question 4: Load the admissions dataset from dslabs, which contains 
#college admission information for men and women across six majors, and
#remove the applicants percentage column:  
data(admissions)
dat <- admissions %>% select(-applicants)
#Your goal is to get the data in the shape that has one row for each major, like this:
#major  men   women
#A      62    82		
#B      63    68		
#C      37    34		
#D      33    35		
#E      28    24		
#F       6     7	
#Which command could help you to wrangle the data into the desired 
#format?

dat_tidy <- spread(dat, gender, admitted) 

#Question 5: Now use the admissions dataset to create the object tmp, 
#which has columns major, gender, key and value:
tmp <- gather(admissions, key, value, admitted:applicants)
tmp
#Combine the key and gender and create a new column called column_name to
#get a variable with the following values: admitted_men, admitted_women,
#applicants_men and applicants_women. Save the new data as tmp2.
#Which command could help you to wrangle the data into the desired format?

tmp2 <- unite(tmp, column_name, c(key, gender))

#Which function can reshape tmp2 to a table with six rows and five columns
#named major, admitted_men, admitted_women, applicants_men and 
#applicants_women?

tmp3 <- spread(tmp2, column_name, value)

##Section 2.1 Combining Tables Assesment
#Question 1: You have created data frames tab1 and tab2 of state population
#and election data, similar to our module videos. What are the dimensions 
#of the table dat, created by the following command? 
dat <- left_join(tab1, tab2, by = “state”)
#5x3, left_join keeps rows with info in lef table

#Question 2: We are still using the tab1 and tab2 tables shown in 
#question 1. What join command would create a new table “dat” with three
#rows and two columns? 
#Answer: semi_join

#Install and load the Lahman library. This library contains a variety of
#datasets related to US professional baseball. We will use this library 
#for the next few questions and will discuss it more extensively in the 
#Regression course. For now, focus on wrangling the data rather than 
#understanding the statistics. The Batting data frame contains the 
#offensive statistics for all baseball players over several seasons.  
#Filter this data frame to define top as the top 10 home run (HR) hitters
#in 2016:
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
#Also Inspect the Master data frame, which has demographic information for all players:
Master %>% as_tibble()
data(Master)
p_info <- as_tibble(Master)
#Question 4: Use the correct join or bind function to create a combined 
#table of the names and statistics of the top 10 home run (HR) hitters for 
#2016. This table should have the player ID, first name, last name, and 
#number of HR for the top 10 players. Name this data frame top_names.
#Identify the join or bind that fills the blank in this code to create
#the correct table: 

top_names <- top %>% left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, HR)

#Question 5: Inspect the Salaries data frame. Filter this data frame to
#the 2016 salaries, then use the correct bind join function to add a 
#salary column to the top_names data frame from the previous question. 
#Name the new data frame top_salary. Which bind or join function fills 
#the blank to generate the correct table?
head(Salaries)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names)%>%
  select(nameFirst, nameLast, teamID, HR, salary)

#Question 6: Inspect the AwardsPlayers table. Filter awards to include 
#only the year 2016. How many players from the top 10 home run hitters
#won at least one award in 2016?

head(AwardsPlayers)
ap <- filter(AwardsPlayers, yearID=="2016")
intersect(ap$playerID, top$playerID)

#How many players won an award in 2016 but were not one of the top 10 
#home run hitters in 2016?

setdiff(ap$playerID, top$playerID)

##Section 2.3: Web scraping Assesment
library(rvest)

#set url
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"

#read html file
h <- read_html(url)

#extract table by node
nodes <- html_nodes(h, "table")
class(nodes)

#view content of arbitrarily node component
html_text(nodes[8])

#Question 1: Many tables on this page are team payroll tables, with 
#columns for rank, team, and one or more money values.
#Convert the first four tables in nodes to data frames and inspect them.
#Which of the first four nodes are tables of team payroll?
tab1 <- html_table(nodes[[1]])
tab2 <- html_table(nodes[[2]]) #payroll table
tab3 <- html_table(nodes[[3]]) #payroll table
tab4 <- html_table(nodes[[4]]) #payroll table
class(tab4)

#Question 2: For the last 3 components of nodes, which of the following
#are true? (Check all correct answers.)
tab19 <- html_table(nodes[[19]])
tab20 <- html_table(nodes[[20]])
tab21 <- html_table(nodes[[21]])
#all 3 tables
#last one not payroll but average

#Question 3: Create a table called tab_1 using entry 10 of nodes. Create
#a table called tab_2 using entry 19 of nodes. Note that the column names
#should be c("Team", "Payroll", "Average"). You can see that these column
#names are actually in the first data row of each table, and that tab_1
#has an extra first column No. that should be removed so that the column
#names for both tables match.
#Remove the extra column in tab_1, remove the first row of each dataset,
#and change the column names for each table to c("Team", "Payroll", "Average"). 
#Use a full_join() by the Team to combine these two tables.
#How many rows are in the joined data table?

tab1 <- html_table(nodes[[10]], header = T)
tab1 <- tab1 %>% select(-"No.")
tab2 <- html_table(nodes[[19]], header = T)
tab <- full_join(tab1, tab2, by="Team")
#Answer: 58

#Question 4: The Wikipedia page on opinion polling for the Brexit referendum
#External link, in which the United Kingdom voted to leave the European
#Union in June 2016, contains several tables. One table contains the results
#of all polls regarding the referendum over 2016.
#Use the rvest library to read the HTML from this Wikipedia page (make sure
#to copy both lines of the URL):

url_brexit <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
b <- read_html(url_brexit)
nodes_b <- html_nodes(b, "table")
tab_b <- nodes_b %>% html_table(fill=T)
#answer: 41

?html_table

#Question 5: Inspect the first several html tables using html_table()
#with the argument fill=TRUE (you can read about this argument in the 
#documentation). Find the first table that has 9 columns with the first 
#column named "Date(s) conducted".
#What is the first table number to have 9 columns where the first column
#is named "Date(s) conducted"?

tab6 <- html_table(nodes_b[[6]], fill=T)


##section 3: String Processing
#basic codes
#detect pattern (4 or 5 small letters) in string
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "moo*"
str_detect(animals, pattern)

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

yes <- c("5 feet 7inches", “5 7”)
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)
[1]  TRUE TRUE FALSE FALSE
#repair with
converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
#allows for none or more spaces between characters

#seperate with regex
#first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

#string processing: using groups and quantifiers
#case 1
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")
#The pattern says it has to start (^), be followed with a digit between
#4 and 7, and then end there ($). The parenthesis defines the group that
#we pass as \\1 to the replace regex.

#case 2
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
#repair different formats

#example as a function
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

#convert words to numbers
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

#find remaining problematic cases
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

#putting all together
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)
#start by cleaning up the height column so that the heights are closer to a feet'inches format
#original heights added for comparison

#check all coverted heights
ew_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

#list shortest
new_heights %>% arrange(height) %>% head(n=7)

##string processing: extracting table from pdf
#downloading the PDF document then importing it into R using the following code
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
#character vector with an entry for each page. So we keep the page we want using the following code
raw_data_research_funding_rates <- txt[2]

raw_data_research_funding_rates %>% head

#create a list with the lines of the text as elements, sep="\n"
tab <- str_split(raw_data_research_funding_rates, "\n")





#test Joe
a <- c("43.555,76", "44.898,76")
as.numeric(a)
repair <- function(x){
  x <- x %>% str_replace("[.]", "") %>% str_replace("[,]", ".") %>% as.numeric(x, dec=".")
  class(x)
} 
sapply(a, repair)
class(a)
a
mean(a)

yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
