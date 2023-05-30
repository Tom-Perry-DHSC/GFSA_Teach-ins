###########################################################
# ---------------------------------------------------------
#         GFSA Programming Teach-ins - R - Series 1        
# ---------------------------------------------------------
###########################################################

######## Some really useful guides for learning R:
########    R for Data Science - Hadley Wickham & Garrett Grolemund: https://r4ds.had.co.nz/r-markdown.html
########    R Cheatsheets - Posit: https://posit.co/resources/cheatsheets/
########    Introduction to R - Analysis Function: https://learninghub.ons.gov.uk/course/view.php?id=498

###########################
# -------------------------
#         Session 1        Introduction to R Studio and Data Types in R
# -------------------------
###########################

# ---------------------------------------------------------
# Section 1: Overview of RStudio
# ---------------------------------------------------------

######## Text here

# ---------------------------------------------------------
# Section 2: Variable assignment in R
# ---------------------------------------------------------

######## Variables are assigned with either <- or =
######## The two lines below do the same thing - assign the value 7 to the variable
########   called num_colours_in_rainbow.

num_colours_in_rainbow <- 7
num_colours_in_rainbow =  7

######## Use the print function to see the current value of a variable

print(num_colours_in_rainbow) # Prints 7

######## Assigning a new value to a variable will overwrite the old value:

num_planets_in_solar_system <- 9
print(num_planets_in_solar_system) # Prints 9
num_planets_in_solar_system <- 8
print(num_planets_in_solar_system) # Prints 8

# ---------------------------------------------------------
# Section 3: Data types in R
# ---------------------------------------------------------

######## R has 6 so-called basic data types:
########   logical, numeric, integer, complex, character, and raw

######## You can use the "class" function to check the class of a variable:

print(class(num_colours_in_rainbow)) # Prints "numeric"

# ---------------------------------------------------------
# Section 3a: Logical variables
# ---------------------------------------------------------

######## logical variables take one of two possible values, TRUE or FALSE. They're
########   often used for indexing/filtering and if statements.

operational_research_is_cool <- TRUE
print(operational_research_is_cool) # Prints TRUE

######## Key operations on logicals are: NOT (!), AND (&), and OR (|).

print(!TRUE)          # Prints FALSE
print(TRUE  & TRUE)   # Prints TRUE
print(TRUE  & FALSE)  # Prints FALSE
print(TRUE  | FALSE)  # Prints TRUE
print(FALSE | FALSE)  # Prints FALSE

# ---------------------------------------------------------
# Section 3b: Numeric variables
# ---------------------------------------------------------

######## Numeric variables can be used for both integers and decimals:

a <- 21
b <- 2.1
c <- 2.1e-1

d <- -1

######## Numbers can be added, multiplied etc:

print(1 + 2)   # Addition
print(12 - 15) # Subtraction
print(4 * 5.5) # Multiplication
print(21 / 3)  # Division

print(2^3)     # Exponentiation

######## While R does have a set order of operations, using parentheses minimises
########    the risk of unexpected behaviour.

print(1/3^2 + 5 * 2)     # Not clear what is intended.
print(1/(3^2) + (5 * 2)) # Same result, but much clearer.

######## There also many standard functions:

print(log(10)) 	       # Defaults to the natural log (base e)
?log
print(log(10, base=2)) # Base can be declared
print(sin(pi/2))       # pi is a defined constant
print(sqrt(9))

print(floor(2.9))       # Rounds down to nearest whole number
print(ceil(2.1))        # Rounds up to nearest whole number
print(round(2.4))       # Rounds up or down to nearest whole number
print(round(2.451515, digits=3))  # Can optionally choose number of decimal places
print(round(31415, digits=-1))    # Including negative values for e.g. to nearest ten

# ---------------------------------------------------------
# Section 3c: Integer variables
# ---------------------------------------------------------

######## In many instances, you can use "numeric" to represent integers, but where memory
########    or precision is particularly important, you may want to use the integer data type.
######## To do so, write an upper case 'L' after the number.

a <- 2L

######## Note that division will always output a "numeric", even when the result
########    is a whole number:

print(class(2L / 2L)) # Prints "numeric"

######## Use as.integer() to force conversion to an integer:

print(class(as.integer(2L / 2L))) # Prints "integer"

######## Note also that decimals will always be rounded towards 0 when using as.integer():

print(as.integer(1.5))  # Prints 1
print(as.integer(-1.5)) # Prints -1

# ---------------------------------------------------------
# Section 3d: Complex and Raw variables
# ---------------------------------------------------------

######## Complex and raw variables won't be covered here.

# ---------------------------------------------------------
# Section 3e: Character variables
# ---------------------------------------------------------

######## These are equivalent to what is often called a "string" in other programming languages,
########    and are the main way you'll interact with text-based data in R.
######## Double quotes are used when defining a character variable:

the_wheels_on_the_bus_go <- "round and round"
sky_colour <- "blue"

######## Beware backslash (\), R's escape character.
######## This is used to tell R that the following character should be treated specially.
######## If you want an actual backslash, you need to use two:

my_text <- "It's a yes\no question." # Wrong - R will see "It's a yes{newline}o question."
my_text <- "It's a yes\\no question." # Right

######## There are lots of "special" characters that can be accessed in this way, e.g. \n, \t, but they're
######## beyond the scope of this course.

######## There are many useful string functions in base r.

favourite_colour <- "orange"

######## Use paste() to combine multiple character variables. The 'sep' argument is a string that will
########    be inserted between each component. If no 'sep' is given, the default is a space.

favourite_colour_sentence <- paste("My favourite colour is ", favourite_colour, ".", sep = "")

######## substr(input, start, stop) can be used to extract parts of a string:

print(substr(favourite_colour, 2, 4)) # Prints "ran"

######## Use nchar() to get the length of a character variable. (Not length() - this is for vectors!)

print(nchar(favourite_colour)) # Prints 5

######## Other functions like grep(), gsub(), sprintf() can be very handy, but are a bit more complex.

# ---------------------------------------------------------
# Section 4: Comparisons
# ---------------------------------------------------------

######## Basic variables can be compared.
######## Checking for equality uses "==" (not "=" - that is only used for variable assignment)

print(1 == 1) # Prints TRUE
print("hello" == "goodbye") # Prints FALSE

######## Non-equality can be checked with "!=":

print(1 != 1) # Prints FALSE
print("hello" != "goodbye") # Prints TRUE

######## We also have <, >, <=, >=, which behave expectedly on numerical variables:

print(1 > 2) # Prints FALSE
print(100 <= 100) # Prints TRUE

######## They can also be used on character variables, comparing via lexicographic ordering.
######## Before using, it's worth doing some simple test cases that these behave as you'd expect
######## when it comes to e.g. punctuation, or upper- vs lower-case.

print("cat" < "dog") # Prints TRUE

## In addition to the basic data types, we will also look at vectors, lists, and dataframes.
## (And possibly tibbles in later sessions.)

# ---------------------------------------------------------
# Section 5: Larger Data Types
# ---------------------------------------------------------

# ---------------------------------------------------------
# Section 5a: Vectors
# ---------------------------------------------------------

######## A vector in R is an ordered set of basic variables, all of which have the same type.
######## In fact, all the variables we have seen so far have secretly been vectors of length one!
######## Vectors can be defined explicitly using c():

planets <- c("mercury", "venus", "earth", "mars", "jupiter", "saturn", "uranus", "neptune")
numbers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
clock_sounds <- c("tick", "tock", "tick", "tock", "tick", "tock")

######## As noted above all basic variables are vectors of length one,
########    so the two lines below do the same thing.

sky_colour <- "blue"
sky_colour <- c("blue")

######## Sometimes explicit definitions would be tedious to write out. There are some shortcuts:

numbers <- 1:10 # Makes a vector of the numbers from 1 to 10 (inclusive)
clock_sounds <- rep(c("tick", "tock"), 3) # REPeats the vector c("tick", "tock") 3 times

######## Use length() to check the length of a vector

print(length(planets)) # Prints "8"

######## You can get elements of a vector by indexing, using [] notation.
######## Note that R, unlike many languages, uses 1-indexing rather than 0-indexing.
######## This means that the first element of a vector has index 1.

print(planets[1]) # Prints "mercury"
planet_4 <- planets[4]
print(planet_4)   # Prints "mars"

# ---------------------------------------------------------
# Section 5b: Index positions
# ---------------------------------------------------------

######## You can also index multiple elements at once. There are two main ways of doing this - with
########    a vector of "index positions", and with a vector of logicals.

outer_solar_system_planet_indices <- c(5, 6, 7, 8)
outer_solar_system_planet_names <- planets[outer_solar_system_planet_indices]

######## Or more simply:

outer_solar_system_planet_names <- planets[5:8]
print(outer_solar_system_planet_names) # Prints the planets in the outer solar system

######## Logical indexing:

planets_with_moons <- c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
print(planets[planets_with_moons]) # Prints everything but Mercury and Venus.

###### You'll rarely write out your own list of logicals, but this approach is very useful
###### when combined with functions that output logicals.

# ---------------------------------------------------------
# Section 5c: Vectors and functions
# ---------------------------------------------------------

######## Almost all functions/operations can act element-wise on vectors, outputting a vector:

print(c(1, 2, 3) + c(1, -2, 5.2)) # prints the vector c(2, 0, 8.2)

planets <- c("mercury", "venus", "earth", "mars", "jupiter", "saturn", "uranus", "neptune")
planet_start_letters <- substr(planets, 1, 1) # Gives the vector c("m", "v", "e", "m", "j", "s", "u", "n")

planet_descriptions <- c("small", "hot", "where humans live", "red", "big", "ringed", "icy", "very far away")
planet_sentences <- paste(toupper(planet_start_letters), substr(planets, 2, nchar(planets)), " is ", planet_descriptions, ".", sep="")

######## Hidden in the last example is the fact that R can handle operations involving vectors of different lengths, but only when
######## there is one longest vector whose length is (pairwise) a multiple of all other vector lengths in the expression.

print(c(1, 2, 3) + 1) # Prints (2, 3, 4)
print(c(1, 2, 3, 4) + c(0, 1)) # Prints (1, 3, 3, 5)
print(c(1, 2, 3) + c(0, 1)) # Returns an error - neither vector length is a multiple of the other.

######## This behaviour is mostly useful for combining 1-length vectors with vectors of other lengths.
######## For other cases, be sure that the behaviour is what you're expecting it to be!

######## In R, a vector can't be "higher dimensional" - unlike other languages, it's not possible
########    to have a vector with vector elements.
######## Making a vector of vectors instead concatenates the vectors:

even_numbers <- c(2, 4, 6)
odd_numbers <- c(1, 2, 3)

all_numbers <- c(odd_numbers, even_numbers)

######## Vectors can be sorted:

sorted_numbers <- sort(all_numbers)
sorted_numbers <- all_numbers[order(all_numbers)] # Useful for e.g. sorting a dataframe on one column.

# ---------------------------------------------------------
# Section 5d: Lists
# ---------------------------------------------------------

######## Lists behave similarly to vectors in many ways.
######## Unlike vectors, they can contain different variable types as entries:

my_list <- list(1, "two", FALSE)

######## Like vectors, they can be indexed to retrieve elements. Note that standard indexing will always return a list:

element_2 <- my_list[2] # Creates a new list of length 1, equal to list("two")
print(class(element_2)) # Prints "list"
multiple_elements <- my_list[2:3] # Creates a new list of length 2, equal to list("two", FALSE)

######## To get the actual element at an index, use double brackets:

element_2 <- my_list[[2]]
print(class(element_2)) # Prints "character"

######## Double brackets accept only a single index - what would it mean to return multiple elements of the list not as a list?

multiple_elements <- my_list[[1:2]] # Will cause an error.

######## Lists can be nested:

multi_dimensional <- list(list("a", "b"), list("c", "d"))
element_1 <- multi_dimensional[[1]] # Returns the list given by list("a", "b")
print(element_1[[2]]) # Prints "b"

######## With one less step:

print(multi_dimensional[[1]][[2]]) # Prints "b".

######## Both lists and vectors can optionally be "named".
######## Naming lets you access elements with strings as well as their indices.
######## This acts like a dictionary object in many other programming languages.

animal_info <- c(type="dog", noise="woof") # Left hand side of "=" sign is the name of the element; the right hand side is the value
print(animal_info["type"]) # Prints "dog"
print(animal_info[1]) # Can still use numerical indices.

print(names(animal_info)) # Prints c("type", "noise")

# ---------------------------------------------------------
# Section 5e: Dataframes
# ---------------------------------------------------------

######## Dataframes are the main way of handling tabular information in base R.
######## They are structured as having rows and columns.
######## Rows are often called "observations"; columns are often called "variables".
######## One column can have different data types to others, but all variables within a column must have a single data type.

# Here, we create a dataframe with three columns. "type" and "noise" hold character variables, while "legs" has numeric.
# Note that all columns must have the same length.

animal_info_df <- data.frame(
  type  = c("dog", "fish", "chicken", "cat"),
  noise = c("woof", "blub", "cluck", "meow"), 
  legs  = c(4, 0, 2, 4)
)

######## Dataframes can be indexed similarly to vectors and lists, but now two dimensions must be given:
######## First the row index, then the column index.

print(animal_info_df[2, 1]) # Prints "fish"

######## Columns can be indexed by name:

print(animal_info_df[1, "noise"]) # Prints "woof"

######## Vectors can also be used for indexing:

print(animal_info_df[c(1, 4), "type"]) # Prints "dog" "cat"

######## So long as only one column is indexed, the returned data is a vector (possibly of length one, so a single data point):

print(class(animal_info_df[2, 1])) # Prints "character"
print(class(animal_info_df[2, 3])) # Prints "numeric"
print(class(animal_info_df[c(1, 4), "type"])) # Prints "character"

######## But when more than one column is indexed, the returned data is a dataframe

print(class(animal_info_df[2, c(1, 2)])) # Prints "data.frame"
print(class(animal_info_df[2, c("type", "legs")]))

######## Indexing can be skipped in one or both dimensions of the dataframe:

animal_info_df[, "type"] # Prints the vector c("dog", "fish", "chicken", "cat")
animal_info_df[c(1, 2),] # Prints a sub-dataframe with all info about dogs and fish

######## It's also possible to access a column of the dataframe by name using $:

animal_info_df$type # Prints the vector c("dog", "fish", "chicken", "cat")

######## Performing comparisons on dataframe columns can be very useful for indexing:

indicator_vector <- (animal_info_df$legs > 2) # Creates the vector c(TRUE, FALSE, FALSE, TRUE)
filtered_df <- animal_info_df[indicator_vector,] # Creates a df containing only info on animals with more than 2 legs.

######## Or in a single step:

filtered_df <- animal_info_df[animal_info_df$legs > 2,]

######## The %in% operator can be very useful here:

cats_and_fish <- animal_info_df$type %in% c("cat", "fish")
filtered_df <- animal_info_df[cats_and_fish,] #Dataframes are the main way of handling tabular information in base R.

###########################
# -------------------------
#         Session 2        Data Manipulation
# -------------------------
###########################

# ---------------------------------------------------------
# Section 1: Coding Best Practice
# ---------------------------------------------------------

######## When naming objects in R, there are different defined ways of labelling a newly created object
######## These include,
########    snake_case - where_underscores_replace_spaces
########    CamelCase  - whereCapitalsReplaceSpaces

######## Internal consistency is the most important thing - make sure to always
########    use the same convention for all variable names, and the same convention
########    for all function names.
######## As an example, the tidyverse uses snake_case for both variables 
########    and functions. See https://style.tidyverse.org/syntax.html#object-names

######## When code spans multiple lines, it is best practice to split at sensible points.
######## It is also best to leave whitespace to make code most readable.
######## Compare the following examples:

######## mutate(total=sum((data_1+data_2)/data_3,na.rm=TRUE))
######## mutate(total= = sum((data_1 + data_2) / data_4, na.rm = TRUE))

######## Evidently one is better!

# ---------------------------------------------------------
# Section 2: Packages
# ---------------------------------------------------------

######## When analysing data in R, you will almost always need to import a library.
######## The most common is tidyverse, which is a collection of packages that
########    provide various useful functionalities.

######## To install the tidyverse package, we run the following line

install.packages("tidyverse")

######## Note that this only needs to be done once - after the packages are
########    installed, they're saved to a local library.
######## There's no need to run this line every time you open R.

######## On some outdated versions of R, you may need to run the following line of code:

install.packages("tidyverse", type = "binary")

######## Once a package is installed, it's available for use.
######## You still need to tell RStudio that you intend to use it in the current session/script.
######## To load/import a library, we run

library(tidyverse)

# ---------------------------------------------------------
# Section 3: What is the Tidyverse?
# ---------------------------------------------------------

######## Before we start using data, we need to focus on the tidyverse
######## The tidyverse is the main package you will use in R
######## Let's load in the tidyverse, once we've made sure that it's installed

install.packages("tidyverse)")
library(tidyverse)

######## The tidyverse includes many packages:
######## Part of the tidyverse is the "core tidyverse"
######## This includes ggplot2, dplyr, readr and tidyr

######## You can find out more about the tidyverse at
######## https://tidyverse.org/

# ---------------------------------------------------------
# Section 4: Importing and Exporting Data
# ---------------------------------------------------------

######## Most R projects will usually consist of importing data, (sometimes combining data), 
########   analysing it and then exporting either a summary or a visualisation
######## Today we'll focus on the importing/exporting and combining

######## To import data, the most useful package is readr, part of the tidyverse
######## It includes options to import csv, xlsx, sav and other file types

######## Let's start with importing the easiest format, a csv.
######## To import a csv, we have a couple of options, a base R function, (in the utils package), 

read.csv()

######## Or a function from the readr package

readr::read_csv()

######## You can read about the advantages of each here:
########    https://r4ds.had.co.nz/data-import.html#compared-to-base-r
######## I prefer using the readr function since it is quicker, part of the 
########    tidyverse and therefore works better with other tidyverse functions

######## Let's import some data

######## We only need to specify where we are importing the data from and what we are calling it
######## Firstly, where; we need to ensure this file can be found, 
########   making sure the directory is specified, by either setwd or withr::with_dir

getwd()

setwd(paste0(getwd(), "/R files"))

######## Secondly, what; We also need to assign it to a variable, if we are going to use this data set

my_data <- readr::read_csv("filepath/filename.csv")

######## We can also use the with_dir function to temporarily change the directory

with_dir(filepath, my_function)

with_dir(filepath, read_csv("filename.csv"))

######## Note that the name of the file and the filepath need to be strings!

######## Once we have performed analysis on an imported dataframe, we may need to export this
######## To export a data frame, we use either of the following:

write.csv()

readr::write_csv()

######## In practice, we need to specfiy where and what:

readr::write_csv(dataframe, filepath)

######## Usually csvs are the easiest data sources to use,
########    usually, a csv is already processed in some sense
########    and so is approaching "oven-ready"

######## Often however, you will be using Excel files, most commonly
########    a .xls or .xlsx file

######## To handle Excel files, we use another tidyverse package, readxl

readxl::read_excel()

######## This detects the file extension, and calls one of the following:

readxl::read_xls()
readxl::read_xlsx()

######## For example, some preloaded data in the readxl package:

readxl::read_xlsx(readxl_example("datasets.xlsx"))

######## We will use the storms dataset, which is contained within the tidyverse
######## So we can play around with some functions, we'll define some other dataframes
######## Let's assign them separately

my_data <- storms

storms_old <- my_data[my_data["year"] < 2000, ]
storms_new <- my_data[my_data["year"] >= 2000, ]
storms_ll <- my_data[c("year", "month", "day", "lat", "long")]
storms_type <- my_data[c("year", "month", "day", "status", "category")]

######## I'll also create another table we'll need

lat_lookup <- mutate(tibble(lat = seq(-90,90,0.1)), lat_label = case_when(lat < 0 ~ "southern hemisphere", lat < 23.43 ~ "below tropic of cancer", lat < 66.57 ~ "between cancer and arctic", TRUE ~ "above arctic"))
lat_lookup_filtered <- filter(lat_lookup, (5 * lat) %% 1 == 0)
long_lookup <- mutate(tibble(lat = seq(-90,90,0.1)),lat_label =  case_when(lat < 0 ~ "western hemisphere", TRUE ~ "eastern hemisphere"))

# ---------------------------------------------------------
# Section 5: Manipulating Data (binding and merging)
# ---------------------------------------------------------

######## To combine data sets with the same column or row names, we can use

dplyr::bind_rows()
dplyr::bind_cols()

######## For example
######## Can you guess what these produce

storms_all_dates <- dplyr::bind_rows(storms_old, storms_new)
storms_all_info <- dplyr::bind_cols(storms_ll, storms_type)

######## Similarly, to join data sets together, we can use 

dplyr::left_join()
dplyr::right_join()
dplyr::inner_join()
dplyr::full_join()

storms_lat_labelled <- dplyr::left_join(storms_ll, lat_lookup)
storms_lat_labelled <- dplyr::full_join(storms_ll, lat_lookup)
storms_lat_labelled <- dplyr::right_join(storms_ll, lat_lookup)

storms_lat_labelled <- dplyr::inner_join(storms_ll, lat_lookup_filtered)
storms_lat_labelled <- dplyr::full_join(storms_ll, lat_lookup_filtered)

######## Note longer means more rows and wider means more columns

###########################
# -------------------------
#         Session 3        Data Analysis
# -------------------------
###########################

######## Let's play around with the Storms dataset some more.
######## The key to using dplyr functions is the *pipe* which looks like: %>%
######## The shortcut to add a pipe is <ctrl-shift-m>
######## We call our dataset, use the pipe %>%, and then call the different functions we want to apply.

######## First off, the Select Function allows us to select specific Columns that we are interested in. 
######## Here, we can create a new dataset that just looks at the date and time of each storm by listing the columns from our original dataset that we want to keep.

storms_dates <- storms %>%
  dplyr::select(name, year, month, day, hour)

storms_dates_reordered <- storms %>%
  dplyr::select(name, hour, day, month, year)

######## This can also be achieved by deselecting the columns we are not interested in. To deselect, add a minus sign infront of column name. 
######## This can be useful when you have a large dataset with lots of columns, and only want to remove a one or two. 

storms_dates2 <- storms %>%
  dplyr::select(-lat, -long, -status, -category, -wind, -pressure, -ts_diameter, -hu_diameter)

######## The rename function allows us to rename columns (duh!)
######## The first variable is the new name, the second variable is the old name (less obvious!)

storms_renamed <- storms %>% 
  dplyr::rename(storm_name = name)

######## The arrange function allows us to sort out dataset by a given column
######## For example, storms is already sorted chronologically, but lets sort it alphabetically by storm name

storms_alphabetical <- storms_renamed %>% 
  dplyr::arrange(storm_name)

######## We can arrange it the opposite way wrapping our column in the 'desc' function
storms_alphabetical <- storms_renamed %>% 
  dplyr::arrange(desc(storm_name))

######## We can use the Filter function to select Rows we are interested in based on a condition. 
######## For example, here we can narrow down the Storms Dataset to only include Storms which occured after 2010. 

storms_recent <- storms %>%
  dplyr::filter(year > 2010)

######## We can also choose what we DON'T want our rows to include, using the exclamation point!
######## Below, we are filtering our dataset to include all rows that DON'T have an NA value for ts_diameter.

storms_clean <- storms %>%
  dplyr::filter(!is.na(ts_diameter))

######## We can use multiple pipes to apply multiple commands in one step. This can be using the same function or different functions.
######## The pipe always comes at the end of your line, as it introduces the next line.
######## Let's filter our dataset to show us only hurricanes after 2010 in one step using our pipe.

hurricanes_recent <- storms %>%
  dplyr::filter(year > 2010) %>%
  dplyr::filter(status == "hurricane")

######## The mutate function allows us to add new columns to our dataset. It's most useful when applying a function to that column which we pre-populate the column for you.
######## For example, we're going to add a column to our storms_clean dataset to tell us whether the storm was in the Northern or Southern Hemisphere, using our latitude value.
######## The first element is the name of our new column. We then add our function to calculate whether the column contains 'Northern' or 'Southern' based on the value of the latitude.
######## Reminder that '&' is our symbol for AND, and "|" is our symbol for OR

storms_clean <- storms_clean %>%
  dplyr::mutate(time = case_when(
    hour > 6 & hour < 18 ~ "Daytime",
    hour <= 6 | hour >= 18 ~ "Nightime",
  ))

######## The summarise function creates a vector or dataset from a specific function applied to the dataset.
######## For example here, we can get the mean hour which storms occur by applying the mean function within the summarise function.
storms_clean_mean <- storms_clean %>%
  dplyr::summarise(mean(hour))

######## However, this hasn't been that useful as a standalone function, because we could have achieved the same output by simply doing:

storms_clean_mean <- mean(storms_clean$hour)

######## Summarise becomes most useful as a function when it is used in conjunction with the group_by and ungroup functions.
######## These functions allow us to separate data into groups depending on a specific variable, and then apply functions to these groups.
######## For example, we can get the mean hour which storms occur, grouped by the different type of storms.

storms_clean_mean2 <- storms_clean %>%
  dplyr::group_by(status)%>%
  dplyr::summarise(mean(hour))

######## This has given us a new dataframe that tells us the mean hour storms occur for each of the different storm types.

######## The ungroup function is the opposite of the group_by function, and simply disbands the groups you've created.
######## This is most useful when you want to group your dataset, mutate it in some way based on these groups, and then get back to the original ungrouped dataset.
######## For example, let's group our dataset by storm type, add a column that tells us the mean hour by storm type, ungroup our data, and add another column that tells us the mean hour overall.

storms_clean <- storms_clean %>%
  dplyr::group_by(status) %>%
  dplyr::mutate(mn_hour_type = mean(hour)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(mn_hour = mean(hour))

######## For best practice, always ungroup your data after grouping as this will reduce the likelihood of errors.

######## Let's put this all together to make one chunk of code that does all our cleaning
hurricane_clean <- storms %>% 
  dplyr::select(-c(category, wind, pressure)) %>%  #remove the unnecessary columns
  dplyr::filter(year > 2000) %>% # select storms after 2000 only
  dplyr::filter(status == "hurricane") %>% #select hurricanes only
  dplyr::arrange(name) %>% #sort them alphabetically
  dplyr::mutate(time = case_when(
    hour > 6 & hour < 18 ~ "Daytime",
    hour <= 6 | hour >= 18 ~ "Nightime",)) #add our extra time column

######## I want to know the mean latitude/longitude of each storm 
######## Normally, I would simply to the group_by and summarise process as so

hurricane_summary <- hurricane_clean %>% 
  dplyr::group_by(name) %>% 
  dplyr::summarise(mean(lat), mean(long)) %>% 
  dplyr::ungroup()


######## However, I've spotted that names are often used more than once e.g. Alex 2004 and Alex 2010
######## Our code above was taking the mean for all storms named Alex, even if they were different storms
######## Let's fix this by adding a column to our hurricane_clean dataset to differentiate between these storms

hurricane_clean_fix <- hurricane_clean %>% 
  dplyr::mutate(hurricane_id = paste(name, year))

######## Now let's go back and run our summary code again, this time using the hurricane_id column rather than the name column
hurricane_summary2 <- hurricane_clean_fix %>% 
  dplyr::group_by(hurricane_id) %>% 
  dplyr::summarise(mean(lat), mean(long)) %>% 
  dplyr::ungroup()

######## You can see the difference this has made because we now have 96 rows insetad of 75! Scroll through and spot which names have been used multiple times

###########################
# -------------------------
#         Session 4        Data Visualisation
# -------------------------
###########################

library(tidyverse)
library(ggplot2)
library(dplyr)

# ---------------------------------------------------------
# Section 1: Understanding Our Data & Review
# ---------------------------------------------------------

dt <- storms #Using the standard storms data 

str(dt) #Gives us data frame info

first_ever_storm_name <- dt[1,1] #How to extract a cell

storm_names <- dt['name'] #Next 4 lines all achieve the same thing
storm_names <- dt[1]
storm_names <- dt[,1]
storm_names <- dt$name

storm_names <- dt[[1]] #removes column name - v.useful!

storm_names <- dt[1] 

unique_list_of_storms <- unique(storm_names)
number_of_storm_names <- count(unique_list_of_storms) #NB not the same as number of storms! Over the years multiple storms have had the same name

number_of_storm_names <- count(unique(storm_names)) #In 1 line what we did in two lines above
number_of_storm_names <- storm_names %>% unique() %>% count() #Same as above using the 'pipe' operator

# ---------------------------------------------------------
# Section 2: Getting the Data Ready to Plot 
# Aim: stacked bar chart, storms since 2000 (inc.), categories shown by colours 
# ---------------------------------------------------------

ordered_dt <- arrange(dt, desc(category)) #So when I remove duplicates (e.g. all entries of 'Katrina 2005') then the entry I keep is the one with the highest category

names_and_years <- ordered_dt[c('name', 'year')] #Since a duplicate isn't just same name, but same name & year (BUT see storm Zeta in 2005/6)

filtered_dt <- ordered_dt[!duplicated(names_and_years),]
filtered_dt <- subset(filtered_dt, !(name == 'Zeta' & year =='2006')) #Removing the storm which spanned 2005 & 2006

plot_dt <- filter(filtered_dt, filtered_dt$year > 1999)

# ---------------------------------------------------------
# Section 3: A Simple Bar Chart & Corresponding Pie Chart
# ---------------------------------------------------------

bar_chart <- ggplot(plot_dt, aes(x = year, fill = category)) + geom_bar() 
bar_chart <- bar_chart + ggtitle('Atlantic Storms') + xlab("Year") + ylab("Number of Storms") + labs(caption='Data Source: NOAA', fill='Category')

pie_chart <- ggplot(plot_dt, aes(x = "", fill = category)) + geom_bar() + coord_polar(theta = "y")
pie_chart <- pie_chart + geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5)) + scale_fill_brewer()
pie_chart <- pie_chart + ggtitle('Atlantic Storm Categories (2000-2015)') + labs(caption='Data Source: NOAA', fill='Category') + theme_void()


# ---------------------------------------------------------
# Section 4: More Than 1 Plot/Geom On One Chart!
# Aim: Plotting the path of Hurricane Katrina (2005)
# Here, preparing the data is a lot easier and more work is done on the chart
# ---------------------------------------------------------

katrina_dt <- filter(dt, name == 'Katrina' & year == '2005')
plot2_dt <- katrina_dt[order(katrina_dt$month, katrina_dt$day, katrina_dt$hour),] #Not necessary, but helpful to see the order() function

world_dt <- map_data('state') #Built in data of the edges of all US states
new_orleans <- data.frame(
  long = c(-90.0),
  lat = c(29.6)
)

scatter_plot <- ggplot() + geom_polygon(world_dt, mapping=aes(long, lat, group=group), fill='white')
scatter_plot <- scatter_plot + geom_point(plot2_dt, mapping = aes(long,lat, size=category, color=as.factor(day)))
scatter_plot <- scatter_plot + geom_point(new_orleans, mapping = aes(long,lat), shape=3)
scatter_plot <- scatter_plot + ggtitle('Path of Hurricane Katrina (August 2005)') + xlab("Longitude") + ylab("Latitude") + labs(caption='Data Source: NOAA', size='Category', color='Date') + coord_cartesian(xlim=c(-74, -95), ylim=c(22,38))
scatter_plot <- scatter_plot + annotate('text', x=-92.5, y=30.2, label='New Orleans')

#The chart below is another example of more than one Plot/Geom on one chart
#We are plotting a histogram for the storm pressures from our original data overlaid with a 'density' plot

density_plot <- ggplot(dt, aes(x= pressure)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5)
density_plot <- density_plot + geom_density(alpha=.2, fill="#FF6666") #We see some more ways to format, here a new way to refer to colours and 'alpha' or opacity
density_plot <- density_plot + ggtitle('Distribution of Atlantic Storm Pressures (1975-2015)') + xlab("Pressure (mbar)") + ylab("Density") + labs(caption='Data Source: NOAA')

# ---------------------------------------------------------
# Section 5: How to save your plot
# Uncomment the line below and paste into console, it'll save in the current directory and you need format in file name
# If you don't specify the plot, it'll just save the last one you produced
# You can also save using RStudio buttons provided, but code may be more convenient
# ---------------------------------------------------------

#ggsave(plot = scatter_plot, filename = 'My_great_plot.jpeg', device = 'jpeg')