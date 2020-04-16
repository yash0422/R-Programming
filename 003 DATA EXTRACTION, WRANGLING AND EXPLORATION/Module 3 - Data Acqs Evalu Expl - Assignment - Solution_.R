# Module 3: Data Acquisition, Evaluation and Exploration

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Perform the following tasks on the dataset

# Task 1: Import the dataset into R
# ??? Ensure imported data is interpreted correctly by R.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

getwd                                           # Checking the present working directory

# Setting the Working Directory
setwd("C:\Users\Asus\Desktop\Data Science Master Program\Edureka_Studies\R Certification\003 DATA EXTRACTION, WRANGLING AND EXPLORATION")

m2_disease <- read.csv('M2_Disease_data_AssignDataset).csv')            # Reading the DATASET

m2_disease <- read.csv(file.choose())           # 4. Choosing the file if above command is not used

str(m2_disease)                                 # Gives the STRUCTURE OF DATA

names(m2_disease)                               # Gives the NAME OF COLUMNS

summary(m2_disease)                             # Gives the SUMMARY OF DATA (Columns)

class(m2_disease)                               # Gives the CLASS OF DATA

nrow(m2_disease)                                # Shows number of ROWS

ncol(m2_disease)                                # Shows number of COLUMNS

m2_disease[,1]                                  # Check the COLUMN CONTENT

sub("seven","7",m2_disease$Age)                 # Replace the STRING VALUE(seven) to NUMERIC VALUE(7)

m2_disease$Age <- sub("seven","7",m2_disease$Age) # Save the above made changes in our DATAFRAME,

m2_disease                                      # Check for change made

sapply(m2_disease, class)                       # Check CLASS TYPE of Dataframe

m2_disease_2 <- m2_disease                      # Make a copy of our dataframe


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

######     ??? Convert Age column to type Numeric

m2_disease_2$Age <- as.numeric(as.character(m2_disease_2$Age))   # Convert AGE COlumn to NUMERIC
sapply(m2_disease_2, class)                     # Check CLASS TYPE

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

######     ??? Omit NULL values from non-numerical data fields
######     ??? Replace NULL values with average value of that variable

mean(m2_disease_2$timesupper)                   # This will give us an NA Output as all values are not in
                                                # Numeric format, so we need to convert it to numeric

m2_disease_2$timesupper                         # Check for NA Values if present
m2_disease_2$timesupper <- m2_disease$timesupper
m2_disease_2
m2_disease_2$timesupper[is.na(m2_disease_2$timesupper)]    # Get only the NA Values

Mean <- mean(m2_disease_2[, 3], na.rm = TRUE)   # Calculate the MEAN in timesupper column excluding the NA Value
Mean

m2_disease_2[,3][is.na(m2_disease_2[,3])] <- Mean    # Replace NA with MEAN of that Column Variables Mean
m2_disease_2

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

######      ??? Give proper time format for timesupper column using strptime() and
######        get the date and time in the below specified format

# Sorry, Could not solve this as it was not even taught in my class

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

no_of_agg <- aggregate(x = m2_disease_2, by = list(unique.values = m2_disease_2$timesupper), FUN = length)
no_of_agg_2 <- aggregate(m2_disease_2, by = list(m2_disease_2$timesupper, m2_disease_2$sex), FUN = mean, na.rm = FALSE)
View(no_of_agg_2)

m2_disease_2$sex <- sub("-1","Male", m2_disease_2$sex)                 # Replace the STRING VALUE(seven) to NUMERIC VALUE(7)
m2_disease_2

group_by <- aggregate(m2_disease_2$ill, by = list(m2_disease_2$sex), FUN = length)
View(group_by)

# Find the average age of people who are ill using Boxplot.

attach(m2_disease_2)
library(ggplot2)
age_ill_plot = ggplot(m2_disease_2, aes(x = Age, y = ill)) + geom_boxplot() 
age_ill_plot


# Visualize gender ratio from the data.

group_by <- aggregate(m2_disease_2$ill, by = list(m2_disease_2$sex), FUN = length)
View(group_by)