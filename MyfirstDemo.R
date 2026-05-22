
# Testing the type of R  objects
age <- 30

str(age)

# Now AgeNew as an Integer
AgeNew <-  as.integer(age)

# Display text with age
paste(" The age is: ", age)

# Case sensitive example
Age <- 25
AgeNew == age 

# Work with data frames
Mydata <- data.frame(name = c("Chesca", "Sokol", "Sinovia"),
                     age = c(25, 30, 28),
                     student = c( FALSE,TRUE, FALSE))

# Accessing with indexes 
Mydata[1,2]

# Second line
SecondLine <- Mydata[2,]

# Third column with indexes (Matrix style)
ThirdCol <- Mydata[,3]
SecondCol <- Mydata[,2]

# Columns as an object
Agecolumn <- Mydata$age
Agecolumn











