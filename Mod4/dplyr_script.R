## scripts for working with dplyr package

# define path to data location
directory = "/home/torsh/data_science_ml/edx/Mod4/"
fileName = "test_data.csv"
path = file.path(directory, fileName)

# make data frame
frame1 = read.csv(path, header = TRUE, stringsAsFactors = FALSE)
# get the header of data frame
head(frame1)

# select statements
frame2 = select(frame1, engine.size, horsepower)
head(frame2)

# horsepower needs to be cleaned before further processing!
# remove ? in horse pwer and its corresponding engin.size!
max(horsepower)

# select all except some ...
head(select(frame1, -fuel.system, -bore, -stroke, -compression.ratio, 
                     -horsepower, -peak.rpm, -city.mpg, -highway.mpg, -price
                     ))

# select from col_a to col_b
head(select(frame1, make:engine.size))

# select all cols starting with 'engine'
head(select(frame1, starts_with("engine")))

# point diagram plotting 2 cols by ggplot2 library
# todo: before plot clean horsepwer...
library(ggplot2)
ggplot(frame2, aes(engine.size, order(horsepower))) + geom_point(color="black") + ggtitle("size and power dependency")

