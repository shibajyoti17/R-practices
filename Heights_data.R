library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

#First, determine the average height in this dataset.
#Then create a logical vector ind with the indices for those individuals who are above average height.

avg <- mean(heights$height)
ind <- which(heights$height > avg)
length(ind)

#How many individuals in the dataset are above average height and are female?

idx <- which(heights$height > avg & heights$sex == "Female")
length(idx)

#What proportion of individuals in the dataset are female?

is_fem <- (heights$sex == "Female")
avg_fem <- mean(is_fem)
print(avg_fem)

#Determine the minimum height in the heights dataset.

min_heights <- min(heights$height)
print(min_heights)

#Use the match() function to determine the index of the individual with the minimum height.

min_idx <- match(min_heights,heights$height)
print(min_idx)

#Subset the sex column of the dataset by the index to determine the individual’s sex.

print(heights$sex[min_idx])

#Determine the maximum height.

max_heights <- max(heights$height)
print(max_heights)

#Write code to create a vector x that includes the integers between the minimum and maximum heights.

x <- as.integer(min(heights$height)):as.integer(max(heights$height))
print(x)

#How many of the integers in x are NOT heights in the dataset?

sum(!x %in% heights$height)

#Using the heights dataset, create a new column of heights in centimeters named ht_cm. Recall that 1 inch =
#2.54 centimeters. Save the resulting dataset as heights2.

library(dplyr)
ht_cm <- heights$height*2.54
heights2 <- mutate(heights,height_cm = ht_cm)

#What is the height in centimeters of the 18th individual (index 18)?

print(ht_cm[18])

#What is the mean height in centimeters?

print(mean(ht_cm))

#Create a data frame females by filtering the heights2 data to contain only female individuals

female <- filter(heights2,sex == "Female")

#How many females are in the heights2 dataset?

nrow(female)

#What is the mean height of the females in centimeters?
 
mean(female$height_cm) 

#The olive dataset in dslabs contains composition in percentage of eight fatty acids found in the lipid fraction of 572 Italian olive oils:

data("olive")
head(olive)

#Plot the percent palmitic acid versus palmitoleic acid in a scatterplot. What relationship do you see?

plot(olive$palmitic,olive$palmitoleic)

#Create a histogram of the percentage of eicosenoic acid in olive.

hist(olive$eicosenoic)

#Make a boxplot of palmitic acid percentage in olive with separate distributions for each region.

boxplot(olive$palmitic~olive$region)