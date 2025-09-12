setwd("C:/Users/DReaM/Documents/deer exclosure data")

library(readr)


four_west <- read_csv("4-Sided_West.csv")

four_east <- read_csv("4-Sided_East.csv")

two_west <- read_csv("2-Sided_West.csv")

two_east <- read_csv("2-Sided_East.csv")

control_west <- read_csv("Control_West.csv")

control_east <- read_csv("Control_East.csv")

plant_key <- read_csv("Botanical Key.csv")


#log and nums misidentified, gotta change "browse" and "height""


### START by analyzing control and 4-sided plots (need by OCT)


##plots (ie east vs west) 
#averages of inside vs outside 

#average inside vs outside of combined zone (both east and west)



##convert ocular cover from integer categories to % midpoints. 
# 0 = 0%; 1 = 2.5%; 2 = 15%; 3 = 37.5%; 4 = 62.5%; 5 = 85%; 6 = 97.5%


##GRID estimate (inside vs out, not east vs west) to estimate % of veg type 
#50 cells and total %s will add up to 100%
# so want to count cells based on chr of "G", "NV", "W", "F", "O", "X"
# then divide by 50 and get percent 


##species RICHNESS (focus on Woody and Forbs FIRST)
# list species and count, divide into invasives and native/not invasive 

##for species with at least 5 individuals, split by zone (inside vs out, not east vs west)
#woodies: split individuals of a species by browsed or not (same species: % 
#browsed, % unbrowsed, avg height of browsed, avg height of unbrowsed)

#forbs: same species: % browsed, % unbrowsed 