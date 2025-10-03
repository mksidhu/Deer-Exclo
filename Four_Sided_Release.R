setwd("C:/Users/DReaM/Documents/deer exclosure data")

library(readr)

plant_key <- read_csv("Botanical Key.csv")

four_west <- read_csv("4-Sided_West.csv")
four_east <- read_csv("4-Sided_East.csv")

control_west <- read_csv("Control_West.csv")
control_east <- read_csv("Control_East.csv")

two_west <- read_csv("2-Sided_West.csv")
two_east <- read_csv("2-Sided_East.csv")


library(purrr)
library(glue)
library(stringr)
library(dplyr)
library(tidyselect)

library(ggplot2)
library(RColorBrewer)


####drafting
##need a function that would modify occular cover and get it to 100% each category


########### named functions
##1: ocular cover
convert_ocular <- function(x) {
  x[x == 1] <- ((0 + 5)/2)/100
  x[x == 2] <- ((5 + 25)/2)/100
  x[x == 3] <- ((25 + 50)/2)/100
  x[x == 4] <- ((50 + 75)/2)/100
  x[x == 5] <- ((75 + 95)/2)/100
  x[x == 6] <- ((95 + 100)/2)/100
  return(x)
}

cover_avgs <- function(df_east, df_west) {
  ocular_cols = 5:9
  east_converted_cover <- lmap(df_east[,ocular_cols], convert_ocular)
  west_converted_cover <- lmap(df_west[,ocular_cols], convert_ocular)
  
  inside_rows = 11:20 
  outside_rows = 1:10
  
  e_inside_avg <- map(east_converted_cover[inside_rows,], \(x) round(mean(x), 3))
  e_outside_avg <- map(east_converted_cover[outside_rows,], \(x) round(mean(x), 3))
  w_inside_avg <- map(west_converted_cover[inside_rows,], \(x) round(mean(x), 3))
  w_outside_avg <- map(west_converted_cover[outside_rows,], \(x) round(mean(x), 3))
  tot_inside_avg <- apply(rbind(east_converted_cover[inside_rows,], 
                                west_converted_cover[inside_rows,]), 2, mean) %>% round(3)
  tot_outside_avg <- apply(rbind(east_converted_cover[outside_rows,], 
                                 west_converted_cover[outside_rows,]), 2, mean) %>% round(3)
  
  four_sided_avgs <- rbind(e_inside_avg, w_inside_avg, e_outside_avg, 
                           w_outside_avg, tot_inside_avg, tot_outside_avg)
  return(four_sided_avgs)
}
#new_df <- this function


##2: ggplots
plot1 <- ggplot(df, aes(x = deer_zone, y = four_sided_avgs, fill = categories)) 
+ geom_col(position="dodge") + labs(title = "Four-Sided Release Data", 
                                    x = "Transects", y = "Averages") + 
  scale_fill_brewer(palette = "Paired")

plot2 <- ggplot(df, aes(x = deer_zone, y = four_sided_avgs, fill = categories)) 
+ geom_col(position="stack") + labs(title = "Four-Sided Release Data",
                                    x = "Transects", y = "Averages") +
  scale_fill_brewer(palette = "Paired")
###LABEL FOR THE side fill SHOULD BE "COVER"



##3: keying function: takes species name, returns associated veg letter 
keying_code <- function(df_spp) {
  key_row <- str_which(df_spp, plant_key$Code)
  pk <- plant_key[key_row, 1]
  return(pk)
} 


##4: creates dataframe of all species+veg code in range (in vs out) of E or W 
listing_spp <- function(df_side, row_range) {
  input_list <- list()
  for (r in row_range){
  df_val_list <- list()
  for (c in 10:(ncol(df_side))) {
    if (typeof(df_side[[r,c]]) == "character" && !is.na(df_side[r,c])) {
      if (nchar(df_side[r,c]) > 1) {
        spp <- df_side[r,c]
        pl_key <- keying_code(spp)
        #print(glue("{spp} is {pl_key}"))
        brow <- df_side[r, (c+1)]
        height <- df_side[r, (c+2)]
        df_val <- data.frame(pl_key, spp, brow, height) 
        colnames(df_val) <- c("Key", "Species", "Browse", "Height")
        #print(df_val)
        df_val_list <- append(df_val_list, list(df_val))
      }
    }
  }
  input_list <- append(input_list, df_val_list)
  }
  return(bind_rows(input_list))
}


##5: returns dataframes of the same treatment (east and west combined but
##separates inside vs out)
unite_treatment <- function(df_east, df_west, inside = TRUE) {
  if (inside == TRUE) {
    row_range = 11:20
  }
  else {
    row_range = 1:10
  }
  listing_east <- listing_spp(df_east, row_range)
  listing_west <- listing_spp(df_west, row_range)
  sides_combined <- bind_rows(listing_east, listing_west)
  return(sides_combined)
}
#new_combined_df <- this function


##6: given df of all species+veg code, gives species richness of specified category 
##or all categories as a print statement, accounting for duplicates. 
##creates df of all spp (including dupes and unks) and counts
species_richness <- function(spp_df, code_list = list("F", "G", "W", "O")){ 
  spp_count <- spp_df %>% count(Key, Species)
  for (l in code_list) {
    category_count <- spp_count %>% filter(Key == l, str_detect(spp_count$Species, "Unk", negate = TRUE))
    l_count <- nrow(category_count)
    if (l == "W") {
      if (sum(str_like(category_count$Species, "%cherry")) == 2) {
        l_count <- l_count - 1
        #print(glue("cherry minus, count = {l_count}"))
      }
      if (any(str_like(category_count$Species,"rub%")) && any(str_like(category_count$Species, "%rasp"))) {
        l_count <- l_count - 1
        #print(glue("rubus minus, count = {l_count}"))
      }
      if (any(str_like(category_count$Species, "%dog")) && any(str_like(category_count$Species, "dog%"))) {
        l_count <- l_count - 1
        #print(glue("dog minus, count = {l_count}"))
      }
      if (any(str_like(category_count$Species, "%vib")) && any(str_like(category_count$Species, "vib%"))) {
        l_count <- l_count - 1
        #print(glue("vib minus, count = {l_count}"))
      }
    }
    print(glue("{l} = {l_count} species"))
  }
  return(spp_count)
}


##7: % invasive
keying_invasive <- function(df_richness) {
  invasives_df <- filter(plant_key, Invasive == 1)
  key_col <- 1
  count_col <- 3
  df_keys <- list()
  for (s in invasives_df$Code) {
    key_row <- str_which(df_richness$Species, s)
    pk <- df_richness[key_row, key_col]
    pcount <- df_richness[key_row, count_col]
    df_val <- data.frame(pk, pcount)
    colnames(df_val) <- c("Key", "Count")
    df_keys <- append(df_keys, list(df_val))
  }
  keys_bound <- bind_rows(df_keys)
  bound_sums <- keys_bound %>% group_by(Key) %>% 
    summarise(Invasive_Count = sum(Count))
  return(bound_sums)
}

invasive_percents <- function(df_richness) {
  df_invasives <- keying_invasive(df_richness)
  df_plant_sums <- df_richness %>% group_by(Key) %>% summarise(Tot_Count = sum(n))
  df_counts <- inner_join(df_invasives, df_plant_sums, by = "Key") %>% 
    mutate(Prop_Invasive = round((Invasive_Count / Tot_Count), 3))
  return(df_counts)
} 
#invasive_df <- this function



##8: evaluating browse (OUTSIDE ONLY, assuming no browsed inside). only evaluates browse
##if 5 or more individuals in the species 
#####could combine this with inside and outside
percent_browse_outside <- function(treatment_listing_df, treatment_richness_counts) {
  treatment_counts_wf <- treatment_richness_counts %>% 
    filter((Key == "W" | Key == "F") & (n >= 5))
  min_spp <- treatment_counts_wf$Species
  df_to_eval <- list() #df with sp, H, brow, individuals with n >= 5
  for (s in min_spp) {
    min_spp_rows <- treatment_listing_df %>% filter(Species == s)
    df_min_spp <- data.frame(min_spp_rows)
    df_to_eval <- rbind(df_to_eval, df_min_spp)
  } ##i feel like this could be more efficient
  
  summarise_browse <- df_to_eval %>% group_by(Browse, Species) %>% 
    summarise(AvgHeight = mean(Height, na.rm = TRUE), Count = n(), 
              .groups = "keep")
  
  for (sp in min_spp) {
    df <- summarise_browse %>% filter(Browse == 1)
    n = 4
    cc <- df[str_which(df$Species, sp), n]
    c <- treatment_counts_wf[str_which(treatment_counts_wf$Species, sp), (n - 1)]
    if (nrow(cc) > 0) {
      percent <- round(((cc/c)*100), 1)
      print(glue("{sp}: {percent}% browsed"))
    }
    else {
      print(glue("{sp} has no browsed individuals"))
    }
  }
  return(summarise_browse)
}



##9: find average height of every woody species over 5 individuals. removes heights of na,
##so if a species has fewer than 5 individuals after na's removed, avg height will not
##be calculated. this function does not evaluate for browse or unbrowsed, but has 
##optional parameter to ignore 5 minimum if used in evaluate browse function. 
###########i think this would still find the avg height for unkwood
avg_spp_heights <- function(treatment_count, treatment_df, evaluate_browse = FALSE) {
  df_species <- list()
  for (s in 1:nrow(treatment_count)) {
    #print(s)
    Count = 3
    spp_count <- treatment_count[s, Count]
    #print(spp_count)
    spp_df <- treatment_df %>% filter(Species == treatment_count$Species[s])
    #print(spp_df)
    if (evaluate_browse == FALSE) {
      na_height_count <- sum(is.na(spp_df$Height))
      if ((spp_count - na_height_count) < 5) {
        #print(glue("ran if, count is {spp_count - na_height_count}"))
        next
        }
    }
    spp_heights <- spp_df$Height
    spp_avg_height <- mean(spp_heights, na.rm = TRUE)
    df_intm <- data.frame(Species = treatment_count$Species[s], Average_Height = spp_avg_height)
    #print(df_intm)
    df_species <- append(df_species, list(df_intm))
  } 
  return(bind_rows(df_species))
}

avg_inside_heights <- function(treatment_richness_count, treatment_listing_df){
  treatment_woodies <- filter(treatment_richness_count, Key == "W" & (n >= 5))
  #print(treatment_woodies).... but should i combine n with output tibble here?
  df_over_5_woodies <- avg_spp_heights(treatment_woodies, treatment_listing_df)
  return(df_over_5_woodies)
}
#df_avg_spp_heights <- this function



##10: main functions all combined so it doesn't return a million different dfs
total_analysis <- function(df_east, df_west){
  inside <- unite_treatment(df_east, df_west, TRUE)
  outside <- unite_treatment(df_east, df_west, FALSE)
  
  print("Inside: Species Richness")
  inside_richness <- species_richness(inside)
  print("Outside: Species Richness")
  outside_richness <- species_richness(outside)
  
  inside_invasives <- invasive_percents(inside_richness)
  print("Inside: Count and Proportion of Invasive Species")
  print(inside_invasives)
  outside_invasives <- invasive_percents(outside_richness)
  print("Outside: Count and Proportion of Invasive Species")
  print(outside_invasives)
  
  heights_inside <- avg_inside_heights(inside_richness, inside)
  print("Inside: Woody Average Heights")
  print(heights_inside, na.print = "NA", n = Inf)
  print("Outside: Counts of Woody and Forb Browse and Average Heights")
  browse_outside <- percent_browse_outside(outside, outside_richness)
  print(browse_outside, na.print = "NA", n = Inf)
}
