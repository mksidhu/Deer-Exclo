
####This file contains all the functions for Deer Data analysis. 

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
library(tidyr)

##1: ocular cover. optional parameter to fix cover estimates to sum = 100%
convert_ocular <- function(x) {
  x[x == 1] <- ((0 + 5)/2)/100
  x[x == 2] <- ((5 + 25)/2)/100
  x[x == 3] <- ((25 + 50)/2)/100
  x[x == 4] <- ((50 + 75)/2)/100
  x[x == 5] <- ((75 + 95)/2)/100
  x[x == 6] <- ((95 + 100)/2)/100
  return(x)
}

adjusted_ocular <- function(df_side) {
  ocular_cols = 5:9 
  df_side_sums <- cbind(convert_ocular(df_side[ocular_cols]), Sums =
                          rowSums(convert_ocular(df_side[, ocular_cols])))
  r_adj_t = list()
  for (r in 1:nrow(df_side_sums)){
    s <- df_side_sums$Sums[r]
    c_adj <- df_side_sums[r, 1:5]
    if (is.na(s) == TRUE) {
      r_adj <- c_adj
    }
    else{
      r_adj <- c_adj %>% map(\(x) round((x/s), 3))
    }
    r_adj_t <- append(r_adj_t, list(r_adj))
  }
  adj_df <- bind_rows(r_adj_t)
  return(adj_df)
}


cover_avgs <- function(df_east, df_west, adjust = FALSE) {
  ocular_cols = 5:9
  if (adjust == TRUE){
    east_converted_cover <- adjusted_ocular(df_east)
    west_converted_cover <- adjusted_ocular(df_west)
  }
  else {
    east_converted_cover <- lmap(df_east[,ocular_cols], convert_ocular)
    west_converted_cover <- lmap(df_west[,ocular_cols], convert_ocular)
  }
  
  inside_rows = 11:20 
  outside_rows = 1:10
  
  e_inside_avg <- east_converted_cover[inside_rows,] %>% 
    map_df(\(x) round(mean(x, na.rm = TRUE), 3))
  e_outside_avg <- east_converted_cover[outside_rows,] %>% 
    map_df(\(x) round(mean(x, na.rm = TRUE), 3))
  w_inside_avg <- west_converted_cover[inside_rows,] %>% 
    map_df(\(x) round(mean(x, na.rm = TRUE), 3))
  w_outside_avg <- west_converted_cover[outside_rows,] %>% 
    map_df(\(x) round(mean(x, na.rm = TRUE), 3))
  tot_inside_avg <- map2_df(east_converted_cover[inside_rows,], 
                            west_converted_cover[inside_rows,], 
                            \(x, y) round(mean(c(x, y), na.rm = TRUE), 3))
  tot_outside_avg <- map2_df(east_converted_cover[outside_rows,], 
                             west_converted_cover[outside_rows,], 
                             \(x, y) round(mean(c(x, y), na.rm = TRUE), 3))
  
  four_sided_avgs <- bind_rows(list("East Inside" = e_inside_avg, "West Inside" 
                                    = w_inside_avg, "East Outside" = 
                                      e_outside_avg, "West Outside" = 
                                      w_outside_avg, "Inside Total" =
                                      tot_inside_avg, "Outside Total" = 
                                      tot_outside_avg),
                               .id = "Location")
  
  return(four_sided_avgs)
}



##2: keying function: takes species name, returns associated veg letter 
keying_code <- function(spp) {
  key_row <- str_which(spp, plant_key$Code)
  pk <- plant_key[key_row, 1]
  return(pk)
} 


##3: creates dataframe of all species+veg code in range (in vs out) of E or W 
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


##4: returns dataframes of the same treatment (east and west combined but
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


##5: given df of all species+veg code, gives species richness of specified category 
##or all categories as a print statement, accounting for duplicates. 
##creates df of all spp (including dupes and unks) and counts
species_richness <- function(spp_df, code_list = list("F", "G", "W", "O"), 
                             quiet = TRUE){ 
  richness_df <- list()
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
    #print(glue("{l} = {l_count} species"))
    df_row <- data.frame(l, l_count)
    colnames(df_row) <- c("Key", "Count")
    richness_df <- append(richness_df, list(df_row))
  }
  richness_df <- bind_rows(richness_df)
  if (!quiet) {
    print(richness_df)
  }
  return(spp_count)
}


##6: % invasive
keying_invasive <- function(df_richness) {
  invasives_df <- filter(plant_key, Invasive == 1)
  key_col = 1
  count_col = 3
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



##7: evaluating browse (OUTSIDE ONLY, assuming no browsed inside). only evaluates browse
##if 5 or more individuals in the species. if heights = TRUE, returns df with avg heights. else
##returns df with % browse by species 
#####could combine this with inside and outside


##for some reason two sided BlkCherry % browse shows up twice??
percent_browse_outside <- function(treatment_listing_df, 
                                   treatment_richness_counts, heights = TRUE,
                                   quiet = FALSE) {
  treatment_counts_wf <- treatment_richness_counts %>% 
    filter((Key == "W" | Key == "F") & (n >= 5))
  df_to_eval <- treatment_listing_df %>% filter(Species %in% treatment_counts_wf$Species)
  
  summarise_avg_heights <- df_to_eval %>% group_by(Browse, Species) %>% 
    summarise(AvgHeight = mean(Height, na.rm = TRUE), Count = n(), 
              .groups = "keep")
  df <- summarise_avg_heights %>% filter(Browse == 1)
  
  summarise_browse <- list()
  for (sp in treatment_counts_wf$Species) {
    col_n = 4 #column for species count
    cc <- df[str_which(df$Species, regex(paste0("^", sp, "$"))), col_n]
    c <- treatment_counts_wf[str_which(treatment_counts_wf$Species, 
                                       regex(paste0("^", sp, "$"))), (col_n - 1)]
    if (nrow(cc) > 0) {
      percent <- round(((cc/c)*100), 1)
      if ((heights == TRUE) && (quiet == FALSE)) {
        print(glue("{sp}: {percent}% browsed"))
      }
    }
    else {
      percent <- 0
      if ((heights == TRUE) && (quiet == FALSE)) {
        print(glue("{sp} has no browsed individuals"))
      }
    }
    key <- keying_code(sp)
    sum_brow_row <- data.frame(key, sp, percent)
    colnames(sum_brow_row) <- c("Key", "Species", "Percent_Browse")
    summarise_browse <- append(summarise_browse, list(sum_brow_row))
  }
  
  if (!heights) {
    return(bind_rows(summarise_browse))
  }
  else {
    return(summarise_avg_heights)
  }
}
#could pivot_wider and turn create browse/unbrowse as cols but necessary? 


##8: find average height of every woody species over 5 individuals. removes heights of na,
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
    spp_avg_height <- round(mean(spp_heights, na.rm = TRUE), 1)
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



##9: main functions all combined so it doesn't return a million different dfs
total_analysis <- function(df_east, df_west){
  #print("Occular Cover Averages")
  #print(cover_avgs(df_east, df_west))
  
  inside <- unite_treatment(df_east, df_west, TRUE)
  outside <- unite_treatment(df_east, df_west, FALSE)
  
  print("Inside: Species Richness")
  inside_richness <- species_richness(inside, quiet = FALSE)
  print("Outside: Species Richness")
  outside_richness <- species_richness(outside, quiet = FALSE)
  
  #print("Inside: Count and Proportion of Invasive Species")
  #print(invasive_percents(inside_richness))
  #print("Outside: Count and Proportion of Invasive Species")
  #print(invasive_percents(outside_richness))
  
  print("Inside: Woody Average Heights")
  print(avg_inside_heights(inside_richness, inside), na.print = "NA", n = Inf)
  print("Outside: Counts of Woody and Forb Browse and Average Heights")
  print(percent_browse_outside(outside, outside_richness), 
        na.print = "NA", n = Inf)
}


##10: returns name of dataframe treatment
which_treatment <- function(df_side) {
  full_nm <- df_side[1,1]
  df_nm <- word(full_nm, 1, sep = ("_"))
  if (str_detect(df_nm, "2")) {
    df_nm <- str_replace(df_nm, "2", "Two")
    df_nm <- glue("{df_nm} Release")
  }
  if (str_detect(df_nm, "4")) {
    df_nm <- str_replace(df_nm, "4", "Four")
    df_nm <- glue("{df_nm} Release")
  }
  return(df_nm)
}


#11: returns list of matching species (name only) btwn 3 dfs
####yikes, order of dfs is not interchangeable ????
spp_in_common <- function(df1, df2, df3) {
  common_spp <- list()
  for (sp in df1$Species){
    if (has_element(df2$Species, sp) && has_element(df3$Species, sp)){
      common_spp <- append(common_spp, sp)
    }
  }
  return(common_spp)
}

