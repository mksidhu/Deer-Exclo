library(readr)

library(purrr)
library(glue)
library(stringr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(RColorBrewer)
library(patchwork)

source("Named_Functions.R")


##when doing all three, go in order of control, two sided, four sided


#avg heights in-out browsed-out unbrowse of only woody spp in common
common_avg_heights_fig <- function(df_east, df_west){
  title <- which_treatment(df_east)
  
  inside <- unite_treatment(df_east, df_west, TRUE)
  outside <- unite_treatment(df_east, df_west, FALSE)
  inside_richness <- species_richness(inside, quiet = TRUE)
  outside_richness <- species_richness(outside, quiet = TRUE)
  inside_heights <- avg_inside_heights(inside_richness, inside)
  outside_heights <- percent_browse_outside(outside, outside_richness, quiet = TRUE)

  common_spp <- spp_in_common(inside_heights, outside_heights, outside_heights)
  
  inside_common <- inside_heights %>% filter(Species %in% common_spp)
  outside_common <- outside_heights %>% filter(Species %in% common_spp)
  
  plot1 <- ggplot(inside_common, aes(x = Species, y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + labs(title = 
                                        glue("Average Common Woody Heights 
                                             by Transect and Browse - {title}"), 
                                      x = "Inside", y = "Average Height (cm)") + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none", 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank()) + 
    coord_cartesian(ylim = c(0, 100))
  plot2 <- ggplot((outside_common %>% filter(Browse == 1)), aes(x = Species,
                                                                y = AvgHeight, 
                                                            fill = Species)) + 
    geom_col(position="dodge") + labs(x = "Outside - Browsed") + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none",
                                                  axis.title.y = element_blank(), 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank()) +
    coord_cartesian(ylim = c(0, 100))
  plot3 <- ggplot((outside_common %>% filter(Browse == 0)), aes(x = Species, 
                                                                y = AvgHeight, 
                                                            fill = Species)) + 
    geom_col(position="dodge") + labs(x = "Outside - Unbrowsed", 
                                      fill = "Species") + 
    scale_fill_brewer(palette = "Paired") + theme(axis.title.y = element_blank(), 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank()) + 
    coord_cartesian(ylim = c(0, 100))
  
  plot4 <- plot1 + plot2 + plot3
  return(plot4)
}


##ocular inside and outside tot avgs only control and four-sided

tot_avg_oc_fig <- function(df1_east, df1_west, df2_east, df2_west) {
  
  tidy_table1 <- pivot_longer(cover_avgs(df1_east, df1_west), 
                              cols = str_which(colnames(cover_avgs(df1_east, 
                                                                   df1_west)),
                                               "%"), names_to = "Category", 
                              values_to = "Averages") %>% 
    filter(str_detect(Location, "Total"))
  
  tidy_table2 <- pivot_longer(cover_avgs(df2_east, df2_west), cols = 
                                str_which(colnames(cover_avgs(df2_east, 
                                                              df2_west)),"%"), 
                              names_to = "Category", values_to = "Averages") %>%
    filter(str_detect(Location, "Total"))
 
  
  table_inside_tot <-  bind_rows(Control = filter(tidy_table1, str_detect(Location, "Inside")),
                                 "Four-Sided" = filter(tidy_table2, str_detect(Location, "Inside")),
                                 .id = "id")

  
  table_outside_tot <-  bind_rows(Control = filter(tidy_table1, str_detect(Location, "Outside")), 
                                  "Four-Sided" = filter(tidy_table2, str_detect(Location, "Outside")),
                                  .id = "id") 
 
  legend_colors <- c("Forbs_%" ="#B2DF8A", "Grasses_%" =  "#1F78B4", "NVGround_%" 
                     = "#FB9A99", "Other_%" = "#A6CEE3", "Woody_%" = "#33A02C")
  legend_labels <- c("Forbs_%" = "Forb", "Grasses_%" = "Grass", "NVGround_%" = 
                       "Bare Ground", "Other_%" = "Other (Sedges/Ferns)", "Woody_%"
                     = "Woody")
  
  plot1 <-  table_inside_tot %>% ggplot(aes(x = id, y = Averages, fill = Category)) + 
    geom_col(position="dodge") + labs(title = "Control and Four-Sided Release Treatment Plots
Plant Cover Averages", x = "Inside Transects", y = "Percent Averages") + 
    theme(legend.position = "none") + scale_fill_manual(values = legend_colors) +
    coord_cartesian(ylim = c(0, 1))
  plot2 <-  table_outside_tot %>% ggplot(aes(x = id, y = Averages, 
                                             fill = Category)) + 
    geom_col(position="dodge") + scale_fill_manual(values = legend_colors,
                                                   labels = legend_labels, 
                                                   name = "Plant Category") +
    labs(x = "Outside Transects") + theme(axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 1))
  
  plot3 <- plot1 + plot2 
  return(plot3)
}





##creates figures of average ocular covers - maybe could make the combined use
#optional parameters for df2 and df3 so dont need separate function for within figs
within_oc_fig <- function(df_east, df_west){
  oc_df <- cover_avgs(df_east, df_west)
  tidy_ocular <- pivot_longer(oc_df, cols = str_which(colnames(oc_df), "%"), 
                              names_to = "Category", values_to = "Averages")
  
  title <- which_treatment(df_east)
  legend_colors <- c("Forbs_%" = "#B2DF8A", "Grasses_%" = "#1F78B4", "NVGround_%" 
                     = "#FB9A99", "Other_%" = "#A6CEE3", "Woody_%" = "#33A02C")
  legend_labels <- c("Forbs_%" = "Forb", "Grasses_%" = "Grass", "NVGround_%" = 
                       "Bare Ground", "Other_%" = "Other (Sedges/Ferns)", "Woody_%"
                     = "Woody")
  
  plot1 <- tidy_ocular %>% filter(str_detect(Location, "Inside")) %>% 
    ggplot(aes(x = Location, y = Averages, fill = reorder(Category, Averages))) + 
    geom_col(position="dodge") + 
    labs(title = glue("{title} Treatment Plant Cover Averages"), x = 
           "Inside Transects", y = "Percent Averages") + theme(legend.position = 
                                                                 "none") + 
    scale_fill_manual(values = legend_colors) + coord_cartesian(ylim = c(0, 1))
    
  plot2 <- tidy_ocular %>% filter(str_detect(Location, "Outside")) %>% 
    ggplot(aes(x = Location, y = Averages, fill = reorder(Category, Averages))) + 
    geom_col(position="dodge") + scale_fill_manual(values = legend_colors,
                                  labels = legend_labels, name = "Cover") + 
    labs(x = "Outside Transects") + theme(axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 1))

  
  plot3 <- plot1 + plot2
  
  return(plot3)
}


btwn_oc_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                        df3_west, inside = TRUE) {
  
  tidy_table1 <- pivot_longer(cover_avgs(df1_east, df1_west), 
                              cols = str_which(colnames(cover_avgs(df1_east, 
                                                                   df1_west)),
                                               "%"), names_to = "Category", 
                              values_to = "Averages") %>% 
    filter(str_detect(Location, "Total"))
  title1 <- which_treatment(df1_east)
  
  tidy_table2 <- pivot_longer(cover_avgs(df2_east, df2_west), cols = 
                                str_which(colnames(cover_avgs(df2_east, 
                                                              df2_west)),"%"), 
                              names_to = "Category", values_to = "Averages") %>% 
    filter(str_detect(Location, "Total"))
  title2 <- which_treatment(df2_east)
  
  tidy_table3 <- pivot_longer(cover_avgs(df3_east, df3_west), cols = 
                                str_which(colnames(cover_avgs(df3_east, 
                                                              df3_west)),"%"), 
                              names_to = "Category", values_to = "Averages") %>% 
    filter(str_detect(Location, "Total"))
  title3 <- which_treatment(df3_east)
  
  if (inside == TRUE){
    tr <- "Inside"
  }
  else {
    tr <- "Outside"
  }
  
  legend_colors <- c("Forbs_%" = "#B2DF8A", "Grasses_%" = "#1F78B4", "NVGround_%" 
                     = "#FB9A99", "Other_%" = "#A6CEE3", "Woody_%" = "#33A02C")
  legend_labels <- c("Forbs_%" = "Forb", "Grasses_%" = "Grass", "NVGround_%" = 
                       "Bare Ground", "Other_%" = "Other (Sedges/Ferns)", "Woody_%"
                     = "Woody")
  
  plot1 <- tidy_table1 %>% filter(str_detect(Location, tr)) %>% 
    ggplot(aes(x = Location, y = Averages, fill = Category)) + 
    geom_col(position="dodge") + labs(title = 
                                        glue("{tr} Plots Plant Cover Averages"), 
                                      x = glue("{title1}"),
                                      y = "Percent Averages") + 
    theme(legend.position = "none", axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) + scale_fill_manual(values = legend_colors) +
    coord_cartesian(ylim = c(0, 1))
  plot2 <- tidy_table2 %>% filter(str_detect(Location, tr)) %>% 
    ggplot(aes(x = Location, y = Averages, fill = Category)) + 
    geom_col(position="dodge") + labs(x = glue("{title2}")) + 
    theme(legend.position = "none", axis.title.y = element_blank(), 
          axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    scale_fill_manual(values = legend_colors) +
    coord_cartesian(ylim = c(0, 1))
  plot3 <- tidy_table3 %>% filter(str_detect(Location, tr)) %>% 
    ggplot(aes(x = Location, y = Averages, fill = Category)) + 
    geom_col(position="dodge") + scale_fill_manual(values = legend_colors,
                                                   labels = legend_labels, 
                                                   name = "Plant Category")  +
    labs(x = glue("{title3}")) + theme(axis.title.y = element_blank(), 
                                       axis.text.x = element_blank(),
                                       axis.ticks.x = element_blank()) +
    coord_cartesian(ylim = c(0, 1))
  
  plot4 <- plot1 + plot2 + plot3
  return(plot4)
}


##figures displaying species richness
within_richness_fig <- function(df_east, df_west) {
  inside_df <- species_richness(unite_treatment(df_east, df_west), count = FALSE)
  outside_df <- species_richness(unite_treatment(df_east, df_west, FALSE), count = FALSE) 
  
  title <- which_treatment(df_east)
  legend_colors <- c("F" = "#B2DF8A", "G" = "#1F78B4", "O" = "#A6CEE3", 
                     "W" = "#33A02C")
  legend_labels <- c("F" = "Forb",  "W" = "Woody", "G" = "Grass", "O" = "Other (Sedges/Ferns)")
  
  plot1 <- ggplot(inside_df, aes(x = Key, y = Count, fill = Key)) + 
    geom_col(position="dodge") + scale_fill_manual(values = legend_colors) + 
    labs(title = glue("{title} Species Richness"), x = "Inside") + 
    theme(legend.position = "none", axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) + coord_cartesian(ylim = c(0, 15))
  #+ geom_text(aes(label = Count))
  
  plot2 <- ggplot(outside_df, aes(x = Key, y = Count, fill = Key)) + 
    geom_col(position="dodge") + labs(x = "Outside") + 
    scale_fill_manual(values = legend_colors, labels = legend_labels, 
                  name = "Plant Category") + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 15))
  # + geom_text(aes(label = Count))
  
  plot3 <- plot1 + plot2
  
  return(plot3)
}



btwn_richness_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                              df3_west, inside = TRUE) {
  title1 <- which_treatment(df1_east)
  title2 <- which_treatment(df2_east)
  title3 <- which_treatment(df3_east)
  
  legend_colors <- c("F" = "#B2DF8A", "G" = "#1F78B4", "O" = "#A6CEE3", 
                     "W" = "#33A02C")
  legend_labels <- c("F" = "Forb",  "W" = "Woody", "G" = "Grass", "O" = "Other (Sedges/Ferns)")
  
  if (inside == TRUE){
    inside_df1 <- species_richness(unite_treatment(df1_east, df1_west), count = FALSE)
    inside_df2 <- species_richness(unite_treatment(df2_east, df2_west), count = FALSE)
    inside_df3 <- species_richness(unite_treatment(df3_east, df3_west), count = FALSE)
    plot1 <- ggplot(inside_df1, aes(x = Key, y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(title = "Species Richness - Inside Plots", 
                                        x = glue("{title1}")) + 
      scale_fill_manual(values = legend_colors) + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank()) + 
      coord_cartesian(ylim = c(0, 15))
    plot2 <- ggplot(inside_df2, aes(x = Key, y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(x = glue("{title2}")) + 
      scale_fill_manual(values = legend_colors) + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank(),
                                                    axis.title.y = element_blank()) +
      coord_cartesian(ylim = c(0, 15))
    plot3 <- ggplot(inside_df3, aes(x = Key, y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(x = glue("{title3}")) + 
      scale_fill_manual(values = legend_colors, labels = legend_labels, 
                        name = "Plant Category") + theme(axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank(),
                                                    axis.title.y = element_blank()) + 
      coord_cartesian(ylim = c(0, 15))
  }
  else {
    outside_df1 <- species_richness(unite_treatment(df1_east, df1_west, FALSE), count = FALSE) 
    outside_df2 <- species_richness(unite_treatment(df2_east, df2_west, FALSE), count = FALSE) 
    outside_df3 <- species_richness(unite_treatment(df3_east, df3_west, FALSE), count = FALSE) 
    plot1 <- ggplot(outside_df1, aes(x = Key, y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(title = "Species Richness - Outside Plots", 
                                        x = glue("{title1}")) + 
      scale_fill_manual(values = legend_colors) + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank()) + 
      coord_cartesian(ylim = c(0, 15))
    plot2 <- ggplot(outside_df2, aes(x = Key, y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(x = glue("{title2}")) + 
      scale_fill_manual(values = legend_colors) + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank(),
                                                    axis.title.y = element_blank()) + 
      coord_cartesian(ylim = c(0, 15))
    plot3 <- ggplot(outside_df3, aes(x = Key, y = Count, fill = Key)) + 
      geom_col(position="dodge") + labs(x = glue("{title3}")) + 
      scale_fill_manual(values = legend_colors, labels = legend_labels, 
                        name = "Plant Category") + theme(axis.text.x = element_blank(),
                                                    axis.ticks.x = element_blank(),
                                                    axis.title.y = element_blank()) + 
      coord_cartesian(ylim = c(0, 15))
  }
  plot4 <- plot1 + plot2 + plot3
  return(plot4)
}



##figure of proportion of invasive species for only woodies
woody_invasive_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                               df3_west) {
  title1 <- which_treatment(df1_east)
  title2 <- which_treatment(df2_east)
  title3 <- which_treatment(df3_east)
  
  df1_row_in <- invasive_percents(species_richness(unite_treatment(df1_east, df1_west))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Inside")
  df1_row_out <- invasive_percents(species_richness(unite_treatment(df1_east, df1_west, FALSE))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Outside")
  df1_props <- bind_rows(df1_row_in, df1_row_out)
  
  df2_row_in <- invasive_percents(species_richness(unite_treatment(df2_east, df2_west))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Inside")
  df2_row_out <- invasive_percents(species_richness(unite_treatment(df2_east, df2_west, FALSE))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Outside")
  df2_props <- bind_rows(df2_row_in, df2_row_out)
  
  df3_row_in <- invasive_percents(species_richness(unite_treatment(df3_east, df3_west))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Inside")
  df3_row_out <- invasive_percents(species_richness(unite_treatment(df3_east, df3_west, FALSE))) %>% 
    filter(str_detect(Key, "W")) %>% mutate(Plot = "Outside")
  df3_props <- bind_rows(df3_row_in, df3_row_out)
  
  
  plot1 <- ggplot(df1_props, aes(x = Plot, y = Prop_Invasive, fill = Plot)) + 
    geom_col(position="dodge") + labs(title = "Proportions of Invasive Woody Species", 
                                      x = glue("{title1}"), y = "Proportion Invasive") + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none", 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank()) + 
    coord_cartesian(ylim = c(0, 1))
  plot2 <- ggplot(df2_props, aes(x = Plot, y = Prop_Invasive, fill = Plot)) + 
    geom_col(position="dodge") + labs(x = glue("{title2}")) + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none", 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 1))
  plot3 <- ggplot(df3_props, aes(x = Plot, y = Prop_Invasive, fill = Plot)) + 
    geom_col(position="dodge") + labs(x = glue("{title3}"), fill = "Plot") + 
    scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 1))
  
  
  plot4 <- plot1 + plot2 + plot3
  return(plot4)
}

##avg heights of woody species present in all three treatment groups
common_in_w_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                            df3_west) {
  df1_in <- unite_treatment(df1_east, df1_west)
  df1_in_richness <- species_richness(df1_in, quiet = TRUE)
  df1_with_heights <- avg_inside_heights(df1_in_richness, df1_in)
  
  df2_in <- unite_treatment(df2_east, df2_west)
  df2_in_richness <- species_richness(df2_in, quiet = TRUE)
  df2_with_heights <- avg_inside_heights(df2_in_richness, df2_in)
  
  df3_in <- unite_treatment(df3_east, df3_west)
  df3_in_richness <- species_richness(df3_in, quiet = TRUE)
  df3_with_heights <- avg_inside_heights(df3_in_richness, df3_in)
  
  title1 <- which_treatment(df1_east)
  title2 <- which_treatment(df2_east)
  title3 <- which_treatment(df3_east)
  
  common_spp <- spp_in_common(df1_with_heights, df2_with_heights, df3_with_heights)
  
  df1_common <- df1_with_heights %>% filter(Species %in% common_spp)
  df2_common <- df2_with_heights %>% filter(Species %in% common_spp)
  df3_common <- df3_with_heights %>% filter(Species %in% common_spp)
  
  
  plot1 <- ggplot(df1_common, aes(x = Species, y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + labs(title = "Average Heights of Common Woody Species - Inside Plots", 
                                      x = glue("{title1}"), y = "Average Height (cm)") + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none", 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank()) + 
    coord_cartesian(ylim = c(0, 100))
  plot2 <- ggplot(df2_common, aes(x = Species, y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + labs(x = glue("{title2}")) + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none", 
                                                  axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 100))
  plot3 <- ggplot(df3_common, aes(x = Species, y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + labs(x = glue("{title3}")) + 
    scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 100))
  
  
  plot4 <- plot1 + plot2 + plot3
  return(plot4)
}


##from most to least preferred: woody and forb spp browse comparing all three dfs
outside_brow_fig <- function(df1_east, df1_west, df2_east, df2_west, df3_east, 
                                  df3_west, Woody = TRUE) {
  title1 <- which_treatment(df1_east)
  title2 <- which_treatment(df2_east)
  title3 <- which_treatment(df3_east)
  
  df1_outside  <- unite_treatment(df1_east, df1_west, FALSE)
  df1_out_richness  <- species_richness(df1_outside, quiet = TRUE)
  df1_brow <- percent_browse_outside(df1_outside, df1_out_richness, FALSE)
  df2_outside  <- unite_treatment(df2_east, df2_west, FALSE)
  df2_out_richness  <- species_richness(df2_outside, quiet = TRUE)
  df2_brow <- percent_browse_outside(df2_outside, df2_out_richness, FALSE)
  df3_outside  <- unite_treatment(df3_east, df3_west, FALSE)
  df3_out_richness  <- species_richness(df3_outside, quiet = TRUE)
  df3_brow <- percent_browse_outside(df3_outside, df3_out_richness, FALSE)
  
  
  if (!Woody){
    vegetation <- "F"
    type <- "Forb"
  }
  else {
    vegetation <- "W"
    type <- "Woody"
  }
  
  plot1 <- ggplot((df1_brow %>% filter(Key == vegetation)), aes(x = reorder(Species, -Percent_Browse),
                                                                y = Percent_Browse, fill = Species)) + 
    geom_col(position="dodge") + labs(title = 
                                        glue("Percent Browse of Common {type} Species - Outside Plots"), 
                                      x = glue("{title1}"), y = "Percent Browse") + 
    scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank()) + 
    coord_cartesian(ylim = c(0, 100))
  plot2 <- ggplot((df2_brow %>% filter(Key == vegetation)), aes(x = reorder(Species, -Percent_Browse),
                                                                y = Percent_Browse, fill = Species)) + 
    geom_col(position="dodge") + labs(x = glue("{title2}")) + 
    scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 100))
  plot3 <- ggplot((df3_brow %>% filter(Key == vegetation)), aes(x = reorder(Species, -Percent_Browse), 
                                                                y = Percent_Browse, fill = Species)) + 
    geom_col(position="dodge") + labs(x = glue("{title3}")) + 
    scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_blank(),
                                                  axis.ticks.x = element_blank(),
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 100))
  
  plot4 <- plot1 + plot2 + plot3
  
  return(plot4)
}


##inside, outside browsed, and outside unbrowsed heights per df: all woody spp combined
combined_avg_heights_fig <- function(df_east, df_west){
  title <- which_treatment(df_east)
  
  inside <- unite_treatment(df_east, df_west, TRUE)
  outside <- unite_treatment(df_east, df_west, FALSE)
  inside_richness <- species_richness(inside, quiet = TRUE)
  outside_richness <- species_richness(outside, quiet = TRUE)
  inside_heights <- avg_inside_heights(inside_richness, inside)
  outside_heights <- na.omit(percent_browse_outside(outside, outside_richness, quiet = TRUE))
  
  
  plot1 <- ggplot(inside_heights, aes(x = Species, y = Average_Height, fill = Species)) + 
    geom_col(position="dodge") + labs(title = 
                                        glue("Average Woody Height 
                                             by Transect and Browse - {title}"), 
                                      x = "Inside Transects", y = "Average Height (cm)") + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none") + 
    coord_cartesian(ylim = c(0, 100))
  plot2 <- ggplot((outside_heights %>% filter(Browse == 1)), aes(x = Species,
                                                                 y = AvgHeight, 
                                                                 fill = Species)) + 
    geom_col(position="dodge") + labs(x = "Outside Transects - Browsed") + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none",
                                                  axis.title.y = element_blank()) +
    coord_cartesian(ylim = c(0, 100))
  plot3 <- ggplot((outside_heights %>% filter(Browse == 0)), aes(x = Species, 
                                                                 y = AvgHeight, 
                                                                 fill = Species)) + 
    geom_col(position="dodge") + labs(x = "Outside Transects - Unbrowsed", 
                                      fill = "Species") + 
    scale_fill_brewer(palette = "Paired") + theme(legend.position = "none",
                                                  axis.title.y = element_blank()) + 
    coord_cartesian(ylim = c(0, 100))
  
  plot4 <- plot1 + plot2 + plot3
  return(plot4)
}

