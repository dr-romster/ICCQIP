# generic functions script to process UKHSA CCIQIP spreadsheets and 
# create bump charts

renv::restore()
# check for missing pacakges
required_pkgs <- c("dplyr", "readxl", "ggplot2",
                   "ggbump", # removed from CRAN Dec 2025!
                   "ggrepel",
                   "ggtext",
                   'MetBrewer',
                   "patchwork",
                   "showtext")

missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
missing_pkgs
if(length(missing_pkgs) > 0){ 
  stop(paste("Please install ", 
             missing_pkgs, " before proceeding.\n ", sep = ""))
} else {if(length(missing_pkgs) == 0){
  rm(missing_pkgs, required_pkgs)
} }


library(dplyr)
library(here)
library(readxl)
library(ggplot2)
library(ggbump)
library(MetBrewer) # for Colour Blind-friendly palettes
library(ggtext)
library(ggrepel)
library(showtext)
library(patchwork)


# ---- List of functions -------
#
# 1. read_ukhsa - import and process the ukhsa file and extract key sheets
# 2. t1_extract_unit_data - importe t1 data. returns a table 1 object (df) at local or national level
# 3. t3_extract_unit_data - imports t3 data. returns a table 2 object (df) at local or national level
# 4. combined_tab - combines local and national t1 or t3 tables
# 5. quick_bugs_list - list of common organisms, fall back list.
# 6. t3_long_function - called by combined_tab. Pivots organisms data to long format
#                       identifies top bugs, combines Staph epi, coag-neg Staphs.
# 7. t1_augment - binds Table 1 data from previous quarterly reports to latest one. 
# 8. t3_augment - binds Table 3 data from previous quarterly reports to latest one. 
# 9. icqqip_plot - bump plot. For either Table 1 or Table 3 data
# 10. organism_t3_plot - bump plot for organims-level data. 
# 11. add_df_xmr_ - creates data frames to add control limts for plots from Table 1 data
# 12. quick_plot_ukhsa - quick and dirty look at bump plots. 
#     Produces 3 plots from Table 1 and 2 plots from Table 3. 
#     Non-augmented, no xMr, default organisms. 



#generic read /import function for ukhsa data file
read_ukhsa <- function(xl, sht = "Table1"){
  if (!file.exists(here("data", xl))){
    stop(paste0("Report file not found: ", xl, "/n/n", 
                "Please check file name and location in data subfolder"), call.=FALSE) 
  }
  
  
  if (sht %in% c("Table1", "Table 1", "Rate", "rate")){
    read_xlsx(here("data", xl), sheet = sht, 
              n_max = 10 # data after row 10 is just notes
    ) |> 
      dplyr::rename("metric"= "...1")
    
  } else if (sht %in% c("Table3", "Table 3", "Organism", "organism")) {
    read_xlsx(here("data", xl), sheet = sht, 
              n_max = 26 ) |> 
      dplyr::rename("metric"= "...1") |> 
      mutate(metric = ifelse(is.na(metric), "location", metric))
    
  }
}


t1_extract_unit_data <- function(tab1_obj, local = TRUE , unit_name = "Your Unit"){
  if(local == TRUE){
    metric_unit = "Your Unit"
    unit_name = unit_name
  } else { 
    metric_unit = "Adult CCUs"
    unit_name = "Adult CCUs"
  }
  
  tab1_obj |> 
    dplyr::select(c('metric', 
                    where(~.x[1] ==  metric_unit))
    ) |> 
    dplyr::filter(metric != "Metric") |> 
    rename_with(
      ~ ifelse(startsWith(., "Q"), substr(., 1, 3), .),
      .cols = everything()
    ) |> 
    mutate(across(
      .cols = starts_with("Q"),
      .fns = as.numeric
    )) |> 
    dplyr::filter(metric != "Total number of positive blood cultures*") |> 
    # this row is repeated which causes pivot issues
    mutate(metric = gsub("[§*¥]", "", metric)) |> 
    tidyr::pivot_longer(cols = starts_with("Q"), names_to = "Period"
    ) |>  
    tidyr::pivot_wider(names_from = 'metric', values_from = value) |> 
    mutate(Unit = unit_name)
  
}

t3_extract_unit_data <- function(tab3_obj, local = TRUE , 
                                 unit_name = "Your Unit"){
  if(local == TRUE){
    metric_unit = "Your Unit*"
    unit_name = unit_name
  } else { 
    metric_unit = "Adult CCUs§"
    unit_name = "Adult CCUs"
  }
  tab3_obj |> 
    dplyr::select(c(metric, where(~.x[1] ==  metric_unit))) |> 
    dplyr::select(c(metric, where(~.x[2] == "% of +ve BC**"))) |> 
    
    dplyr::filter(!metric %in% c("location", "Metric")) |> 
    rename_with(
      ~ ifelse(startsWith(., "Q"), substr(., 1, 3), .),
      .cols = everything()
    ) |> 
    
    mutate(across(
      .cols = starts_with("Q"),
      .fns = as.numeric)) |> 
    mutate(metric = gsub("[◊†]", "", metric)) |> 
    tidyr::pivot_longer(cols = starts_with("Q"), names_to = "Period"
    ) |>  
    tidyr::pivot_wider(names_from = 'metric', values_from = value) |> 
    mutate(Unit = unit_name)
  
}  


quick_bugs_list <- c(
  "Skin commensals", "Coag-neg Staph", "Recognised pathogens",
  "Polymicrobial infections", "Klebsiella spp." )
# default list of bugs, can be modified but use names as they appear
# in Table 3


# this creates a vector of tidy quarterly period names for plot labels and table labels
start_q      <- 28
start_date   <- as.Date("2023-04-01")

period_lookup <- tibble(
  Period = paste0("Q", start_q:(start_q + 20)),  # generate as many as needed
  q_date   = seq.Date(start_date, by = "3 months", length.out = 21),
  Tidy_period  = format(q_date, "%b\n%Y")
)

combined_tab <- function(df1, df2,   # local and national dfs.
                         table = "Table1",
                         unit_name = "Your Unit", 
                         top_X= 10){ 
  unit_name = df1$Unit[[1]]
  
  interim_df <- 
    bind_rows(df1, df2) |> 
    mutate(Unit = as.factor(Unit) ) |> 
    mutate(Period = as.factor(Period) ) |> 
    mutate(time = row_number()) |> 
    mutate(Period2 = as.numeric(Period)) |> 
    left_join(period_lookup %>% dplyr::select(-q_date), by = 'Period') |> 
    mutate(Period = as.factor(Period) ) # have to respecify factor after join
#    mutate(Tidy_period = case_when(
#   
#      Period == "Q28" ~ "Apr\n2023",
#      Period == "Q29" ~ "Jul\n2023",
#      Period == "Q30" ~ "Oct\n2023",
#      Period == "Q31" ~ "Jan\n2024",
#      Period == "Q32" ~ "Apr\n2024",
#      Period == "Q33" ~ "Jul\n2024",
#      Period == "Q34" ~ "Oct\n2024",
#      Period == "Q35" ~ "Jan\n2025",
#      Period == "Q36" ~ "Apr\n2025",
#      Period == "Q37" ~ "Jul\n2025",
#      Period == "Q38" ~ "Oct\n2025",
#      Period == "Q39" ~ "Jan\n2026",
#      Period == "Q40" ~ "Apr\n2026",
#      Period == "Q41" ~ "Jul\n2026",
#      Period == "Q42" ~ "Oct\n2026",
#      Period == "Q43" ~ "Jan\n2027",
#      Period == "Q44" ~ "Apr\n2027",
#      Period == "Q45" ~ "Jul\n2027",
#      Period == "Q46" ~ "Oct\n2027",
#      Period == "Q47" ~ "Jan\n2028",
#      Period == "Q48" ~ "Apr\n2028",
#      Period == "Q49" ~ "Jul\n2028",
#      Period == "Q50" ~ "Oct\n2028",
# #  # can be extended, might be able to do this programmatically
# 
#    TRUE ~ Period
  #))
  if(table == "Table1"){
    
    return(interim_df)
    
  } else if (table == "Table3"){
    
    t3_object <- list()
    
    t3_interim <- 
      interim_df |> 
      dplyr::select(-c(`Positive blood cultures`,`Skin commensals which meet the BSI case definition `,`Parasites`) )
    
    t3_object$tbl <- t3_interim # wide table for organism prevalence locally and nationally. 
    
    top_bugs <- 
      t3_interim |> 
      dplyr::filter(Unit == unit_name) |>  # ensure we highlight most common local organisms
      tidyr::pivot_longer(cols = -c("Unit", "time", "Period", "Period2", "Tidy_period"), 
                          names_to = "Pathogen", 
                          values_to = "pct") |> 
      mutate(Pathogen = ifelse(Pathogen == "Staphylococcus, coagulase-negative", 
                               "Coag-neg Staph", Pathogen)) |> 
      group_by(Pathogen) |> 
      summarise(mean_pct=mean(pct)) |> 
      arrange(desc(mean_pct)) |> 
      ungroup() |> 
      dplyr::top_n(top_X, wt = mean_pct)
    
    top_x = top_bugs$Pathogen[1:top_X]
    
    t3_object$top_bugs <- top_bugs
    # top bugs list by mean incidence, priority for local organisms
    
    t3_object$long_local_organisms <- t3_long_function(t3_interim, 
                                                       local = TRUE, 
                                                       unit_name = unit_name, 
                                                       top_x = top_x)
    
    t3_object$long_national_organisms <- t3_long_function(t3_interim, 
                                                          local = FALSE, 
                                                          top_x = top_x)
    return(t3_object)
  }
}


t3_long_function <- function(t3_df, 
                             local = TRUE, 
                             unit_name = "Your Unit",
                             top_x = top_x){
  if(local == TRUE){
    unit_filter <- unit_name
  } else if (local == FALSE){
    unit_filter <- "Adult CCUs"
  }
  
  
  #top_Bugs <- data.frame(Pathogen = c(
  #  "Skin commensals", "Coag-neg Staph", "Recognised pathogens",
  #  "Polymicrobial infections", "Klebsiella spp." ))
  if(length(top_x) > 0){
    top_Bugs <-  data.frame(Pathogen = top_x) 
    
  } else {
    top_Bugs <- data.frame(Pathogen = quick_bugs_list) # default list (NCCU bias!)
  }
  
  t3_df |> 
    dplyr::filter(Unit == unit_filter) |> 
    dplyr::select(-time, -Period2, -Tidy_period, -Unit) |> 
    tidyr::pivot_longer(cols = -Period, names_to = "metric", values_to = "Pct", cols_vary = 'slowest' ) |> 
    mutate(metric = ifelse(metric == "Staphylococcus, coagulase-negative", 
                           "Coag-neg Staph", metric)) |> 
    mutate(metric = as.factor(metric)) |> 
    mutate(selected = ifelse(metric %in% top_Bugs$Pathogen, 1, 0)) |> 
    mutate(Period = as.factor(Period)) |> 
    mutate(Period2 = as.numeric(Period)) |> 
    left_join(period_lookup %>% dplyr::select(-q_date), by = 'Period') |>
    mutate(Period = as.factor(Period)) # re-specify factor after chr join
    # mutate(Tidy_period = case_when(
    # 
    #   Period == "Q28" ~ "Apr\n2023",
    #   Period == "Q29" ~ "Jul\n2023",
    #   Period == "Q30" ~ "Oct\n2023",
    #   Period == "Q31" ~ "Jan\n2024",
    #   Period == "Q32" ~ "Apr\n2024",
    #   Period == "Q33" ~ "Jul\n2024",
    #   Period == "Q34" ~ "Oct\n2024",
    #   Period == "Q35" ~ "Jan\n2025",
    #   Period == "Q36" ~ "Apr\n2025",
    #   Period == "Q37" ~ "Jul\n2025",
    #   Period == "Q38" ~ "Oct\n2025",
    #   Period == "Q39" ~ "Jan\n2026",
    #   Period == "Q40" ~ "Apr\n2026",
    #   Period == "Q41" ~ "Jul\n2026",
    #   Period == "Q42" ~ "Oct\n2026",
    #   Period == "Q43" ~ "Jan\n2027",
    #   Period == "Q44" ~ "Apr\n2027",
    #   Period == "Q45" ~ "Jul\n2027",
    #   Period == "Q46" ~ "Oct\n2027",
    #   Period == "Q47" ~ "Jan\n2028",
    #   Period == "Q48" ~ "Apr\n2028",
    #   Period == "Q49" ~ "Jul\n2028",
    #   Period == "Q50" ~ "Oct\n2028",
    # 
    #   TRUE ~ Period))
}

# augmentiing functions for previous sheets
augment_t1 <- function(recent_df, old_report_file, unit_name= "NCCU"){
  
  df2 <- recent_df
  old_t1 <- read_ukhsa(old_report_file, sht = "Table1")
  combined_old <- combined_tab(t1_extract_unit_data(old_t1, TRUE, unit_name),
                               t1_extract_unit_data(old_t1, FALSE)
  )
  new_rows <- 
    combined_old |> 
    dplyr::filter(!Period %in% df2$Period) |> 
    mutate(Period = as.character(Period))
  
  aug_df <- 
    df2 |> 
    mutate(time = time +1, Period2 = Period2+1) |> 
    mutate(Period = as.character(Period)) |> 
    bind_rows(new_rows) |> 
    dplyr::arrange(Unit, Period) |> 
    mutate(Period = as.factor(Period))
  
  return(aug_df)
}



augment_t3 <- function(recent_t3_object, old_report_file, unit_name= "NCCU"){
  
  df_recent <- recent_t3_object
  #df_recent <- jun_t3_combined # testing
  old_t3 <- read_ukhsa(old_report_file, sht = 'Table3')
  unit_target = unit_name
  
  combined_old_t3 <- combined_tab(t3_extract_unit_data(old_t3, TRUE, unit_target),
                                  t3_extract_unit_data(old_t3, FALSE), top_X = 5,
                                  table = "Table3")
  new_rows_local <- 
    combined_old_t3$long_local_organisms |> 
    dplyr::filter(!Period %in% df_recent$long_local_organisms$Period) |> 
    dplyr::filter(metric %in% df_recent$top_bugs$Pathogen) |> 
    mutate(Period = as.character(Period))
  
  new_rows_national <- 
    combined_old_t3$long_national_organisms |> 
    dplyr::filter(!Period %in% df_recent$long_national_organisms$Period) |> 
    dplyr::filter(metric %in% df_recent$top_bugs$Pathogen) |> 
    mutate(Period = as.character(Period))
  
  aug_df <- list()
  
  aug_df$long_local_organisms <- 
    df_recent$long_local_organisms |> 
    mutate(Period2 = Period2+1) |> 
    mutate(Period = as.character(Period)) |> 
    bind_rows(new_rows_local) |> 
    dplyr::arrange(Period) |> 
    mutate(Period = as.factor(Period))
  
  aug_df$long_national_organisms <- 
    df_recent$long_national_organisms |> 
    mutate( Period2 = Period2+1) |> 
    mutate(Period = as.character(Period)) |> 
    bind_rows(new_rows_national) |> 
    dplyr::arrange(Period) |> 
    mutate(Period = as.factor(Period))
  
  return(aug_df)
}


# plotting functions
# generic geom bump plotting function

icqqip_plot <- function(combined_df, # local and national merged tidy data
                        column = 1, 
                        colour1 = "darkorchid", # national unit colour 
                        colour2 = "darkorange", # local data colour
                        xmr = FALSE             # add xMr lines
){
  
  # column options
  # 1 = Rate of BSI per 1,000 patient days
  # 2 = Rate of positive blood cultures per 1,000 patient days
  # 3 = Rate of positive blood cultures per 1,000 blood culture sets taken
  
  metric_select <- switch(as.character(column),
                          "1" = colnames(combined_df)[8],
                          "2" = colnames(combined_df)[4],
                          "3" = colnames(combined_df)[6],
                          stop("Invalid column value") )
  
  
  bump_size = 1.5
  bump_smooth = 6
  point_size = 3
  # sub_lab = "Q29-Q33: Q1-Q4 2023/4\nQ33-Q36: Q1-Q4 2024/5"
  
  df2 <- combined_df
  
  bump_plot <-
    combined_df %>%
    ggplot(.,   aes(x = Period2, 
                    y = .data[[metric_select]], 
                    colour = Unit)) +
    geom_point(size = point_size) + 
    #geom_line(aes())+
    geom_bump(size = bump_size, smooth = bump_smooth)+
    scale_x_continuous(breaks =  df2$Period2, 
                       labels = df2$Tidy_period, 
                       name = "") +
    scale_colour_manual(values = c(colour1, colour2),
                        #c(colour1, colour2), 
                        name = "", 
                        labels = c("UK Adult ICUs", as.character(levels(df2$Unit)[2]) ) 
    )+
    labs(title = paste(metric_select, sep = "" ), 
         # subtitle = sub_lab, 
         caption = "@dr_romster | rs307@cam.ac.uk")+
    theme_minimal()+
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 14, hjust = .5, face="bold"),
          plot.subtitle = element_text(size = 10, hjust = .5)
    ) 
  #}
  
  max_y_scale <- ifelse(max(combined_df[[8]]+0.5) < 8,  8, 
                        max(combined_df[[8]]+0.5 ) )
  # try to keep y-axis scale consistent/harmonised between units
  
  if ( column == 1){
    bump_plot <- bump_plot +
      scale_y_continuous(name = "", 
                         #limits = c(0, length(unique(df2$Tidy_period))),
                         limits = c(0, max_y_scale)
      ) 
  }  else if (column %in% c(2,3) ) {
    bump_plot <- bump_plot +
      scale_y_continuous(name = "")
  }
  
  if (xmr == TRUE){
    cl_df <- add_xmr_df(combined_df)
    
    cl_df_col <- cl_df[[as.numeric(column)]]
    bump_plot <- bump_plot +
      geom_hline(data = cl_df_col,
                 aes(yintercept = ucl_unit, colour = Unit),
                 linetype = 'dotted') +
      geom_hline(data = cl_df_col,
                 aes( yintercept = lcl_unit, colour = Unit),
                 linetype = 'dotted') +
      geom_hline(data = cl_df_col,
                 aes( yintercept = xbar_unit, colour = Unit),
                 linetype = 'dashed') +
      labs(subtitle = "XmR chart. Dashed lines are mean (control limit) values.
           Dotted lines are upper and lower control limits.")
  } else { bump_plot <- bump_plot}
  
  return(bump_plot)
}





organism_t3_plot <- function(t3_object, 
                             local = TRUE, 
                             unit_name = "NCCU",
                             quartiles = 7){
  # if extended data then increase the quartiles to the number of periods
  
  
  if(local == TRUE){
    organism_df <- t3_object$long_local_organisms
  } else if (local == FALSE) {
    organism_df <- t3_object$long_national_organisms
    unit_name <- "Adult CCUs"
  }
  
  # data frame to help with tidy plotting, axes and line segments
  df2 <- data.frame(y = seq(0,80,20), 
                    yend = seq(0,80,20), 
                    x = rep(0, 5),
                    xend = rep(quartiles, 5), 
                    group = rep(NA, 5) ) 
  
  ggplot(data = organism_df, # |> dplyr::filter(selected == 1),
         aes(x = Period2, y = Pct, group = metric))+
    
    geom_segment(data= df2,  
                 aes(x=1, y=y, xend = quartiles, yend = yend, group = group), 
                 colour ="gray95", alpha = 0.6, size = 0.8, inherit.aes = FALSE)+
    
    geom_bump(smooth = 6, colour = "gray90")+
    geom_bump(data = organism_df |> dplyr::filter(selected == 1),
              aes(colour = metric), smooth = 6)+
    
    geom_point(colour = "white", size= 4)+
    
    geom_point(colour = "gray90", size= 2)+
    
    geom_point(data = organism_df |> # geom_points for top_n organisms
                 dplyr::filter(selected == 1),
               aes(colour = metric), size= 2) +
    
    geom_text_repel(data = organism_df |> 
                      dplyr::filter(selected == 1, 
                                    Period == max(levels(organism_df$Period) ) ),
                    
                    aes(label = metric, 
                        y = Pct, 
                        colour = metric), x = quartiles+0.2,
                    direction = "y",           # Repel only along x-axis
                    nudge_x = 0.01,             # Push labels to the right
                    hjust = 0,                 # Left-align text
                    segment.size = 0.2,        # Optional: thinner connector line
                    min.segment.length = 1,    # Optional: always show segment
                    box.padding = 0.3,         # Space around label
                    point.padding = 0.01,       # Space around point
                    max.overlaps = Inf,
                    seed = 42,
                    label.padding = 0.05, 
                    label.r = unit(0, "lines")
    ) +
    
    #scale_x_continuous(limits = c(0.8, 9.5), expand=c(0,0.1), 
    scale_x_continuous(limits = c(0.8, quartiles + 2.5), expand=c(0,0.1), 
                       breaks = unique(organism_df$Period2), 
                       labels = unique(organism_df$Tidy_period)) +
    
    scale_y_continuous(limits = c(0, 95))+
    
    labs(x = NULL, y = "%",
         title = "ICCQIP: Percentages of species in positive blood cultures",
         subtitle = paste( unit_name,"Cohort", sep = " "),
         caption = "Relative abundances of different bacterial species in positive blood culutres.\nLow abundance species omitted.\n @dr_romster | rs307@cam.ac.uk") +
    theme_minimal(base_size = 12) +
    theme(legend.position = 'none', 
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(size = 14, hjust = .5, face="bold"),
          plot.subtitle = element_text(size = 10, hjust = .5) ) 
}



add_xmr_df <- function(combined_t1_obj){
  
  # three columns to target
  column_list <- c("Rate of BSI per 1,000 patient days", 
                   "Rate of positive blood cultures per 1,000 patient days",
                   "Rate of positive blood cultures per 1,000 blood culture sets taken")
  
  # group_by unit, 
  # calculate mr, x_bar, sd_est (= mr_bar/1.128), upper and lower CLs
  
  cl_df <- list()
  
  for(i in seq_along(column_list)){
    
    col <- column_list[i]
    
    cl_df[[i]] <- 
      combined_t1_obj |> 
      dplyr::select(all_of(c(col, "Unit"))) |> 
      group_by(Unit) |> 
      mutate(mr = c(NA, abs ( diff (.data[[col]] ))), 
             xbar_unit = mean(.data[[col]], na.rm = TRUE ),
             mr_bar_unit = mean(mr, na.rm = TRUE), 
             sd_est_unit =  mr_bar_unit/1.128 , 
             ucl_unit = xbar_unit + 3*sd_est_unit, 
             lcl_unit = xbar_unit - 3*sd_est_unit )  |> 
      mutate(lcl_unit = ifelse(lcl_unit < 0, 0.2, lcl_unit),
             ucl_unit = ifelse(ucl_unit > 100, 100, ucl_unit) )  |> 
      # in case lcl < 0%, or ucl > 100%
      ungroup()
    
  }
  
  return(cl_df)
}


quick_plot_ukhsa <- function(xl, name = "Your Unit"){
  # xl = path to and file name of Excel sheet from UKSHA
  # name = what your local unit is called, defaults to "Your Unit"
  
  # this will create R objects which are ggplot2 run/trend charts
  # BSI_plot_1, BSI_plot_3 & BSI_plot_3 for BSI trend data 
  # organism_local and organism_national for pathogen trend data
  
  if (!is.character(name)) {
    # common cases: name = trends, name = 123, name = factor("x")
    name <- as.character(name)
  }
  
  if (name == "Your Unit"){ 
    message("Unit name not provided, default name used")
  } else {  
    message("Unit name is:", name) 
  }
  
  rt1 <- read_ukhsa(xl, sht = "Table1")
  t1_Local <- t1_extract_unit_data(tab1_obj = rt1, local = TRUE, unit_name = name )
  t1_national <- t1_extract_unit_data(tab1_obj = rt1, local = FALSE )
  
  local_national_t1 <- combined_tab(t1_Local, t1_national, table = "Table1")
  
  bsi_plots <- list()
  
  bsi_plots[[1]] <- icqqip_plot(local_national_t1, column = 1, xmr = FALSE )
  bsi_plots[[2]] <- icqqip_plot(local_national_t1, column = 2, xmr = FALSE )
  bsi_plots[[3]] <- icqqip_plot(local_national_t1, column = 3, xmr = FALSE )
  
  
  bsi_obj_names <- c("BSI_plot_1", "BSI_plot_2", "BSI_plot_3")
  
  for ( i in seq_along(bsi_plots)){
    assign(bsi_obj_names[i], bsi_plots[[i]], envir = parent.frame())  
  }
  
  
  
  rt3 <- read_ukhsa(xl, sht = "Table3")
  
  local_national_t3 <- combined_tab(t3_extract_unit_data(tab3_obj = rt3, local = TRUE, unit_name = name ), 
                                    t3_extract_unit_data(tab3_obj = rt3, local = FALSE), table = "Table3", top_X = 5 )
  
  organism_plot <- list()
  
  organism_plot[[1]] <- organism_t3_plot(local_national_t3, 
                                         local = TRUE, 
                                         unit_name = name, 
                                         quartiles = 7)
  
  organism_plot[[2]] <- organism_t3_plot(local_national_t3, 
                                         local = FALSE, 
                                         quartiles = 7)
  
  org_obj_names <- c("organism_local", "organism_national")
  
  for ( i in seq_along(org_obj_names)){
    assign(org_obj_names[i], organism_plot[[i]], envir = parent.frame())  
  }
  
}
