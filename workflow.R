## ICC QIP Bump plot workflow

# This workflow presumes that you are using a POSIT.Cloud
# instance of R-Studio

# To import all the scripts from the Github repository:

# New project >> New project from Git repository
# URL of git repository = https://github.com/dr-romster/ICCQIP.git
 
# Do this first to install all the correct package versions
# This step takes about 5 minutes. 
install.packages("renv")
renv::activate()
renv::restore(prompt = FALSE)


# load all the functions from the process script

source('ICCQIP process.R')


# you will need to add a sub-folder to the project, called "data".
system('mkdir data')

 
# The UKHSA Excel file should be copied here or else the script will not 
# know where it is. 
# Create images folder to store plot files.
# system('mkdir img')


# All users (local or Posit.cloud):
# Upload your UKHSA ICC QIP Excel file to the "data' folder.
# Use the "Upload" button in the Files window.



# Define some basic parameters specific to your unit and file name:

#### please change these as appropriate BEFORE proceeding

unit_name <- "Your unit"
filename <- "XXXX-Report.xlsx"

## These changes should remain within the quotation marks " "


# specify colour of your unit's line in the bump plots

unit_colour = "darkorange"

# if you don't know how to change this, leave it alone!



# 1. read and import data from UKHSA report
ukhsa_table_1 <- read_ukhsa(filename, sht = "Table1")

# 2. organise data at local and national level into a single data frame

local_national_combined_table_1 <- combined_tab(
  t1_extract_unit_data(tab1_obj = ukhsa_table_1, 
                       local = TRUE, 
                       unit_name = unit_name), # organise local data
  t1_extract_unit_data(tab1_obj = ukhsa_table_1, 
                       local = FALSE), # organise national data
  table = "Table1"
  )

# this object allows for plotting of local and national data from 
# table 1

# 3. Bump plot of BSI rates per patient days, local vs national
plot_1 <- 
  icqqip_plot(combined_df = 
                local_national_combined_table_1, 
              column = 1) +
  labs(
    caption = glue::glue('Fig 1. Rates of BSI on {target_unit} compared with the national averages'))

plot_1

# Bump plot of BSI rates per 1000 patients days.

plot_2 <- icqqip_plot(local_national_combined_table_1, column = 2) +
  labs(
    caption= glue::glue('Fig 2. Rates of positive blood cultures on {target_unit} compared with the national averages'))

plot_2


# Bump plot of BSI rates 1000 blood cultures taken. 

plot_3 <- icqqip_plot(local_national_combined_table_1, column = 3) +
  labs(
    caption= glue::glue('Fig 3. Rates of positive blood cultures adjusted for sampling rates on {target_unit} compared with the national averages'))

plot_3

-----

# 4. Organism trend plot using data from Report Sheet / Table 3

ukhsa_table_3 <- read_ukhsa(filename, sht = "Table3")

t3_combined <- combined_tab(
  t3_extract_unit_data(tab3_obj = ukhsa_table_3, 
                       local = TRUE, 
                       unit_name = unit_name), # local
  t3_extract_unit_data(tab3_obj = ukhsa_table_3, 
                       local = FALSE), #national data
  table = "Table3", 
  top_X = 6 # extracts top 6 organisms, can be adjusted
  )


# 5. organisms abundance trend plot - local 

plot_table_3_local <- organism_t3_plot(t3_combined, 
                                       local = TRUE, 
                                       unit_name = unit_name, 
                                       quartiles = 7 #number of quarter to plot, default = 7
                                       )
plot_table_3_local

# organisms abundance trend plot - national

plot_table_3_national <- organism_t3_plot(t3_combined, 
                                          local = FALSE, 
                                       quartiles = 7) #number of quarter to plot, default = 7

plot_table_3_national


# ---- 6. Save plots

ggsave("img/bsi_plot1.png", 
       plot_1, 
       width = 8, height = 6, dpi = 300, 
       bg="white")


ggsave("img/bsi_plot2.png", 
       plot_2, 
       width = 8, height = 6, dpi = 300, 
       bg="white")


ggsave("img/bsi_plot3.png", 
       plot_3, 
       width = 8, height = 6, dpi = 300, 
       bg="white")


ggsave("img/bsi_organism_plot1.png", 
       plot_table_3_local, 
       width = 8, height = 6, dpi = 300, 
       bg="white")


ggsave("img/bsi_organism_plot2.png", 
       plot_table_3_national, 
       width = 8, height = 6, dpi = 300, 
       bg="white")
