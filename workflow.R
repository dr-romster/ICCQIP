## ICC QIP Bump plot workflow

# This worklfow presumes that you are using a POSIT.Cloud
# instance of R Studio

# To import all the scripts from the Github repository:

# New project >> New project from Git repository
# URL of git repository = https://github.com/dr-romster/ICCQIP.git
 

# load all the functions from the process script

source('ICCQIP process.R')

# add a folder to the project, called "data"
system('mkdir data')
# upload your UKHSA ICC QIP Excel file to the "data' folder
# use the "Upload" button in the Files window

# create images folder to store plot files
system('mkdir img')


# define some basic parameters specific to your unit and file name:
target_unit <- "NCCU"
filename <- "RGT4-Report-NCCUDec2025.xlsx"
unit_colour = "darkorange"

ukhsa_t1 <- read_ukhsa(filename, sht = "Table1")





