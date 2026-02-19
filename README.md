# ICCQIP

ICU QIP for blood stream infections (BSIs)


### Disclaimer

This project is released under the MIT License and is provided “as is”, without warranty or liability. It reflects the author’s personal work and does not imply endorsement by any affiliated organisations or the UK Health Security Agency. 

## Getting started

> [!TIP]
> Create a new project in [Posit.cloud](https://posit.cloud/)

This is free for simple accounts that require minimal resources and avoids making changes to your local R installation and its libraries. 

**Create a New Project**

Select from the dropdown list: **New Project from Git Repository**

![git import](https://github.com/dr-romster/ICCQIP/blob/d4b7504c17bef087014bacae21ffec98a5ef831a/img/new_project.png)

  
Paste this link into the url box: 

`https://github.com/dr-romster/ICCQIP.git`


## R Studio / Posit

If using R Studio / Posit on your own computer. 

Creat a New Project >> Version control >> Git 

![rstudio import](https://github.com/dr-romster/ICCQIP/blob/bd3f6b0131f17527b779ade918a013e08ea40a11/img/rstudio_git.png)

Repository URL:

`https://github.com/dr-romster/ICCQIP.git`

The other fields are to tell R Studio which local folder on your computer
you want to use for the project. 

----


> \[!IMPORTANT]
> The Excel file from UKHSA needs to be copied / uploaded to the `/data` sub-directory
> or else the script wull not know where to find it. 

If using Posit Cloud, use the Upload button in the Files window which is located in the lower right pane.

----

## Contents


This R Project contains 3 scripts

[1] workflow.R

- This gives a step-by-step approach to creating the plots from the UKHSA spreadsheet. 
- Start here if you are a beginner / novice. 

[2] ICCQIP process.R

- This contains the core functions that import, process and plot the data. 

[3] Packages and initialisation.R

- If you wish to install the libraries manually this contains details on how to do this. 

- Not essential, only for people who want to look "under the bonnet" and tinker for themselves.

-----

