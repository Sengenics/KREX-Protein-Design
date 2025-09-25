## Setup Instructions

1. Clone the repository
2. Open the R project in RStudio
3. Run `renv::restore()` in the console to install packages
4. Run the app with `shiny::runApp()`


Create a folder in this directory

```
Software Development/Protein Design Automation/Shiny App/Automated Protein Design App
```

pwd
# Git Command line
```
git init
git remote add github git@github.com:Sengenics/KREX-Protein-Design.git
git pull github master
```
Create you own branch
```
git branch <name>_dev
git checkout <name>_dev
git push github <name>_dev
```
Work from this branch, notify shaun when the master needs to be updated

Open Project in Rstudio

First run, to setup the package environment

```
renv::restore()
```

Open global.R amd Run App

```
