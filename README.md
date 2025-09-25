
Create a folder in this directory

```
Software Development/Protein Design Automation/Shiny App/Automated Protein Design App
mkdir <name>_dev
cd <name>_dev
```

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
renv::init()
```
Select : 
1: Restore the project from the lockfile.


Open global.R amd Run App

```
