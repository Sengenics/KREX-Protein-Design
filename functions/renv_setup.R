# Run this once in your project directory
install.packages("renv")
renv::init()

# This will:
# 1. Create renv.lock file (tracks exact package versions)
# 2. Create renv/ directory (local package library)
# 3. Modify .Rprofile to auto-activate renv

# After making changes to packages:
renv::snapshot()  # Updates renv.lock

# When someone clones your repo:
renv::restore()   # Installs exact package versions from renv.lock
