# dependencies.R

# List of required packages
required_packages <- c("shiny", "quantmod", "xts", "dygraphs", "dplyr", "shinydashboard")

# Function to check if a package is installed
is_installed <- function(pkg){
  is.element(pkg, installed.packages()[,1])
}

# Install missing packages
for (pkg in required_packages){
  if (!is_installed(pkg)){
    cat(paste("Installing package:", pkg, "\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

cat("All required packages are installed.\n")
