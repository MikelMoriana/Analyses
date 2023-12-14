# Libraries and data files----

# First we get the data from OSF, this does not go into the final R script
library(osfr)
library(dataDownloader)
# osf_auth(token="...") Write in your own token
get_file(
  node = "zhk3m",
  file = "INCLINE_community_subplot_fixed.csv",
  remote_path = "Community",
  path ="data")

# If not already installed, we install the packages we need
package_vec <-  c("tidyverse", "vegan", "ellipse", "lme4")
sapply(package_vec, install_load_package)

# File with community data
community_all <- read_csv("data/INCLINE_community_subplot_fixed.csv")
community <- filter(community_all, !treatment == "R") # We are not interested in the removals plots for this study