# WRITE THE LIST OF PACKAGES INSTALLED  ----


# get the list of installed packages ----------
installed_pkgs_df <- as.data.frame(installed.packages(), 
                                   stringsAsFactors = FALSE)

# set the file name to write to ---------------
file_name <- paste0(getwd(), "/data/pkg_df_list.csv")

# write the list of packages into a file ------

write.table(x = installed_pkgs_df, 
            append = FALSE, 
            file = file_name, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep = ",")



# INSTALL THE PACKAGES INTO A NEW INSTALLATION ----

# set the file name to read from ---------------
file_name <- paste0(getwd(), "/data/pkg_df_list.csv")

# read the packages list -----------------------
pkg_df_list <- read.csv(file = file_name, 
                        header = TRUE, 
                        stringsAsFactors = FALSE)

# get the packages that were installed by the user -----
pkg_list_vec <- pkg_df_list[is.na(pkg_df_list$Priority), c("Package"), drop = TRUE]


# try installing the list of packages ------------
for (i in seq_along(pkg_list_vec)){
  
  # declare a data frame to hold the installation status ----
  if(i == 1) 
    pkg_install_status_df <- data.frame()
  
  # install the packages ----
  install.packages(pkg_list_vec[i])
  
  # get the list of installed packages to find if it was installed ----
  get_pkg_vec <- as.data.frame(installed.packages(), 
                               stringsAsFactors = FALSE)[, "Package", drop = TRUE] 
  
  # populate the status of installation ----
  pkg_install_status_df <- rbind(pkg_install_status_df, 
                                 data.frame(pkg = pkg_list_vec[i], 
                                            status = ifelse(pkg_list_vec[i] %in% get_pkg_vec, 
                                                            "installed", 
                                                            "not installed")))
  
}

# print the status of the installation ----
pkg_install_status_df
