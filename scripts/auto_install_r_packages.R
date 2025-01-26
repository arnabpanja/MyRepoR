# WRITE THE LIST OF PACKAGES INSTALLED  ----
# DO THIS AFTER R IS UPGRADED TO THE NEW VERSION
# OTHERWISE PICK UP THE EXECUTION FROM LINE 23 

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
pkg_list_vec <- sort(pkg_df_list[is.na(pkg_df_list$Priority), 
                                 c("Package"), 
                                 drop = TRUE])



# declare a list with defined list to store the installation statuses

pkg_installed_status_list <- vector(mode = "list", 
                                    length = length(pkg_list_vec))

# try installing the list of packages ------------
for (i in seq_along(pkg_list_vec)){
  
  # declare a data frame to hold the installation status ----
  if(i == 1) 
    pkg_install_status_df <- data.frame()
  
  # send a message to the terminal -----
  cat(paste0("INSTALLING PACKAGE ", pkg_list_vec[i], " .... "))
  
  # install the packages ----
  install.packages(pkg_list_vec[i])
  
  # get the list of installed packages to find if it was installed ----
  get_pkg_vec <- installed.packages()[, "Package", drop = TRUE] 
  
  # populate the status of installation ----
  pkg_installed_status_list[[i]] <- c(pkg = pkg_list_vec[i], 
                                            status = ifelse(pkg_list_vec[i] %in% get_pkg_vec, 
                                                            "installed", 
                                                            "not installed"))
  
  
}

# create the data frame with the list elements ---- 
pkg_install_status_df <- do.call(rbind, pkg_installed_status_list)

# print the status of the installation ----
pkg_install_status_df |> print()

