if(system.file(package="rprojroot")!="")
  Sys.setenv(R_CONFIG_FILE = rprojroot::find_package_root_file("tests/config-tessireport.yml"))

user_profile <- file.path(Sys.getenv("R_USER"),".Rprofile")
if(file.exists(user_profile))
  source(user_profile)
