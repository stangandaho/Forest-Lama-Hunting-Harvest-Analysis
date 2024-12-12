# package setting
pkg_to_install <- c("readxl", 
                    "dplyr", 
                    "ggplot2", 
                    "showtext", 
                    "tidyr"
                    )

installed_package <- rownames(installed.packages())

for (pkg in pkg_to_install) {
  if (! pkg %in% installed_package) {
    install.packages(pkg)
  }
}

rm(installed_package, pkg, pkg_to_install)

# plot design
color_plt <- c("#A68F97", "#79717A", "#4B4952", "#1F2024", "#004F4D")
library(showtext)

font_add("msb", "fonts/Montserrat-SemiBold.ttf")
font_add("mm", "fonts/Montserrat-Medium.ttf")

showtext_auto()

