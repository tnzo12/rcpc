# minimum requriements ===================================================
#
# for Windows: Rtools
# for MacOS: build tools
# Linux doesn't need to follow such instructions (have internalized buildtools)
#
# **in this case, we will use 'homebrew' in installing build tools for MacOS
#
# 1. Process
#
#   1) turn on 'terminal' (not R related)
#
#   2) put following text in the terminal:
#      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
#
#   3) put following text in the terminal in sequence by lines (each update process may take some time please wait in patience):
#      brew update && upgrade
#      brew info gcc
#      brew install gcc
#      brew install llvm
#      brew clean up



install.packges("devtools")
install.packges("remotes")
install.packages("shiny")
install.packages("rsconnect")
install.packages('knitr')
install.packages('networkD3')
install.packages('plotly')
install.packges("n1qn1")
install.packges("PreciseSums")
install.packges("shinyWidgets")

devtools::install_github("tidyverse/tidyverse") # includes: dplyr, ggplot2, tidyr...
remotes::install_github("daattali/shinyjs")
devtools::install_github("jrowen/rhandsontable")
devtools::install_github("renkun-ken/formattable")
devtools::install_github("haozhu233/kableExtra")

remotes::install_github("rstudio/htmltools") # installation require for bs4Dash package
remotes::install_github("rstudio/thematic") # optional, not being used in bs4Dash currently
devtools::install_github("RinteRface/bs4Dash")
