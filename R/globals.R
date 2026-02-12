# R/globals.R

# Silence R CMD check NOTES for non-standard evaluation (NSE)
# 'data' is a special symbol used in monitor_replaceValues() filters
utils::globalVariables(c("data"))
