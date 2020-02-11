source("functions.R")

test_set <- data_import()

d_select <- 5

generate_plots(d_select,strsplit(test_set[[d_select]]$name,"[.]")[[1]][1],test_set)